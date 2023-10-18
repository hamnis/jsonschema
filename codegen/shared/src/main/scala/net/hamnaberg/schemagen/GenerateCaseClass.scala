/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schemagen

import net.hamnaberg.schemagen.internal.*
import cats.data.Ior
import cats.syntax.all.*
import sttp.apispec.*

import scala.collection.immutable.ListMap
import scala.meta._
import scala.meta.dialects.Scala3

case class CaseClass(cls: Defn.Class, companion: Defn.Object, imports: List[Importer]) {
  def asSyntax(pkg: Option[String], dialect: Dialect = Scala3): String = {
    val asPkg: Source = pkg
      .map { p =>
        val arr = p.split("\\.").toList
        val packageTerm = arr.tail.foldLeft[Term.Ref](Term.Name(arr.head)) { case (ref, term) =>
          Term.Select(ref, Term.Name(term))
        }

        Source(List(Pkg(packageTerm, imports.map(i => Import(List(i))) ++ List(cls, companion))))
      }
      .getOrElse(
        Source(imports.map(i => Import(List(i))) ++ List(cls, companion))
      )
    scala.meta.internal.prettyprinters.TreeSyntax.apply[Source](dialect)(asPkg).toString()
  }

}

object GenerateCaseClass {
  type Err[A] = Either[String, A]

  def generate(
      schema: Schema,
      name: String,
      imports: List[ImportableType],
      formats: Map[String, ImportableType]): Err[CaseClass] = {
    val properties = propsFrom(schema)
    mapTypes(Type.Name(name), schema, properties, formats).map { typed =>
      val importMap = (imports ++ formats.values.toList.filterNot(t => ImportableType.never.contains(t))).distinct
        .map(f => (f.typeName: Type) -> f.importer)
        .toMap
      gen(name, typed, importMap, formats)
    }
  }

  private def propsFrom(schema: Schema) = schema.properties.collect { case (k, v) => PropertyName(k) -> v }

  private def mapTypes(
      name: Type.Name,
      objectSchema: Schema,
      properties: ListMap[PropertyName, SchemaLike],
      formats: Map[String, ImportableType]): Err[ListMap[PropertyName, TypedSchema]] = {
    val terms = properties.toList.traverse[Err, (PropertyName, TypedSchema)] {
      case (k, AnySchema.Nothing) =>
        Left(s"$k is disallowed to be represented in JSON, We cant really do anything here")
      case (k, AnySchema.Anything) =>
        Right(k -> TypedSchema(AnySchema.Anything, k.field, ImportableType.Json.typeName))
      case (k, v: Schema) =>
        val isRequired = objectSchema.required.contains(k.name)
        v.`type`
          .map(t => getMetaType(Type.Name(name.value), k, v, t, formats))
          .getOrElse(Right(ImportableType.Json.typeName))
          .map { t =>
            val realType: Type =
              if (isRequired) t
              else {
                t match {
                  case at @ IsOption(_) => at
                  case _ =>
                    Type.Apply(ImportableType.Option.typeName, Type.ArgClause(List(t)))
                }
              }
            k -> TypedSchema(v, k.field, realType)
          }
    }
    terms.map(l => ListMap(l: _*))
  }

  private def gen(
      name: String,
      properties: ListMap[PropertyName, TypedSchema],
      imports: Map[Type, Importer],
      formats: Map[String, ImportableType]): CaseClass = {
    val objectName = Term.Name(name)
    val typeName = Type.Name(name)

    def numInstance(typedSchema: TypedSchema) = {
      def bound(exclusive: Boolean, value: Option[Term]) =
        value.map(v => if (exclusive) q"Bound.Exclusive($v)" else q"Bound.Inclusive($v)")

      def bounds(b1: Option[Term.Apply], b2: Option[Term.Apply]) =
        Ior
          .fromOptions(b1, b2)
          .map(
            _.fold(
              a => q"Bounds.min($a)",
              b => q"Bounds.min($b)",
              (a, b) => q"Bounds.both($a, $b)"
            )
          )

      typedSchema.schema match {
        case schema: Schema =>
          val min = schema.minimum
          val max = schema.maximum
          val excludeMin = schema.exclusiveMinimum.getOrElse(false)
          val excludeMax = schema.exclusiveMinimum.getOrElse(false)
          typedSchema.typ match {
            case Type.Name("Double") =>
              val b1 = bound(excludeMin, min.map(_.toDouble).map(i => Lit.Double(i)))
              val b2 = bound(excludeMax, max.map(_.toDouble).map(i => Lit.Double(i)))
              bounds(b1, b2).map(bounds => q"Schema.boundedDouble($bounds)")
            case Type.Name("Float") =>
              val b1 = bound(excludeMin, min.map(_.toFloat).map(i => Lit.Float(i)))
              val b2 = bound(excludeMax, max.map(_.toFloat).map(i => Lit.Float(i)))
              bounds(b1, b2).map(bounds => q"Schema.boundedFloat($bounds)")
            case Type.Name("Int") =>
              val b1 = bound(excludeMin, min.map(_.toInt).map(i => Lit.Int(i)))
              val b2 = bound(excludeMax, max.map(_.toInt).map(i => Lit.Int(i)))
              bounds(b1, b2).map(bounds => q"Schema.boundedInt($bounds)")
            case Type.Name("Long") =>
              val b1 = bound(excludeMin, min.map(_.toLong).map(i => Lit.Long(i)))
              val b2 = bound(excludeMax, max.map(_.toLong).map(i => Lit.Long(i)))
              bounds(b1, b2).map(bounds => q"Schema.boundedLong($bounds)")
            case Type.Name("BigInt") =>
              val b1 = bound(
                excludeMin,
                min.map(_.toLong).map { i =>
                  val lit = Lit.Long(i)
                  q"BigInt($lit)"
                })
              val b2 = bound(
                excludeMax,
                max.map(_.toLong).map { i =>
                  val lit = Lit.Long(i)
                  q"BigInt($lit)"
                })
              bounds(b1, b2).map(bounds => q"Schema.boundedBigInt($bounds)")
            case _ =>
              None
          }
        case _ => None
      }
    }

    def listInstance(typedSchema: TypedSchema) =
      typedSchema.schema match {
        case schema: Schema =>
          typedSchema.typ match {
            case t: Type.Apply if t.tpe == Type.Name("List") =>
              val elemType = t.argClause.values.head
              val min = schema.minItems.map(i => q"Some($i)")
              val max = schema.maxItems.map(i => q"Some($i)")
              Ior
                .fromOptions(min, max)
                .map(
                  _.fold(
                    m => q"Schema[${elemType}].toList(min = $m)",
                    m => q"Schema[${elemType}].toList(max = $m)",
                    (m, m2) => q"Schema[${elemType}].toList(min = $m, max = $m2)"
                  )
                )
            case _ =>
              None
          }
        case _ => None
      }

    val fields = properties.map { case (key, typedSchema) =>
      numInstance(typedSchema).orElse(listInstance(typedSchema)) match {
        case Some(term) =>
          q"field(${Lit.String(key.name)}, _.${Term.Name(key.field)})($term)"
        case None =>
          q"field(${Lit.String(key.name)}, _.${Term.Name(key.field)})"
      }
    }.toList

    val newImports = properties.values.flatMap(ts => imports.get(ts.typ).toList).toList

    val params = properties.values.map(_.param).toList
    val cc =
      q"final case class $typeName(..$params)"

    val subobjects = properties.values
      .flatMap { cc =>
        val name = cc.getEmbeddedTypeName(objectName)
        name.map { tn =>
          tn -> cc.schema.asInstanceOf[Schema]
        }
      }
      .toList
      .flatMap { case (tn, schema) =>
        val props = propsFrom(schema)
        val embedded = mapTypes(tn, schema, props, formats)
        embedded.map(em => gen(tn.value, em, imports, formats)).toList
      }

    val metaSubObjects = subobjects.flatMap(cc => List(cc.cls, cc.companion))
    val objectImports = subobjects.flatMap(cc => cc.imports)

    val instance =
      q"""implicit val schema: Schema[${Type.Name(name)}] =
          Schema.record{ field =>
            (..$fields).mapN(apply)
          }
          """
    CaseClass(
      cc,
      q"""object $objectName {
       $instance
       ..$metaSubObjects
     }""",
      (ImportableType.alwaysImport.map(_.importer) ++ newImports ++ objectImports).distinct.filterNot(t =>
        ImportableType.neverImport(t))
    )
  }

  private def getMetaType(
      outerTypeName: Type.Name,
      name: PropertyName,
      schema: Schema,
      typ: SchemaType,
      formats: Map[String, ImportableType]): Either[String, Type] = {
    val format = schema.format.getOrElse("").toLowerCase

    typ match {
      case ArraySchemaType(value) if value.size == 2 && value.contains(SchemaType.Null) =>
        val other = value.find(_ != SchemaType.Null).toRight("No schema type found but null")
        other
          .flatMap(getMetaType(outerTypeName, name, schema, _, formats))
          .map(t => Type.Apply(Type.Name("Option"), Type.ArgClause(List(t))))
      case ArraySchemaType(List(inner)) if inner != SchemaType.Null =>
        getMetaType(outerTypeName, name, schema, inner, formats)
      case ArraySchemaType(_) => Left(s"Unsupported schema, $schema")
      case SchemaType.Object =>
        Right(Type.Select(Term.Name(outerTypeName.value), Type.Name(name.field.toLowerCase.capitalize)))
      case SchemaType.Array =>
        val item = schema.items.getOrElse(AnySchema.Anything)
        val itemType = item match {
          case AnySchema.Anything => Right(ImportableType.Json.typeName)
          case AnySchema.Nothing => Left("Unsupported type Nothing")
          case s: Schema =>
            s.`type`.toRight("No type defined").flatMap(t => getMetaType(outerTypeName, name, s, t, formats))
        }
        itemType.map(it => Type.Apply(Type.Name("List"), Type.ArgClause(List(it))))
      case SchemaType.Null => Right(Type.Name("Null"))
      case SchemaType.String =>
        Right(formats.getOrElse(format, ImportableType.String).typeName)
      case SchemaType.Number =>
        Right(formats.getOrElse(format, ImportableType.Float).typeName)
      case SchemaType.Boolean => Right(Type.Name("Boolean"))
      case SchemaType.Integer =>
        Right(formats.getOrElse(format, ImportableType.BigInt).typeName)
    }
  }
}
