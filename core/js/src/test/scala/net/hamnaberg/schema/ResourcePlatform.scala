package net.hamnaberg.schema

import io.circe._

trait ResourcePlatform {
  def rscPath(path: String): String = "core/shared/src/test/resources" + path

  def rsc(path: String): String = {
    import scala.scalajs.js.Dynamic.{global => g}
    val fs = g.require("fs")

    def readFile(name: String): String =
      fs.readFileSync(name).toString

    readFile(rscPath(path))
  }

  def readJson(path: String): Either[Error, Json] = {
    val string = rsc(path)
    parser.decode[Json](string)
  }
}
