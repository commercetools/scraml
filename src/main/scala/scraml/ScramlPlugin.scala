package scraml

import cats.effect.unsafe.implicits.global
import sbt._
import sbt.Keys._

object ScramlPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val ramlFile = settingKey[File]("ramlFile")
    val packageName = settingKey[String]("packageName")
  }

  import autoImport._
  override lazy val globalSettings: Seq[Setting[_]] = Seq(
    packageName := "scraml"
  )

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    ramlFile := new File("api.raml"),
    Compile / sourceGenerators += Def.task {
      val targetDir: File = (Compile / sourceManaged).value
      val params = ModelGenParams(ramlFile.value, targetDir, packageName.value)

      val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()
      val s = streams.value
      s.log.info(s"generated $generated")
      generated.files.map(_.file)
    }.taskValue
  )
}