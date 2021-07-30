package scraml

import cats.effect.unsafe.implicits.global
import sbt._
import sbt.Keys._

object ScramlPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val ramlFile = settingKey[Option[File]]("ramlFile")
    val packageName = settingKey[String]("packageName")
  }

  import autoImport._
  override lazy val globalSettings: Seq[Setting[_]] = Seq(
    ramlFile := None,
    packageName := "scraml"
  )

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    Compile / sourceGenerators += Def.task {
      val targetDir: File = (Compile / sourceManaged).value

      ramlFile.value.map { file =>
        val params = ModelGenParams(file, targetDir, packageName.value)

        val generated = ModelGenRunner.run(DefaultModelGen)(params).unsafeRunSync()
        val s = streams.value
        s.log.info(s"generated API model for $file in $targetDir")
        s.log.debug(generated.toString)
        generated.files.map(_.file)
      }.getOrElse(List.empty)

    }.taskValue
  )
}