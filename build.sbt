import Path.FileMap
import java.nio.file.{ Files, Paths }
import java.nio.file.attribute.PosixFilePermissions

lazy val commonSettings = Seq(
  organization := "org.sellmerfud",
  version      := "1.0",
  scalaVersion := "2.13.10"
)

lazy val stage         = taskKey[Unit]("Create distribution zip file")
lazy val sourceScripts = settingKey[File]("Other source file included in the package")


lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name        := "sv",
    description := "Subversion utilities",
    assembly / mainClass := Some("svutil.Main"),
    assembly / assemblyJarName := "sv.jar",
    scalacOptions       ++= Seq( "-deprecation", "-unchecked", "-feature" ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
      "com.typesafe"           %  "config"    % "1.4.2",
      "org.sellmerfud"         %% "optparse"  % "2.3"
    ),
    sourceScripts := sourceDirectory.value / "scripts",
    // (Compile / mainClass) := None,
    Compile / resourceGenerators += Def.task {
      val versFile = (Compile / resourceManaged).value / "svutil" / "version"
      IO.write(versFile, version.value)
      Seq(versFile)
    }.taskValue,
    
    // Task to create the distribution zip file
    Compile / stage := {
      val log = streams.value.log
      assembly.value           // Depends on the main package being built and assembled
      def rebaseTo(directory: File)(origfile: File): Option[File] = {
        val mapper: FileMap = Path.flat(directory)
        mapper(origfile)
      }
      val pkgDir     = target.value / s"sv-${version.value}"
      val assemblyFile = (assembly / assemblyOutputPath).value
      val zipfile    = file(s"${pkgDir.getAbsolutePath}.zip")
      val scripts    = (sourceScripts.value * "*").get
      val assets     = (scripts pair rebaseTo(pkgDir)) :+ (assemblyFile -> rebaseTo(pkgDir)(assemblyFile).get)

      log.info(s"Staging to $pkgDir ...")
      IO.delete(pkgDir)
      IO.delete(zipfile)
      IO.copy(assets, CopyOptions().withOverwrite(true))
      // Make bash scripts executable
      for (script <- scripts; pkgScript = rebaseTo(pkgDir)(script).get)
        IO.setPermissions(pkgScript, "rwxr-xr-x")

      // Create zip file
      (pkgDir ** ".DS_Store").get foreach IO.delete
      // IO.zip does not preserve the executable file permission on the script file
      // val zipEntries = (pkgDir ** "*").get map (f => (f, IO.relativize(target.value, f).get) )
      // IO.zip(zipEntries, zipfile, None)
      // Relative to pkgDir (rather than target) so the files are not beeath a directory
      Utilities.createZipFile(zipfile, pkgDir, (pkgDir ** "*").get map (f => IO.relativizeFile(pkgDir, f).get))
    }
  )  








