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


lazy val svnl1 = (project in file("."))
  .settings(
    commonSettings,
    name        := "svnl1",
    description := "Subversion logging utility",
    scalacOptions       ++= Seq( "-deprecation", "-unchecked", "-feature" ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "2.1.0"
    ),
    sourceScripts := sourceDirectory.value / "scripts",
    Compile / resourceGenerators += Def.task {
      val versFile = (Compile / resourceManaged).value / "version"
      IO.write(versFile, version.value)
      Seq(versFile)
      }.taskValue,
    
    // Task to create the distribution zip file
    Compile / stage := {
      val log = streams.value.log
      (loader / Compile / packageBin).value  // Depends on the loader package being built
      (Compile / packageBin).value           // Depends on the main package being built
      def rebaseTo(directory: File)(origfile: File): Option[File] = {
        val mapper: FileMap = Path.flat(directory)
        mapper(origfile)
      }
      val pkgDir     = target.value / s"svnl1-${version.value}"
      val lib        = pkgDir / "lib"
      val loader_jar = (loader / Compile / packageBin / artifactPath).value
      val zipfile    = file(s"${pkgDir.getAbsolutePath}.zip")
      val jars       = (Compile / fullClasspathAsJars).value.files
      val scripts    = (sourceScripts.value * "*").get
      val assets     = (scripts pair rebaseTo(pkgDir)) ++ (jars pair rebaseTo(lib))
      
      log.info(s"Staging to $pkgDir ...")
      IO.delete(pkgDir)
      IO.createDirectory(lib)
      IO.copyFile(loader_jar, lib / loader_jar.getName)
      IO.copy(assets, CopyOptions().withOverwrite(true))
      // Make bash scripts executable
      for (script <- scripts; pkgScript = rebaseTo(pkgDir)(script).get)
        IO.setPermissions(pkgScript, "rwxr-xr-x") 
      
      // Create zip file
      (pkgDir ** ".DS_Store").get foreach IO.delete
      val zipEntries = (pkgDir ** "*").get map (f => (f, IO.relativize(target.value, f).get) )
      IO.zip(zipEntries, zipfile, None)
    }
  )
  
  lazy val loader = (project in file("loader"))
    .settings(
      commonSettings,
      name        := "Loader",
      description := "Bootstrap loader",
      (Compile / mainClass) := Some("loader.Loader"),
      // Make loader.jar generic without version number so the scripts can find it.
      (Compile / packageBin / artifactPath) := (Compile / target).value / "loader.jar"
    )








