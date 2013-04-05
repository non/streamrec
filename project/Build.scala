import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    // organization := "org.scalamacros",
    // version := "1.0.0",
    // scalacOptions ++= Seq("-deprecation"),
    // scalaVersion := "2.11.0-SNAPSHOT",
    // scalaOrganization := "org.scala-lang.macro-paradise",
    // resolvers += Resolver.sonatypeRepo("snapshots")

    version := "0.1.0",
    scalacOptions ++= Seq("-deprecation"),
    scalaVersion := "2.10.0",

    libraryDependencies += "org.spire-math" %% "spire" % "0.4.0-M3"
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("core"),
    settings = buildSettings
  ) aggregate(macros, core)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
    )
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings
  ) dependsOn(macros)
}
