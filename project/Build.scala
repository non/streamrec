import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "0.2.0",
    scalacOptions ++= Seq(
      "-feature",
      "-Yinline-warnings",
      "-deprecation",
      "-optimize",
      "-unchecked"
    ),
    scalaVersion := "2.10.2",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise_2.10.2" % "2.0.0-SNAPSHOT"),
    // disable annoying warnings about 2.10.x
    conflictWarning in ThisBuild := ConflictWarning.disable,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.10.2",
      "org.spire-math" %% "spire" % "0.6.1",
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1"
    )
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings
  ).aggregate(macros, core)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(
      javaOptions in run += "-Xmx6G",

      // enable forking in run
      fork in run := true,

      // custom kludge to get caliper to see the right classpath

      // we need to add the runtime classpath as a "-cp" argument to the
      // `javaOptions in run`, otherwise caliper will not see the right classpath
      // and die with a ConfigurationException. unfortunately `javaOptions` is a
      // SettingsKey and `fullClasspath in Runtime` is a TaskKey, so we need to
      // jump through these hoops here in order to feed the result of the latter
      // into the former
      onLoad in Global ~= { previous => state =>
        previous {
          state.get(key) match {
            case None =>
              // get the runtime classpath, turn into a colon-delimited string
              val classPath = Project.runTask(fullClasspath in Runtime in core, state).get._2.toEither.right.get.files.mkString(":")
              // return a state with javaOptionsPatched = true and javaOptions set correctly
              Project.extract(state).append(Seq(javaOptions in (core, run) ++= Seq("-cp", classPath)), state.put(key, true))

            case Some(_) => state // the javaOptions are already patched
          }
        }
      }
    )
  ) dependsOn(macros)

  val key = AttributeKey[Boolean]("javaOptionsPatched")
}
