
lazy val macros = (project in file("macros")).settings(
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % "2.11.7"
  )
)

lazy val root = (project in file(".")).settings(
  name := "js-num-to-string-semantics",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.7",
  persistLauncher in Compile := true,
  persistLauncher in Test := false,
  testFrameworks += new TestFramework("utest.runner.Framework"),
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.2",
    "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
  )
).enablePlugins(ScalaJSPlugin).dependsOn(macros).aggregate(macros)
