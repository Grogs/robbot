
version in ThisBuild := "1.0"
scalaVersion in ThisBuild := "2.11.8"

lazy val commonSettings = Seq(
)

// loads the server project at sbt startup
onLoad in Global := (Command.process("project server", _: State)) compose (onLoad in Global).value

lazy val server = project
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % "0.7.2",
      "com.chuusai" %% "shapeless" % "2.3.2"
    )
  )
  .dependsOn(sharedJvm)

lazy val shared = crossProject.crossType(CrossType.Pure)
  .settings(
    persistLauncher := true,
    persistLauncher in Test := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % "0.7.2"
    )
  )
lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

lazy val client = project.enablePlugins(ScalaJSPlugin)
  .settings(
    persistLauncher := false,
    persistLauncher in Test := false,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.1",
      "org.typelevel" %%% "cats" % "0.7.2",
      "com.lihaoyi" %%% "scalatags" % "0.5.2"
    )
  )
  .dependsOn(sharedJs)
