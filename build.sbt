
name := "RobBot"

version := "1.0"

scalaVersion := "2.11.8"

lazy val server = project
  .settings(
      libraryDependencies ++= Seq(
          "org.typelevel" %% "cats" % "0.7.2"
      )
  )

lazy val client = project.enablePlugins(ScalaJSPlugin)
  .settings(
      persistLauncher := true,
      persistLauncher in Test := false,
      libraryDependencies ++= Seq(
          "org.scala-js" %%% "scalajs-dom" % "0.9.1",
          "com.lihaoyi" %%% "scalatags" % "0.5.2"
      )
  )
