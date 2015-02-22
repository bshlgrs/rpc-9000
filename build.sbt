name := "rpc-9000"

version := "1.0"

scalaVersion := "2.11.5"

enablePlugins(ScalaJSPlugin)

libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"

scalaVersion := "2.11.5"

scalaJSStage in Global := FastOptStage

skip in packageJSDependencies := false
