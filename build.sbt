enablePlugins(ScalaJSPlugin)

name := "moonpatroller.github.io"
scalaVersion := "2.12.2" // or any other Scala version >= 2.10.2

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

// https://mvnrepository.com/artifact/org.scala-js/scalajs-dom_sjs0.6_2.12
libraryDependencies += "org.scala-js" % "scalajs-dom_sjs0.6_2.12" % "0.9.3"

libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1"

skip in packageJSDependencies := false
jsDependencies += "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js"
