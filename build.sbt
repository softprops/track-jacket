organization := "me.lessis"

name := "track-jacket"

version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-json4s-native" % "0.11.1")

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](version)

buildInfoPackage := "trackjacket"

scalacOptions += Opts.compile.deprecation
