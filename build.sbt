samskivert.POMUtil.pomToSettings("pom.xml")

net.virtualvoid.sbt.graph.Plugin.graphSettings

crossPaths := false

autoScalaLibrary := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize",
                      "-language:postfixOps" /*, "-Yinline-warnings"*/)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"
