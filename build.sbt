name := "flights-csv"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies += "org.specs2" %% "specs2-core" % "4.0.2" % "test"
scalacOptions in Test ++= Seq("-Yrangepos")