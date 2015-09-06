name := "alter"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.12" % "0.3.1" % "test"
)
