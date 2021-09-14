name := "training"

version := "0.1"

scalaVersion := "2.13.3"

val versions = new {
  val scalatest = "3.2.2"
  val mockito = "1.10.19"
}
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % versions.scalatest % "test",
  "org.mockito" % "mockito-all" % versions.mockito % "test")
