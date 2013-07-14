name := "lift-formality"

organization := "com.withoutincident"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

resolvers += "Sonatype Snapshots Repository" at "http://oss.sonatype.org/content/repositories/snapshots"

{
  val liftVersion = "2.6-SNAPSHOT"
  libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion
  )
}

libraryDependencies += "com.chuusai" %% "shapeless" % "1.2.4"

scalacOptions += "-deprecation"
