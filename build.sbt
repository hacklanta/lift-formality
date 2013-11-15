name := "lift-formality"

organization := "com.hacklanta"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

resolvers += "Sonatype Snapshots Repository" at "http://oss.sonatype.org/content/repositories/snapshots"

{
  val liftVersion = "2.6-SNAPSHOT"
  libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion,
    "net.liftweb" %% "lift-testkit" % liftVersion % "test",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test"
  )
}

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4",
  "org.specs2" %% "specs2" % "1.12.3" % "test"
)

scalacOptions ++= Seq("-deprecation","-Xfatal-warnings")

//scalacOptions in Test ++= Seq("-Yrangepos")
