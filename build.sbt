name := "lift-formality"

organization := "com.hacklanta"

version := "0.4.0-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers += "Sonatype Snapshots Repository" at "http://oss.sonatype.org/content/repositories/snapshots"

{
  val liftVersion = "2.6"
  libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion,
    "net.liftweb" %% "lift-testkit" % liftVersion % "test",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test"
  )
}

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.5" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.8.5" % "test"
)

scalacOptions ++= Seq("-deprecation","-feature","-Xfatal-warnings")

//scalacOptions in Test ++= Seq("-Yrangepos")

pomExtra :=
<url>http://github.com/Shadowfiend/sbt-resource-management</url>
<licenses>
  <license>
    <name>MIT</name>
    <url>http://opensource.org/licenses/MIT</url>
    <distribution>repo</distribution>
  </license>
</licenses>
<scm>
  <url>https://github.com/hacklanta/lift-formality.git</url>
  <connection>https://github.com/hacklanta/lift-formality.git</connection>
</scm>
<developers>
  <developer>
    <id>shadowfiend</id>
    <name>Antonio Salazar Cardozo</name>
    <email>savedfastcool@gmail.com</email>
  </developer>
</developers>;

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".sonatype")
