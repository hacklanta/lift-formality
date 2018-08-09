val liftVersion = settingKey[String]("Lift Web Framework full version number")
val liftEdition = settingKey[String]("Lift Edition (such as 2.6 or 3.0)")

name := "lift-formality"

organization := "com.hacklanta"

version := "1.2.0-SNAPSHOT"

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.12.6", "2.11.12")

liftVersion := (liftVersion ?? "3.1.1").value

liftEdition := liftVersion.value.substring(0,3)

moduleName := name.value + "_" + liftEdition.value

resolvers += "Sonatype Snapshots Repository" at "http://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++=
  Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion.value,
    "net.liftweb" %% "lift-testkit" % liftVersion.value % "test"
  )

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.0.2" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "4.0.2" % "test",
  "org.mortbay.jetty" % "jetty" % "6.1.22" % "test"
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

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".sonatype")
