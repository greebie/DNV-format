enablePlugins(JavaAppPackaging)

resolvers in ThisBuild += "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/"

resolvers in ThisBuild += "Typesafe Simple Repository" at
  "http://repo.typesafe.com/typesafe/simple/maven-releases/"

lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "com.dnv-parser",
  scalaVersion := "2.12.6"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "dnv-parser",
    libraryDependencies ++= Seq(
      "org.scala-lang"    %  "scala-library"    % "2.12.6",
      "com.typesafe"      % "config"            % "1.3.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.scalanlp" %% "breeze" % "1.0",
      "org.scalanlp" %% "breeze-natives" % "1.0",
      "org.scalanlp" %% "breeze-viz" % "1.0",
      "org.scalatest"     %% "scalatest"        % "3.0.5"     % Test,
      "junit"             % "junit"             % "4.12"      % Test,
    ))
