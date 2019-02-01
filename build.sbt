import Dependencies._
import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "br.com.glauber"

lazy val root = (project in file("."))
  .settings(
    name := "Algebraic Expressions Simplification",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      fastParse % Compile,
      parsec % Compile,
    )
  )
