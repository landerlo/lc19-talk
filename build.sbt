lazy val root = project
  .in(file("."))
  .settings(
    name := "lc19-code",
    description := "Algebraic syntax with union types in  Dotty",
    version := "0.1.0",

    scalaVersion := "0.15.0-RC1"
  )
