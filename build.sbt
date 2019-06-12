lazy val root = project
  .in(file("."))
  .settings(
    name := "lc19-code",
    description := "Algebraic syntax with union types in  Dotty",
    version := "0.1.0",

    scalaVersion := "0.15.0-RC1",
    scalacOptions in Compile in console := Seq(
		"-language:implicitConversions"
    ),
    initialCommands in Compile in console := """
                                               |import quaternion.algae.AlgebraicApplication._
    """.stripMargin
  )

