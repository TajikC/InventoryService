name := "InventoryService"

version := "1.0"

lazy val `inventoryservice` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq( jdbc , anorm , cache , ws )

libraryDependencies += jdbc

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.27"

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  