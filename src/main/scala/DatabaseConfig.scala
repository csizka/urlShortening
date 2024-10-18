package com.junicamp

case class DatabaseConfig(
  host: String,
  port: Int, 
  dbName: Option[String],
  keyspaceName: Option[String],
  user: Option[String], 
  password: Option[Secret], 
)

case class Secret(pw: String) {
  override def toString(): String = "<NON-PRINTABLE>"
}
