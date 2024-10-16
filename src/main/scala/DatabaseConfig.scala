package com.junicamp

case class DatabaseConfig(
  host: String,
  port: Int, 
  dbName: String,
  keySpaceName: String,
  user: String, 
  password: Secret, 
)

case class Secret(pw: String) {
  override def toString(): String = "<NON-PRINTABLE>"
}
