package com.junicamp

trait Database {
  def lookup(handle: String): Option[String]
  def getOrInsertHandle(url: String): Either[String, String]
  def close(): Unit
}

trait MkDatabase {
  val defaultConfig: DatabaseConfig
  def makeDatabase(config: DatabaseConfig, urlTableName: String): Database
  
  def withDatabase[T](config: DatabaseConfig, urlTableName: String)(action: Database => T): T = {
    val db = makeDatabase(config, urlTableName)

    try {
      val res = action(db)
      res
    } catch {
      case e: Throwable => throw e
    } finally {
      db.close()
    }
  }

  def withDatabase[T](action: Database => T): T = {
    val db = makeDatabase(defaultConfig, "url")

    try {
      val res = action(db)
      res
    } catch {
      case e: Throwable => throw e
    } finally {
      db.close()
    }
  }
}