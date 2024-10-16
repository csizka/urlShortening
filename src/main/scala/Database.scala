package com.junicamp

trait Database {
  type Config

  def lookup(handle: String): Option[String]
  def getOrInsertHandle(url: String): Either[String, String]
}

trait MkDatabase {
  def makeDatabase(config: DatabaseConfig, urlTableName: String): Database
}