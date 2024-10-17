package com.junicamp

import java.sql.{Array => SqlArray, *}
import io.lemonlabs.uri._
import java.util.Calendar
import com.junicamp.Main.db


case class DatabasePostgres(conn: Connection, tableName: String) extends Database {

  import DatabasePostgres.*

  private val selectStatement = conn.prepareStatement(s"SELECT url FROM ${tableName} WHERE handle = ?")
  private val insertStatement = conn.prepareStatement(s"INSERT INTO ${tableName} VALUES (?, ?, ?) ON CONFLICT (handle) DO NOTHING")
  private val selectAllstmnt = conn.prepareStatement(s"SELECT * FROM ${tableName};")

  //returns a URL from the database that is associated with the given handle if there is one.
  override def lookup(handle: String): Option[String] = {
    selectStatement.setString(1, handle)
    val resSet = selectStatement.executeQuery()
    val elemsExist = resSet.next()
    if (elemsExist) {
      val res = Some(resSet.getString("url"))
      resSet.close()
      res
    } else{
      resSet.close() 
      None
    } 
  }

  //returns a handle that is associated with a given URL, if it is not present in the table, it inserts them and returns the handle.
  override def getOrInsertHandle(url: String): Either[String, String] = {
    val startHandle = Utils.encodeUrl(url)
    insertStatement.setString(2, url)

    def getOrInsertHandle(handle: String, url: String): Either[String, String] = {
      val now = java.sql.Timestamp.from(java.time.Instant.now()) 
      insertStatement.setString(1, handle)
      insertStatement.setString(2, url)
      insertStatement.setTimestamp(3, now)
      val insertedCount = insertStatement.executeUpdate()
      if (insertedCount > 0) {
        Right(handle)
      } else {
        lookup(handle) match {
          case Some(urlFromDB) if urlFromDB == url => Left(handle)
          case _ => getOrInsertHandle(Utils.encodeUrl(handle), url)
        }
      }
    }
    getOrInsertHandle(startHandle, url)
  }

  //Checks if the URL is present in the database, and if yes, what the handle associated with it is.
  def findHandle(url: String): Option[String] = {
    val startHandle = Utils.encodeUrl(url)

    def findHandle(handle: String, url: String): Option[String] = {
      selectStatement.setString(1, handle)
      val resSet = selectStatement.executeQuery()
      val handleExistsInDb = resSet.next()
      if (handleExistsInDb && resSet.getString("url") == url) {
        resSet.close() 
        Some(handle)
      } else if (handleExistsInDb) {
        resSet.close() 
        findHandle(Utils.encodeUrl(handle), url)
      } else {
        resSet.close() 
        None
      } 
    }

    findHandle(startHandle, url)
  }

  def closeConn(): Unit = {
    conn.close()
  }
}

object DatabasePostgres extends MkDatabase {

  val defaultConfig = DatabaseConfig(
    host = "postgres",
    port = 5432,
    dbName = "postgres",
    user = "postgres",
    password = Secret("password"),
    keySpaceName = ""
  )
 
  def withDatabase[T](action: DatabasePostgres => T): T = {
    val db = makeDatabase()

    try {
      val res = action(db)
      res
    } catch {
      case e: Throwable => throw e
    } finally {
      db.closeConn()
    }
  }

  override def makeDatabase(config: DatabaseConfig = defaultConfig, urlTableName: String = "url"): DatabasePostgres = {
    val url = s"jdbc:postgresql://${config.host}:${config.port}/${config.dbName}?user=${config.user}&password=${config.password.pw}"
    val conn = DriverManager.getConnection(url)
    DatabasePostgres(conn, urlTableName)
  }

}
