package com.junicamp

import java.sql.{Array => SqlArray, *}
import io.lemonlabs.uri._
import java.util.Calendar


case class Database(conn: Connection, tableName: String) {

  import Database.*

  private val selectStatement = conn.prepareStatement(s"SELECT url FROM ${tableName} WHERE handle = ?")
  private val insertStatement = conn.prepareStatement(s"INSERT INTO ${tableName} VALUES (?, ?, ?) ON CONFLICT (handle) DO NOTHING")
  private val selectAllstmnt = conn.prepareStatement(s"SELECT * FROM ${tableName};")
  
  // val timestamp = java.time.Instant.now()
  // val javaTimeStamp : java.sql.Timestamp 
  // insertStatement.set

  def lookup(handle: String): Option[String] = {
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

  def getOrInsertHandle(url: String): Either[String, String] = {
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

  def printRecords(): Unit = {
    val resSet = selectAllstmnt.executeQuery()
    printUrlHandlePairs(resSet)
    resSet.close()
  }

  def insertRows(rows: Seq[(String, String)]): Unit = {
    val count = rows.foldLeft (0) { case (curCount, (handle, url)) => 
        insertStatement.setString(1, handle)
        insertStatement.setString(2, url)
        insertStatement.executeUpdate() + curCount 
    }
    println(s"${count} record(s) inserted to the table.")
  }

  def printUrlHandlePairs(resSet: ResultSet): Unit = {
    println("printing handle - URL pairs:")
    while (resSet.next()) {
      val url = resSet.getString("url")
      val handle = resSet.getString("handle")
      println(s"hande: ${handle} - URL: ${url}")
    }
    println("all requested values printed")
  }

  def insertAndPrintRows(rows: Seq[(String, String)] = Seq(("test", "test"))): Unit = {
    insertRows(rows)
    printRecords()
  }

  def insertAndPrintRows(handle: String, url: String): Unit = {
      insertAndPrintRows(Seq((handle, url)))
    }
  
  def closeConn(): Unit = {
    conn.close()
  }
}

object Database {
 
  def withDatabase[T](tableName: String) (action: Database => T): T = {
    val db = new Database(connectToDB(), tableName)

    try {
      val res = action(db)
      res
    } catch {
      case e: Throwable => throw e
    } finally {
      db.closeConn()
    }
  }

  def connectToDB(): Connection = {
    val dbName = "postgres"
    val host = "postgres"
    val port = "5432"
    val user = "postgres"
    val pw = "password"
    val url = s"jdbc:postgresql://${host}:${port}/${dbName}?user=${user}&password=${pw}"
    val conn = DriverManager.getConnection(url)
    conn
  }

}
