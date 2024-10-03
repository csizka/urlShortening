package com.junicamp

import java.sql.{Array => SqlArray, *}
import org.apache.commons.codec.digest.DigestUtils
import io.seruco.encoding.base62.Base62


case class Database(conn: Connection, tableName: String) {

  import Database.*

  private val selectStatement = conn.prepareStatement(s"SELECT url FROM ${tableName} WHERE handle = ?")
  private val insertStatement = conn.prepareStatement(s"INSERT INTO ${tableName} VALUES (?, ?) ON CONFLICT (handle) DO NOTHING")
  private val selectAllstmnt = conn.prepareStatement(s"SELECT * FROM ${tableName};")
  

  def getOrInsertUrl(url: String): Either[String, String] = {
    val startHandle = encodeUrl(url)
    insertStatement.setString(2, url)
    val finalHandle = getOrInsertHandle(startHandle, url)

    finalHandle
  }

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

  def printRecords(): Unit = {
    val resSet = selectAllstmnt.executeQuery()
    printUrlHandlePairs(resSet)
    resSet.close()
  }

  def insertAndPrintRows(rows: Seq[(String, String)] = Seq(("test", "test"))): Unit = {
    insertRows(rows)
    val resSet = selectAllstmnt.executeQuery()
    printUrlHandlePairs(resSet)
    resSet.close()
  }

  def insertAndPrintRows(handle: String, url: String): Unit = {
      insertAndPrintRows(Seq((handle, url)))
    }

  def findHandleFromUrl(url: String): Option[String] = {
    val startHandle = encodeUrl(url)
    val res = findHandle(startHandle, url)
    res
  }

  def findHandle(handle: String, url: String): Option[String] = {
    selectStatement.setString(1, handle)
    val resSet = selectStatement.executeQuery()
    val handleExistsInDb = resSet.next()
    if (handleExistsInDb && resSet.getString("url") == url) {
      resSet.close() 
      Some(handle)
    } else if (handleExistsInDb) {
      resSet.close() 
      findHandle(encodeUrl(handle), url)
    } else {
      resSet.close() 
      None
    } 
  }

  def getOrInsertHandle(handle: String, url: String): Either[String, String] = {
    insertStatement.setString(1, handle)
    insertStatement.setString(2, url)
    val insertedCount = insertStatement.executeUpdate()
    if (insertedCount > 0) {
      Right(handle)
    } else {
      lookup(handle) match {
        case Some(urlFromDB) if urlFromDB == url => Left(handle)
        case _ => getOrInsertHandle(encodeUrl(handle), url)
      }
    }
  }

  def insertRows(rows: Seq[(String, String)]): Unit = {
    val count = rows.foldLeft (0) { case (curCount, (handle, url)) => 
        insertStatement.setString(1, handle)
        insertStatement.setString(2, url)
        insertStatement.executeUpdate() + curCount 
    }
    println(s"${count} record(s) inserted to the table.")
  }

  def closeConn(): Unit = {
    conn.close()
  }
}

object Database {

  private val base62 = Base62.createInstance()
 
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
    val dbName = "mydb"
    val host = "localhost"
    val port = "5432"
    val url = s"jdbc:postgresql://${host}:${port}/${dbName}"
    val conn = DriverManager.getConnection(url)
    conn
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

  def encodeUrl(url: String): String = 
    base62.encode(DigestUtils.md5(url)).map(_.toChar).mkString.takeRight(7)

  def addScheme(url: String): String = {
    (url)
  }
}
