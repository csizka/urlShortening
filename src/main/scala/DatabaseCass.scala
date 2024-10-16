package com.junicamp

import com.datastax.oss.driver.api.core.CqlSession
import com.datastax.oss.driver.api.core.cql.*

import io.lemonlabs.uri._
import java.util.Calendar
import java.net.InetSocketAddress
import com.datastax.oss.driver.api.core.CqlIdentifier

case class DatabaseCass(sess: CqlSession, tableName: String) {

  import DatabaseCass.*

  private val selectStatement = sess.prepare(s"SELECT url FROM ${tableName} WHERE handle = :handle")
  //IF NOT EXISTS Inserts a new row of data if no rows match the PRIMARY KEY value(s).
  private val insertStatement = sess.prepare(s"INSERT INTO ${tableName} (handle, url, timestamp) VALUES (:handle, :url, :timestamp) IF NOT EXISTS")

  //returns a URL from the database that is associated with the given handle if there is one.
  def lookup(handle: String): Option[String] = {
    val boundSelect = selectStatement.bind().setString("handle", handle)
    val resSet = sess.execute(boundSelect)
    val firstRes = Option(sess.execute(boundSelect).one())
    firstRes.map(_.getString("url"))
  }

  //returns a handle that is associated with a given URL, if it is not present in the table, it inserts them and returns the handle.
  def getOrInsertUrl(url: String): Either[String, String] = {
    val startHandle = Utils.encodeUrl(url)

      def getOrInsertHandle(handle: String, url: String): Either[String, String] = {
        lookup(handle).fold{
          val boundInsert = insertStatement.bind().setString("handle", handle).setString("url", url)
          val resSet = sess.execute(boundInsert)
          if (resSet.wasApplied())
            Right(handle)
          else {
            getOrInsertHandle(handle, url)
          }
        } {foundUrl => if (foundUrl.contains(url)) Left(handle) else getOrInsertHandle(Utils.encodeUrl(handle), url)
        }
      }
    getOrInsertHandle(startHandle, url)
  }

  //Checks if the URL is present in the database, and if yes, what the handle associated with it is.
  def findHandle(url: String): Option[String] = {
    val startHandle = Utils.encodeUrl(url)

      def findHandle(handle: String, url: String): Option[String] = {
        val boundStatement = selectStatement.bind().setString("handle", handle)
        val resSet = sess.execute(boundStatement).iterator()
        val handleExistsInDb = resSet.hasNext()
        if (handleExistsInDb && resSet.next().getString("url") == url) {
          Some(handle)
        } else if (handleExistsInDb) {
          findHandle(Utils.encodeUrl(handle), url)
        } else {
          None
        } 
      }

    findHandle(startHandle, url)
  }

  def printRecords(): Unit = {
    val resSet = sess.execute(s"SELECT * FROM ${tableName};")

    def printUrlHandlePairs(resSet: ResultSet): Unit = {
      println("printing handle - URL pairs:")
      val iterator = resSet.iterator()
      while (iterator.hasNext()) {
        val curRow = iterator.next()
        val url = curRow.getString("url")
        val handle = curRow.getString("handle")
        println(s"hande: ${handle} - URL: ${url}")
      }
      println("all requested values printed")
    }

    printUrlHandlePairs(resSet)
  }
  
  def insertAndPrintRows(rows: Seq[(String, String)] = Seq(("test", "test"))): Unit = {
    insertRows(rows)
    printRecords()
  }

  def insertAndPrintRows(handle: String, url: String): Unit = {
    insertAndPrintRows(Seq((handle, url)))
  }

  def insertRows(rows: Seq[(String, String)]): Unit = {
    val count = rows.foldLeft (0) { case (curCount, (handle, url)) => 
      val boundStatement = insertStatement.bind().setString("handle", handle).setString("url", url)
        val resSet = sess.execute(boundStatement)
        val nextCount = if (resSet.wasApplied()) curCount + 1 else curCount 
        nextCount
    }
    println(s"${count} record(s) inserted into the table.")
  }
}

object DatabaseCass {
 
  def withDatabase[T](tableName: String) (action: DatabaseCass => T): T = {
    val db = new DatabaseCass(startSession(), tableName)

    try {
      val res = action(db)
      res
    } catch {
      case e: Throwable => throw e
    } finally {
      db.sess.close()
    }
  }

  def startSession(): CqlSession = {
    val keyspaceName = "cassandra"
    val host = "localhost"
    val port = 9042
    val user = "cassandra"
    val pw = "password"
    
    CqlSession.builder()
      .addContactPoint(new InetSocketAddress(host, port))
      .withKeyspace(CqlIdentifier.fromCql(keyspaceName))
      .build()
  }
}
