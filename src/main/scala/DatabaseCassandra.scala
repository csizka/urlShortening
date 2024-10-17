package com.junicamp

import com.datastax.oss.driver.api.core.CqlSession
import com.datastax.oss.driver.api.core.cql.*

import io.lemonlabs.uri._
import java.util.Calendar
import java.net.InetSocketAddress
import com.datastax.oss.driver.api.core.CqlIdentifier

case class DatabaseCassandra(sess: CqlSession, tableName: String) extends Database {

  import DatabaseCassandra.*

  private val selectStatement = sess.prepare(s"SELECT url FROM ${tableName} WHERE handle = :handle")
  //IF NOT EXISTS Inserts a new row of data if no rows match the PRIMARY KEY value(s).
  private val insertStatement = sess.prepare(s"INSERT INTO ${tableName} (handle, url, timestamp) VALUES (:handle, :url, :timestamp) IF NOT EXISTS")

  //returns a URL from the database that is associated with the given handle if there is one.
  override def lookup(handle: String): Option[String] = {
    val boundSelect = selectStatement.bind().setString("handle", handle)
    val resSet = sess.execute(boundSelect)
    val firstRes = Option(sess.execute(boundSelect).one())
    firstRes.map(_.getString("url"))
  }

  //returns a handle that is associated with a given URL, if it is not present in the table, it inserts them and returns the handle.
  override def getOrInsertHandle(url: String): Either[String, String] = {
    val startHandle = Utils.encodeUrl(url)

      def getOrInsertHandle(handle: String, url: String): Either[String, String] = {
        val timestamp = java.time.Instant.now().getEpochSecond()

        lookup(handle).fold{
          val boundInsert = insertStatement.bind().setString("handle", handle).setString("url", url).setQueryTimestamp(timestamp)
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

  def closeSess() =
    sess.close()
}

object DatabaseCassandra extends MkDatabase {

  val defaultConfig = DatabaseConfig(
    keySpaceName = "cassandra",
    host = "cassandra",
    port = 9042,
    user = "cassandra",
    password = Secret("password"),
    dbName = ""
  )
 
  def withDatabase[T](action: DatabaseCassandra => T): T = {
    val db = makeDatabase()

    try {
      val res = action(db)
      res
    } catch {
      case e: Throwable => throw e
    } finally {
      db.closeSess()
    }
  }
  override def makeDatabase(config: DatabaseConfig = defaultConfig, urlTableName: String = "url"): DatabaseCassandra = {
     DatabaseCassandra(CqlSession.builder()
      .addContactPoint(new InetSocketAddress(config.host, config.port))
      .withKeyspace(CqlIdentifier.fromCql(config.keySpaceName))
      .build(), 
      urlTableName)
  }
}
