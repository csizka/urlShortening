import java.sql.*
import scala.io.StdIn
import scala.util.{Try, Success, Failure}


def printUrlHandlePairs(resSet: ResultSet): Unit = {
  while (resSet.next()) {
    val url = resSet.getString("url")
    val handle = resSet.getString("handle")
    println(s"hande: ${handle} - URL: ${url}")
  }
  println("all requested values printed")
}

def insertRows(statement: Statement, rows: Seq[(String, String)], table: String): Unit = {
  val count = rows.foldLeft (0) { case (curCount, (handle, url)) => 
      val insertQuery = s"INSERT INTO ${table} VALUES ('${handle}', '${url}') ON CONFLICT (handle) DO NOTHING;"
      statement.executeUpdate(insertQuery) + curCount 
  }
  println(s"${count} record(s) inserted into table '${table}'")
}

def bracket[A, B](acquire: => A)(use: A => B)(release: A => Unit): Try[B] = {
  val resource = Try(acquire)
  val result = resource.map(use)
  resource.map(release)
  result
}

def printRecords(): Unit = {
  val dbName = "mydb"
  val host = "localhost"
  val port = "5432"
  val tableName = "url"
  val url = s"jdbc:postgresql://${host}:${port}/${dbName}"

  bracket(DriverManager.getConnection(url)) { conn => 
    println("connection created")
    bracket(conn.createStatement()) { stmnt => 
      println("statement created")
      bracket(stmnt.executeQuery(s"SELECT * FROM ${tableName};")) { resSet => 
        println("result set created")
        printUrlHandlePairs(resSet)
      } { rs => rs.close(); println("result set closed") }
    } { stmnt => stmnt.close(); println("statement closed") }
  } { conn => conn.close(); println("connection closed") }
}

def insertAndPrintRows(rows: Seq[(String, String)] = Seq(("test1", "test1"))): Unit = {
  val dbName = "mydb"
  val tableName = "url"
  val host = "localhost"
  val port = "5432"
  val url = s"jdbc:postgresql://${host}:${port}/${dbName}"

  bracket(DriverManager.getConnection(url)){ conn =>
    println("connection created")
    bracket(conn.createStatement()) { stmnt =>
      println("statement created")
      insertRows(stmnt, rows, tableName)
      bracket(stmnt.executeQuery(s"SELECT * FROM ${tableName};")) { resSet =>
        println("result set created")
        printUrlHandlePairs(resSet)
      } {resSet => resSet.close()}
    } {stmnt => stmnt.close(); println("statement closed")}
  } {conn => conn.close(); println("connection closed")}
}

def insertAndPrintRows(handle: String, url: String): Unit = {
  insertAndPrintRows(Seq((handle, url)))
}

@main 
def runFns(): Unit = {
insertAndPrintRows()
}