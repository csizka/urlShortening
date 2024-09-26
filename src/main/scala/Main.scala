import java.sql.{Array => SqlArray, *}
import org.apache.commons.codec.digest.DigestUtils
import io.seruco.encoding.base62.Base62


// helper fns for opening/creating and closing conn, statement, resset
def connectToDB(): Connection = {
  val dbName = "mydb"
  val host = "localhost"
  val port = "5432"
  val url = s"jdbc:postgresql://${host}:${port}/${dbName}"

  val conn = DriverManager.getConnection(url)
  println("connection created")
  conn
}

def closeDBConn(conn: Connection): Unit = {
  conn.close()
  println("connection closed")
}

def closeResSet(resSet: ResultSet): Unit = {
  resSet.close()
  println("result set closed")
}

def closeStatement(stmnt: PreparedStatement): Unit = {
  stmnt.close()
  println("statement closed")
}

def closeStatement(stmnt: Statement): Unit = {
  stmnt.close()
  println("statement closed")
}

//helper fns for querying
def printUrlHandlePairs(resSet: ResultSet): Unit = {
  println("printing handle - URL pairs:")
  while (resSet.next()) {
    val url = resSet.getString("url")
    val handle = resSet.getString("handle")
    println(s"hande: ${handle} - URL: ${url}")
  }
  println("all requested values printed")
}

def insertRows(statement: PreparedStatement, rows: Seq[(String, String)]): Unit = {
  val count = rows.foldLeft (0) { case (curCount, (handle, url)) => 
      statement.setString(1, handle)
      statement.setString(2, url)
      statement.executeUpdate() + curCount 
  }
  println(s"${count} record(s) inserted to the table.")
}

//encoding related fns + vals
val base62 = Base62.createInstance()

def encodeUrl(url: String): String = 
  base62.encode(DigestUtils.md5(url)).map(_.toChar).mkString.takeRight(7)

// fns wtat utilize the helper fns to execute querys
def printRecords(): Unit = {
  val conn = connectToDB()
  val tableName = "url"
  val stmnt = conn.prepareStatement(s"SELECT * FROM ${tableName};")
  println("statement created")

  val resSet = stmnt.executeQuery()
  println("result set created")

  printUrlHandlePairs(resSet)
    
  closeResSet(resSet)
  closeStatement(stmnt)    
  closeDBConn(conn)
}

def insertAndPrintRows(rows: Seq[(String, String)] = Seq(("test2", "test2"))): Unit = {
  val tableName = "url"
  val conn = connectToDB()

  val insertStmnt = conn.prepareStatement(s"INSERT INTO ${tableName} VALUES (?, ?) ON CONFLICT (handle) DO NOTHING;")
  println("statement created")

  insertRows(insertStmnt, rows)
  closeStatement(insertStmnt)
  
  val selectAllStmnt = conn.prepareStatement(s"SELECT * FROM ${tableName};")
  println("statement created")
  val resSet = selectAllStmnt.executeQuery()
  println("result set created")

  printUrlHandlePairs(resSet)

  closeResSet(resSet)
  closeStatement(selectAllStmnt) 
  closeDBConn(conn)
}

def insertAndPrintRows(handle: String, url: String): Unit = {
  insertAndPrintRows(Seq((handle, url)))
}


def lookup(handle: String): Option[String] = {
  val conn = connectToDB()
  val stmnt = conn.prepareStatement(s"SELECT url FROM url WHERE handle LIKE '${handle}';")
  println("statement created")

  val resSet = stmnt.executeQuery()
  println("result set created")

  val res = if (resSet.next()) Some(resSet.getString("url")) else None
    
  closeResSet(resSet)
  closeStatement(stmnt)    
  closeDBConn(conn)
  res
}


@main 
def runFns(): Unit = {
  println(lookup("test0"))
}