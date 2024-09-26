import java.sql.{Array => SqlArray, *}
import org.apache.commons.codec.digest.DigestUtils
import io.seruco.encoding.base62.Base62


// helper fn for getconn
def connectToDB(): Connection = {
  val dbName = "mydb"
  val host = "localhost"
  val port = "5432"
  val url = s"jdbc:postgresql://${host}:${port}/${dbName}"
  val conn = DriverManager.getConnection(url)
  conn
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

def insertHandle(insertStatement: PreparedStatement, selectStatement: PreparedStatement, handle: String, url: String): Either[String, String] = {
  insertStatement.setString(1, handle)
  val insertedCount = insertStatement.executeUpdate()
  if (insertedCount > 0) {
    Right(handle)
  } else {
    lookup(selectStatement, handle) match {
      case Some(urlFromDB) if urlFromDB == url => Left(handle)
      case _ => insertHandle(insertStatement, selectStatement, encodeUrl(handle), url)
    }
  }
}

def lookup(selectStatement: PreparedStatement, handle: String): Option[String] = {
  selectStatement.setString(1, handle)
  val resSet = selectStatement.executeQuery()
  if (resSet.next()) {
    val res = Some(resSet.getString("url"))
    resSet.close()
    res
  } else{
    resSet.close() 
    None
  } 
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
    
  resSet.close()
  stmnt.close()
  conn.close()
}

def insertAndPrintRows(rows: Seq[(String, String)] = Seq(("test2", "test2"))): Unit = {
  val tableName = "url"
  val conn = connectToDB()

  val insertStmnt = conn.prepareStatement(s"INSERT INTO ${tableName} VALUES (?, ?) ON CONFLICT (handle) DO NOTHING;")

  insertRows(insertStmnt, rows)
  insertStmnt.close()
  
  val selectAllStmnt = conn.prepareStatement(s"SELECT * FROM ${tableName};")
  val resSet = selectAllStmnt.executeQuery()

  printUrlHandlePairs(resSet)

  resSet.close()
  selectAllStmnt.close()
  conn.close()
}

def insertAndPrintRows(handle: String, url: String): Unit = {
  insertAndPrintRows(Seq((handle, url)))
}


def lookup(handle: String): Option[String] = {
  val conn = connectToDB()
  val stmnt = conn.prepareStatement(s"SELECT url FROM url WHERE handle = '${handle}';")
  val res = lookup(stmnt, handle)
  stmnt.close()
  conn.close()
  res
}

def getHandle(url: String): Either[String, String] = {
  val startHandle = encodeUrl(url)
  val conn = connectToDB()
  val inSertStatement = conn.prepareStatement(s"INSERT INTO url VALUES (?, '${url}') ON CONFLICT (handle) DO NOTHING;")
  val selectStatement = conn.prepareStatement(s"SELECT url FROM url WHERE handle = ?")
  val finalHandle = insertHandle(inSertStatement, selectStatement, startHandle, url)

  inSertStatement.close()
  selectStatement.close()
  conn.close()
  finalHandle
}

//tests
def lookupUrl(url: String): Option[String] = {
  val conn = connectToDB()
  val startHandle = encodeUrl(url)
  val stmnt = conn.prepareStatement(s"SELECT url FROM url WHERE handle = ?;")
  val res = lookupUrl(stmnt, startHandle, url)
  stmnt.close()
  conn.close()
  res
}

def lookupUrl(selectStatement: PreparedStatement, handle: String, url: String): Option[String] = {
  selectStatement.setString(1, handle)
  val resSet = selectStatement.executeQuery()
  val handleExistsInDb = resSet.next()
  if (handleExistsInDb && resSet.getString("url") == url) {
    resSet.close()
    Some(handle)
  } else if (handleExistsInDb) {
    resSet.close() 
    lookupUrl(selectStatement, encodeUrl(handle), url)
  } else {
    None
  } 
}
// def shouldInsertNewEntry(url: String): Unit = {
//   val handle = encodeUrl(url)

//   val insertRes = getHandle(url) 
//   val inserted = ???
//   ????
// }

@main 
def runFns(): Unit = {
  println(lookupUrl("tes1"))
}