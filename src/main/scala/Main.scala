import java.sql.{Array => SqlArray, *}
import org.apache.commons.codec.digest.DigestUtils
import io.seruco.encoding.base62.Base62
import scala.annotation.tailrec
import java.util.Random


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

def getOrInsertHandle(insertStatement: PreparedStatement, selectStatement: PreparedStatement, handle: String, url: String): Either[String, String] = {
  insertStatement.setString(1, handle)
  val insertedCount = insertStatement.executeUpdate()
  if (insertedCount > 0) {
    Right(handle)
  } else {
    lookup(selectStatement, handle) match {
      case Some(urlFromDB) if urlFromDB == url => Left(handle)
      case _ => getOrInsertHandle(insertStatement, selectStatement, encodeUrl(handle), url)
    }
  }
}

def lookup(selectStatement: PreparedStatement, handle: String): Option[String] = {
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

def getOrInsert(url: String): Either[String, String] = {
  val startHandle = encodeUrl(url)
  val conn = connectToDB()
  val insertStatement = conn.prepareStatement(s"INSERT INTO url VALUES (?, '${url}') ON CONFLICT (handle) DO NOTHING;")
  val selectStatement = conn.prepareStatement(s"SELECT url FROM url WHERE handle = ?")
  val finalHandle = getOrInsertHandle(insertStatement, selectStatement, startHandle, url)

  insertStatement.close()
  selectStatement.close()
  conn.close()
  finalHandle
}

//tests
def findHandle(url: String): Option[String] = {
  val conn = connectToDB()
  val startHandle = encodeUrl(url)
  val stmnt = conn.prepareStatement(s"SELECT url FROM url WHERE handle = ?;")
  val res = findHandle(stmnt, startHandle, url)
  stmnt.close()
  conn.close()
  res
}

@tailrec
def findHandle(selectStatement: PreparedStatement, handle: String, url: String): Option[String] = {
  selectStatement.setString(1, handle)
  val resSet = selectStatement.executeQuery()
  val handleExistsInDb = resSet.next()
  if (handleExistsInDb && resSet.getString("url") == url) {
    resSet.close() 
    Some(handle)
  } else if (handleExistsInDb) {
    resSet.close() 
    findHandle(selectStatement, encodeUrl(handle), url)
  } else {
    resSet.close() 
    None
  } 
}

def shouldInsertNewEntry(url: String): Unit = {
  assert(findHandle(url).isEmpty)
  assert(getOrInsert(url).isRight)
  assert(findHandle(url).isDefined)
}

def shouldAlreadyExist(url: String): Unit = {
  assert(findHandle(url).isDefined)
  assert(getOrInsert(url).isLeft)
}

def clearTable(): Unit = {
  val conn = connectToDB()
  val query = "TRUNCATE TABLE url;"
  val statement = conn.prepareStatement(query)
  statement.executeUpdate()
  statement.close()
  conn.close()
}
def stringGenerator(n: Int, set: Set[String]): Set[String] = {
  val rand = new Random
  val charSet = ('a' to 'z').toVector ++ ('A' to 'Z').toVector ++ ('0' to '9').toVector
  val size = charSet.size
  if (set.size < n) {
    val nextString = (1 to 15).map{ _ => 
      charSet(rand.nextInt(size))
    }.mkString
    stringGenerator(n, set + nextString)
  } else {
    set
  }
}

def numSetGen(n: Int): List[String] = {
  (1 to n).map(_.toString()).toList
}

def findCollisions(n: Int): List[(String, String)] = {
  val testSet = numSetGen(n)
  val start = (Map[String, List[String]](), List[(String, String)]())
  val (finalMap, finalCollisions) = testSet.foldLeft{start} { case ((curMap, curCollisions), curElem) =>
    val curHash = encodeUrl(curElem)
    val nextCollisions = curMap.getOrElse(curHash, List()).foldLeft(curCollisions) {
       case (collList, curCollidingElem) => (curElem, curCollidingElem) :: curCollisions
    }
    val nextMap = curMap + (curHash -> (curElem :: curMap.getOrElse(curHash, List())))
    (nextMap, nextCollisions)
  }
  finalCollisions
}

  /* found collisions at 
  * 3M rand strings: List((2821091,2255261), (2143991,1861715))
  * 5M rand strings: List((4701251,2946054), (4441313,1266660), (3005482,2443514), (2821091,2255261), (2143991,1861715))*/
val collPairs = List(("4701251","2946054"), ("4441313","1266660"), ("3005482","2443514"), ("2821091","2255261"), ("2143991","1861715"))

@main 
def runFns(): Unit = {
  
 
  // clearTable()
  // shouldInsertNewEntry("twitter.com")
  // shouldAlreadyExist("twitter.com")
  // shouldInsertNewEntry("alibaba.com")
  // println("finishesd")
}