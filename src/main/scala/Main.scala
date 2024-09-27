import java.sql.{Array => SqlArray, *}
import org.apache.commons.codec.digest.DigestUtils
import io.seruco.encoding.base62.Base62
import scala.annotation.tailrec
import java.util.Random
import scala.collection.mutable.HashMap

case class Database(conn: Connection) {

  def getOrInsert(url: String): Either[String, String] = {
    val startHandle = encodeUrl(url)
    val insertStatement = conn.prepareStatement(s"INSERT INTO url VALUES (?, '${url}') ON CONFLICT (handle) DO NOTHING;")
    val selectStatement = conn.prepareStatement(s"SELECT url FROM url WHERE handle = ?")
    val finalHandle = getOrInsertHandle(insertStatement, selectStatement, startHandle, url)

    insertStatement.close()
    selectStatement.close()
    finalHandle
  }

  def lookup(handle: String): Option[String] = {
    val stmnt = conn.prepareStatement(s"SELECT url FROM url WHERE handle = ?;")
    val res = lookup(stmnt, handle)
    stmnt.close()
    res
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

  def printRecords(): Unit = {
    val stmnt = conn.prepareStatement(s"SELECT * FROM url;")
    val resSet = stmnt.executeQuery()

    printUrlHandlePairs(resSet)
      
    resSet.close()
    stmnt.close()
  }

  def insertAndPrintRows(rows: Seq[(String, String)] = Seq(("test", "test"))): Unit = {
    val insertStmnt = conn.prepareStatement(s"INSERT INTO url VALUES (?, ?) ON CONFLICT (handle) DO NOTHING;")
      insertRows(insertStmnt, rows)
      insertStmnt.close()
      
      val selectAllStmnt = conn.prepareStatement(s"SELECT * FROM url;")
      val resSet = selectAllStmnt.executeQuery()

      printUrlHandlePairs(resSet)

      resSet.close()
      selectAllStmnt.close()
    }

  def insertAndPrintRows(handle: String, url: String): Unit = {
      insertAndPrintRows(Seq((handle, url)))
    }

  def findHandle(url: String): Option[String] = {
    val startHandle = encodeUrl(url)
    val stmnt = conn.prepareStatement(s"SELECT url FROM url WHERE handle = ?;")
    val res = findHandle(stmnt, startHandle, url)
    stmnt.close()
    res
  }

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

  def insertRows(statement: PreparedStatement, rows: Seq[(String, String)]): Unit = {
    val count = rows.foldLeft (0) { case (curCount, (handle, url)) => 
        statement.setString(1, handle)
        statement.setString(2, url)
        statement.executeUpdate() + curCount 
    }
    println(s"${count} record(s) inserted to the table.")
  }
  def closeConn(): Unit = {
    conn.close()
  }
}

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

//encoding related fns + vals
val base62 = Base62.createInstance()

def encodeUrl(url: String): String = 
  base62.encode(DigestUtils.md5(url)).map(_.toChar).mkString.takeRight(7)

//tests

def shouldInsertNewEntry(url: String, database: Database): Unit = {
  val getHandleRes = database.getOrInsert(url)

  getHandleRes.fold(
    oldHandle => assert(false, "This should have not existed in the table"),
    newHandle => {
      val urlIsCorrect = database.lookup(newHandle).contains(url)
      assert(urlIsCorrect, s"Handle should have correct URL: ${url} instead it has: ${database.lookup(newHandle).get}")
    }
  )
}

def shouldAlreadyExist(url: String, database: Database): Unit = {
  val getHandleRes = database.getOrInsert(url)

  getHandleRes.fold(
    oldHandle => {
      val urlIsCorrect = database.lookup(oldHandle).contains(url)
      assert(urlIsCorrect, s"Handle should have correct URL: ${url} instead it has: ${database.lookup(oldHandle).get}")
    },
    newHandle => assert(false, "This should have existed in the table")
  )
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

  /* found collisions at 
  * 3M rand strings: List((2821091,2255261), (2143991,1861715))
  * 5M rand strings: List((4701251,2946054), (4441313,1266660), (3005482,2443514), (2821091,2255261), (2143991,1861715))*/

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

/* Found collisions at (1472600.com,1343617.com)*/

def findCollision(): (String, String) = {
  val handles = new HashMap[String, String]
  
  @tailrec
  def findCollisionHelper(ix: Long): (String, String) = {
    val data = s"${ix}.com"
    val hash = encodeUrl(data)

    handles.get(hash) match {
      case None => handles.addOne((hash -> data)); findCollisionHelper(ix + 1)
      case Some(otherData) => (data, otherData)
    }
  }
  findCollisionHelper(0)
}

/* Found collisions at:
  * 2M strings: ListBuffer((1343617.com,1472600.com))
  * 3M strings: ListBuffer((1343617.com,1472600.com), (314415.com,2235669.com), (1281267.com,2601850.com), (2532640.com,2864459.com))
  *  */
def findCollisionsV2(n: Int): List[(String, String)] = {
  val handles = new HashMap[String, List[String]]
  val collisions = new scala.collection.mutable.ListBuffer[(String, String)]

  @tailrec
  def findCollisionHelper(ix: Long): List[(String, String)] = {
    val data = s"${ix}.com"
    val hash = encodeUrl(data)
    val canStop = ix == n

    (canStop, handles.get(hash)) match {
      case (true, _) => collisions.toList
      case (_, None) => handles.addOne((hash -> List(data))); findCollisionHelper(ix + 1)
      case (_, Some(otherData)) => {
        handles.addOne((hash -> (data +: otherData)))
        otherData.map( collidingData => collisions.addOne((collidingData, data)))
        findCollisionHelper(ix + 1)
      }
    }
  }
  findCollisionHelper(0)
}
val collPairs = List(("4701251","2946054"), ("4441313","1266660"), ("3005482","2443514"), ("2821091","2255261"), 
("2143991","1861715"), ("1343617.com","1472600.com"), ("314415.com","2235669.com"))

def shouldInsertCollidingEntry(database: Database): Unit = {
  val (lhsUrl, rhsUrl) = findCollision()
  shouldInsertNewEntry(lhsUrl, database)
  shouldInsertNewEntry(rhsUrl, database)
}

def collisionsShouldAlreadyExist(database: Database): Unit = {
  val (lhsUrl, rhsUrl) = findCollision()
  val chainHashedValue = encodeUrl(encodeUrl(rhsUrl))
  shouldAlreadyExist(lhsUrl, database)
  shouldAlreadyExist(rhsUrl, database)
  database.lookup(chainHashedValue).contains(rhsUrl)
}

@main 
def runFns(): Unit = {
  val db = new Database(connectToDB())
  clearTable()
  shouldInsertNewEntry("twitter.com",db)
  shouldAlreadyExist("twitter.com", db)
  shouldInsertNewEntry("alibaba.com", db)

  shouldInsertCollidingEntry(db)
  collisionsShouldAlreadyExist(db)
  db.closeConn()
  println("finished")
}