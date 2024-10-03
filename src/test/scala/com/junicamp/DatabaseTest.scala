import utest._
import com.junicamp._
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import java.sql.{Array => SqlArray, *}
import org.apache.commons.codec.digest.DigestUtils
import io.seruco.encoding.base62.Base62
import scala.util.Random

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
    val curHash = Database.encodeUrl(curElem)
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
    val hash = Database.encodeUrl(data)

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
    val hash = Database.encodeUrl(data)
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


object UrlShorteningTests extends TestSuite{
  val random = new Random(42)
  
  def clearTable(conn: Connection, tableName: String): Unit = {
    val query = s"TRUNCATE TABLE ${tableName};"
    val statement = conn.prepareStatement(query)
    statement.executeUpdate()
    statement.close()
  }

  def withTable (action: Database => Unit): Unit = {
    val tableId = math.abs(random.nextInt())
    val tableName = s"url_test_${tableId}"
    Database.withDatabase (tableName) { db => 
      val createQuery = s"CREATE TABLE IF NOT EXISTS ${tableName} (handle VARCHAR(7) PRIMARY KEY, url VARCHAR(2000))" 
      val createStatement = db.conn.prepareStatement(createQuery)
      try {createStatement.executeUpdate()} catch { case e: Throwable => println(e)}
      createStatement.close()
      action(db)
      val dropQuery = s"DROP TABLE IF EXISTS ${tableName}"
      val dropStatement = db.conn.prepareStatement(dropQuery)
      dropStatement.executeUpdate()
      dropStatement.close()
    }
  }

  val tests =  Tests{

    test("insertionTests"){
      test("When inserting a new url, the result handle should point to the original url in a Right"){
        withTable { db =>
          val url = "alibaba.com"
          val getHandleRes = db.getOrInsertUrl(url)
          assertMatch(getHandleRes){
            case Right(newHandle: String) if db.lookup(newHandle).contains(url) => ()
          }
        }
      }

      test("When inserting a url that already exists in the table, the result handle should point to the original url in a Left"){
        withTable { db =>  
          val url = "twitter.com"
          db.getOrInsertUrl(url)
          val getHandleRes = db.getOrInsertUrl(url)
           assertMatch(getHandleRes){
            case Left(oldHandle: String) if db.lookup(oldHandle).contains(url) => ()
          }
        }
      }

      test("When inserting new entries that would have the same handle, the second one's handle should be hashed again, to avoid collision, both should be returned in Rights"){
       withTable { db =>  
          val (lhsUrl, rhsUrl) = ("4701251","2946054")
          Database.encodeUrl(lhsUrl) ==> Database.encodeUrl(rhsUrl)
          val getLhsHandleRes = db.getOrInsertUrl(lhsUrl)
          val getRhsHandleRes = db.getOrInsertUrl(rhsUrl)
          assertMatch(getLhsHandleRes){
            case Right(newHandle: String) if db.lookup(newHandle).contains(lhsUrl) => ()
          }
          assertMatch(getRhsHandleRes){
            case Right(newHandle: String) if db.lookup(newHandle).contains(rhsUrl) => ()
          }
        }
      }

      test("When inserting entries that would have the same handle, and are in the table the second one's handle should be hashed again, to avoid collision, both should be returned in Lefts"){
        withTable { db =>  
          val (lhsUrl, rhsUrl) = ("4701251","2946054")
          Database.encodeUrl(lhsUrl) ==> Database.encodeUrl(rhsUrl)
          db.getOrInsertUrl(lhsUrl)
          db.getOrInsertUrl(rhsUrl)
          val getLhsHandleRes = db.getOrInsertUrl(lhsUrl)
          val getRhsHandleRes = db.getOrInsertUrl(rhsUrl)
          
          val actualUrls = for {
            lhsHandle <- getLhsHandleRes.toOption
            rhsHandle <- getRhsHandleRes.toOption
            lhsUrlFromDb <- db.lookup(lhsHandle)
            rhsUrlFromDb <- db.lookup(rhsHandle)
          } yield (lhsUrl, rhsUrl)

          actualUrls.contains(lhsUrl -> rhsUrl)     
        }
      }
    }
  }
}
