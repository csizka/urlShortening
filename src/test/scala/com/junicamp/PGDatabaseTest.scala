import utest._
import com.junicamp._
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import java.sql.{Array => SqlArray, *}
import org.apache.commons.codec.digest.DigestUtils
import io.seruco.encoding.base62.Base62
import scala.util.Random


object PGUrlShorteningTests extends TestSuite{
  val random = new Random(42)

  def withTable (action: Database => Unit): Unit = {
    DatabasePostgres.withDatabase { db => 
      db.truncateUrlTable()
      action(db)
    }
  }

  val tests =  Tests{

    test("insertionTests"){
      test("When inserting a new url, the result handle should point to the original url in a Right"){
        withTable { db =>
          val url = "alibaba.com"
          val getHandleRes = db.getOrInsertHandle(url)
          assertMatch(getHandleRes){
            case Right(newHandle: String) if db.lookup(newHandle).contains(url) => ()
          }
        }
      }

      test("When inserting a url that already exists in the table, the result handle should point to the original url in a Left"){
        withTable { db =>  
          val url = "twitter.com"
          db.getOrInsertHandle(url)
          val getHandleRes = db.getOrInsertHandle(url)
           assertMatch(getHandleRes){
            case Left(oldHandle: String) if db.lookup(oldHandle).contains(url) => ()
          }
        }
      }

      test("When inserting new entries that would have the same handle, the second one's handle should be hashed again, to avoid collision, both should be returned in Rights"){
       withTable { db =>  
          val (lhsUrl, rhsUrl) = ("4701251","2946054")
          Utils.encodeUrl(lhsUrl) ==> Utils.encodeUrl(rhsUrl)
          val getLhsHandleRes = db.getOrInsertHandle(lhsUrl)
          val getRhsHandleRes = db.getOrInsertHandle(rhsUrl)
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
          Utils.encodeUrl(lhsUrl) ==> Utils.encodeUrl(rhsUrl)
          db.getOrInsertHandle(lhsUrl)
          db.getOrInsertHandle(rhsUrl)
          val getLhsHandleRes = db.getOrInsertHandle(lhsUrl)
          val getRhsHandleRes = db.getOrInsertHandle(rhsUrl)

          getRhsHandleRes ==> Left(Utils.encodeUrl(Utils.encodeUrl(lhsUrl)))
          getLhsHandleRes ==> Left(Utils.encodeUrl(rhsUrl))
        }
      }
    }
  }
}
