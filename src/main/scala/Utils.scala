package com.junicamp

import io.seruco.encoding.base62.Base62
import org.apache.commons.codec.digest.DigestUtils
import io.lemonlabs.uri._
import scala.util.Random
import scala.collection.mutable.HashMap


object Utils {
  private val base62 = Base62.createInstance()
  val collPairs = List(("4701251","2946054"), ("4441313","1266660"), ("3005482","2443514"), ("2821091","2255261"), 
  ("2143991","1861715"), ("1343617.com","1472600.com"), ("314415.com","2235669.com"))

  def encodeUrl(url: String): String = 
    base62.encode(DigestUtils.md5(url)).map(_.toChar).mkString.takeRight(7)

  def refactorUrl(url_input: String): String = {
    val parsedUrl = Url.parse(url_input)
    val finalUrl = parsedUrl match {
      case url: RelativeUrl => "http://" + url_input
      case url: ProtocolRelativeUrl => "http:" + url_input
      case _ => url_input
    }
    finalUrl    
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
      val curHash = Utils.encodeUrl(curElem)
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
    
    def findCollisionHelper(ix: Long): (String, String) = {
      val data = s"${ix}.com"
      val hash = Utils.encodeUrl(data)

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

    def findCollisionHelper(ix: Long): List[(String, String)] = {
      val data = s"${ix}.com"
      val hash = Utils.encodeUrl(data)
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

}
