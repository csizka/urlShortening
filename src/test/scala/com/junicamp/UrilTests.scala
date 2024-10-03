package com.junicamp
import utest._
import io.lemonlabs.uri._

object UtilTests extends TestSuite {

  def parseUrlShouldWorkFor(rawUrl: String, expectedOutput: String) = {
    assert(Utils.refactorUrl(rawUrl) == expectedOutput)
  }
  val tests = Tests {

    test("refactoringUrlTests"){
      test("refactorUrl should keep the url_input for absolute URLs"){
          parseUrlShouldWorkFor("http://youtube.com", "http://youtube.com")
      }

      test("refactorUrl should keep the url_input for URLs without authority"){
        parseUrlShouldWorkFor("mailto:example.com", "mailto:example.com")
      }

      test("refactorUrl should add http:// to the url_input of URLs without protocols"){
        parseUrlShouldWorkFor("youtube.com", "http://youtube.com")
      }

      test("refactorUrl should add http: to the url_input of protovol relat iveURLs"){
        parseUrlShouldWorkFor("//youtube.com", "http://youtube.com")
      }
      
    }


  }
}
