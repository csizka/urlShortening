package com.junicamp
import utest._
import io.lemonlabs.uri._

object UtilTests extends TestSuite {

  def parseUrlShouldWorkFor(inputRawUrl: String, expectedRawUrl: String) = {
    val actual = Utils.parseUrl(inputRawUrl)
    val expected = Url.parse(expectedRawUrl)
    assert(actual.contains(expected))
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

      test("refactorUrl should add http: to the url_input of protocol relative URLs"){
        parseUrlShouldWorkFor("//youtube.com", "http://youtube.com")
      }
      
    }


  }
}
