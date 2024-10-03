package com.junicamp

import com.junicamp.Database
import com.junicamp.Database.encodeUrl
import scalatags.Text.all._

object Main extends cask.MainRoutes {
  override def host = "0.0.0.0"
  val db = new Database(Database.connectToDB(), "url")

  @cask.get("/")
  def index() = {
    html(
      body (
        h1("URL Shortening"),
        br,
        div(
          form(action := "/shorten", method := "get")(
            label(`for` := "url_input")("Insert the URL here:"),
            br,
            input( `type` := "text", id := "url_input", name:= "url_input"),
            br,
            input( `type` := "submit", value := "Submit"),
          ),
        )
      )
    )
  }

  @cask.get("/shorten")
  def shorten(url_input: String) = {
    val url = Database.addScheme(url_input)
    val handle = db.getOrInsertUrl(url).fold(identity, identity)
    html(
      body (
        h1("URL Shortening"),
        br,
        div(
          h2(s"for URL: ${url} the tiny URL is:"),
          h2(s"'localhost:8080/${handle}'"),
          form(action := "/", method := "get")(
            br,
            input( `type` := "submit", value := "New search"),
          ),
        )
      )
    )
  }

  @cask.get("/:handle")
  def resolveUrl(handle: String) = {
    db.lookup(handle).fold(
      cask.Response(
        data = s"Handle not found: ${handle}",
        statusCode = 404,
      )
    ) { url =>
      cask.Redirect(url)
    }
  }
  
  initialize()
}
