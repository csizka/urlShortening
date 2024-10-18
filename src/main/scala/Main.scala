package com.junicamp

import com.junicamp.DatabasePostgres
import scalatags.Text.all._

object Main extends cask.MainRoutes {
  override def host = "0.0.0.0"
  val db: Database = DatabaseCassandra.makeDatabase()

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
  def shorten(url_input: String): cask.Response[ujson.Obj] = {
  Utils.parseUrl(url_input).fold(
    cask.Response( data = ujson.Obj(
      "error" -> "invalid url",
      ),
      statusCode = 400,
      )
    ){ parsedUrl =>
      val absUrlStr = parsedUrl.toString
      val handle = db.getOrInsertHandle(absUrlStr).fold(identity, identity)
      cask.Response(
        data = ujson.Obj(
          "url" -> absUrlStr,
          "handle" -> handle,
        ),
        statusCode = 200,
      )
    }
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

