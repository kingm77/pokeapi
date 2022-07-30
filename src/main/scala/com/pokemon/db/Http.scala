package com.pokemon.db


import monix.eval.Task
import okhttp3.OkHttpClient
import okhttp3.Request.Builder
import okhttp3.{Request, Response}
import scala.util.control.NonFatal

class Http {

  private val client = new OkHttpClient

  private def encodeUrl(url: String, queryParams: Map[String, String]): String = {
    if (queryParams.isEmpty) url
    else {
      val encodedQueryParams = queryParams.map {
        case (key, value) => s"$key=$value"
      }.mkString("&")
      s"$url?$encodedQueryParams"
    }
  }

  def get(url: String, queryParams: Map[String, String] = Map()): String = {
    val request = new Request.Builder()
      .url(encodeUrl(url, queryParams))
      .build()
    var resp: Response = null

    try {
      resp = client.newCall(request).execute()
      resp.body.string()
    } catch {
      case NonFatal(ex) =>
        throw ex
    } finally {
      resp.close()
    }
  }
}

class AsyncHttp {
  private val client = new OkHttpClient

  private def encodeUrl(url: String, queryParams: Map[String, String]): String = {
    if (queryParams.isEmpty) url
    else {
      val encodedQueryParams = queryParams.map {
        case (key, value) => s"$key=$value"
      }.mkString("&")
      s"$url?$encodedQueryParams"
    }
  }

  def get(url: String, queryParams: Map[String, String] = Map()): Task[String] =
    Task {
      val request = new Request.Builder()
        .url(encodeUrl(url, queryParams))
        .build()

      client.newCall(request).execute()
    }.bracket {
      response => Task(response.body.string)
    } {
      response => Task(response.close())
    }
}

