package com.pokemon.db

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import spray.json._
import DefaultJsonProtocol._

object PokemonDB:
  val cmdBehavior: Map[String, Array[String] => (String, Int)]  =
    Map("GET" -> get, "GETALL" -> getAll, "COUNT" -> count, "MATCH" -> matches,"QUIT" -> quit)

  val pokemonData: List[Map[String, String]] =
    parseCSV("K:\\Programs\\Scala\\PokemonDB\\src\\main\\ressources\\pokemon.csv")

  def main(args: Array[String]): Unit =
    trySomething()
    printBanner()
    mainLoop(1)

  @tailrec
  def mainLoop(status: Int): Unit =
    if(status != 0)
      print("PokemonDB>")
      val expr = readLine()
      val (cmd, args) = parse(expr)
      val (result, newStatus) = exec(cmd, args)
      println(result)
      mainLoop(newStatus)

  def parseCSV(path: String): List[Map[String, String]] =
    val lines = Source.fromFile(path).getLines()
    val headers = lines.next().split(",")

    val result = for {
      line <- lines
      values = line.split(",")
      mapping = headers.zip(values).toMap
    } yield mapping

    result.toList

  def parse(expr: String): (String, Array[String]) =
    val splitExpr = expr.split(" ")
    val cmd = splitExpr(0)
    val args = splitExpr.filter(elm => elm != cmd)

    (cmd.toUpperCase(), args)

  def pokemonToString(pokemon: Map[String, String]): String =
    var result = ""
    pokemon foreach {case (key, value) => result += key + ": " + value + "\n"}
    result

  def pokemonListToString(pokemonList: List[Map[String, String]]): String =
    var result = ""
    pokemonList foreach (x => result += pokemonToString(x) + "-------\n")
    result

  def filterPokemonList(pokemonList: List[Map[String, String]], comparisonElm: String,
                        comparisonFunction: (String,String) => Boolean, comparisonValue: String):  List[Map[String, String]] =
    pokemonList.filter(
      elm => elm.get(comparisonElm) match
        case None => false
        case Some(value) => comparisonFunction(value, comparisonValue)
    )

  def exec(cmd: String, args: Array[String]): (String, Int) =
    val action = cmdBehavior.get(cmd)

    action match
      case None => ("Command Not Found", 1)
      case Some(behavior) => behavior(args)

  def get(args: Array[String]): (String, Int) =
    var pokemonName = ""
    try
      pokemonName = args(0)
    catch
      case _: ArrayIndexOutOfBoundsException => return ("Missing Arguments", 1)

    val result = filterPokemonList(pokemonData, "Name", (elm1, elm2) => elm1 == elm2, pokemonName)
    if(result.isEmpty)
      ("Can't find Pokemon", 1)
    else
      val pokemon = result.head
      (pokemonToString(pokemon), 1)

  def getAll(args: Array[String]): (String, Int) =
    var comparisonCriteria = ""
    var comparisonValue = ""
    var result: List[Map[String, String]] = List()

    try
      comparisonCriteria = args(0)
      comparisonValue = args(1)
    catch
      case _: ArrayIndexOutOfBoundsException => return ("Missing Arguments", 1)

    comparisonCriteria match
      case "t1" => result = filterPokemonList(pokemonData, "Type 1", (elm1, elm2) => elm1 == elm2, comparisonValue)
      case "t2" => result = filterPokemonList(pokemonData, "Type 2", (elm1, elm2) => elm1 == elm2, comparisonValue)
      case "g" => result = filterPokemonList(pokemonData, "Generation", (elm1, elm2) => elm1 == elm2, comparisonValue)
      case "l" => result = filterPokemonList(pokemonData, "Legendary", (elm1, elm2) => elm1 == elm2, comparisonValue)
      case _ => return ("Invalid Arguments", 1)

    if(result.isEmpty)
      ("Can't find Pokemon", 1)

    else
      (pokemonListToString(result), 1)

  def count(args: Array[String]): (String, Int) =
    var pokemonType = ""
    try
      pokemonType = args(0)
    catch
      case _: ArrayIndexOutOfBoundsException => return ("Missing Arguments", 1)
    (filterPokemonList(pokemonData, "Type 1", (elm1, elm2) => elm1 == elm2, pokemonType).size.toString, 1)

  def matches(args: Array[String]): (String, Int) =
    var pokemonNamePattern = ""
    try
      pokemonNamePattern = args(0)
    catch
      case _: ArrayIndexOutOfBoundsException => return ("Missing Arguments", 1)

    val result = filterPokemonList(pokemonData, "Name", (elm1, elm2) => elm1.contains(elm2), pokemonNamePattern)
    if(result.isEmpty)
      ("Can't find Pokemon", 1)
    else
      var resultToString = ""
      result.map(elm => elm.get("Name") match
                                          case None => ""
                                          case Some(name) => name
                                        ) foreach(elm => resultToString += elm + "\n")
      (resultToString, 1)

  def quit(args: Array[String]): (String, Int) =
    ("Bye", 0)

  def printBanner(): Unit =
      println("""         __________       __                                ________ __________
                |         \______   \____ |  | __ ____   _____   ____   ____ \______ \\______   \
                |          |     ___/  _ \|  |/ // __ \ /     \ /  _ \ /    \ |    |  \|    |  _/
                |          |    |  (  <_> )    <\  ___/|  Y Y  (  <_> )   |  \|    `   \    |   \
                |          |____|   \____/|__|_ \\___  >__|_|  /\____/|___|  /_______  /______  /
                |                              \/    \/      \/            \/        \/       \/ """.stripMargin)


def trySomething(): Unit =
  val http = new Http()

  val obj = http.get("https://pokeapi.co/api/v2/pokemon")
    .parseJson
    .asJsObject

def getObj