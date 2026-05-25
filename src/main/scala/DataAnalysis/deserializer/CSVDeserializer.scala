package DataAnalysis.deserializer

import java.io.File
import java.time.LocalDate
import scala.io.Source
import scala.util.{Try, Using}
import DataGeneration.domainEntities._

object CSVDeserializer {

  def apply(file: File, date: LocalDate): Try[List[ReciptWithDate]] = {
    for {
      inputList <- Using(Source.fromFile(file))(_.getLines().toList)
      tryList   = inputList.map(CSVtoReceipt)
      receipts  <- tryList.foldLeft(Try(List.empty[Recipt])) {
        (acc, currentTry) =>
          for {
            accumulatedList <- acc
            currentElement <- currentTry
          } yield accumulatedList :+ currentElement
       }
    } yield {
      receipts.map(receipt => ReciptWithDate(receipt, date))
    }
  }
  
  private def CSVtoReceipt(csvRow: String): Try[Recipt] = Try {
    val tokens = csvRow.split(",", -1).map(_.trim)
    val machineCode = tokens(0)

    val coffee = transformStringToCoffee(tokens(1)).getOrElse(
      throw new IllegalArgumentException(s"Unknown coffee type: ${tokens(1)}")
    )

    val beans = Beans.values.find(_.toString.equalsIgnoreCase(tokens(2))).get
    val size = Size.values.find(_.toString.equalsIgnoreCase(tokens(3))).get

    val price = parseCleanDouble(tokens(4))
    val priceAfterDiscount = if (tokens(5).isEmpty) None else Some(parseCleanDouble(tokens(5)))

    val toppings = if (tokens(6).isEmpty) List.empty[Topping]
    else tokens(6).split("\\|").map(value => Topping.values.find(_.toString.equalsIgnoreCase(value)).get).toList

    val milk = if (tokens(7).isEmpty) None else Milk.values.find(_.toString.equalsIgnoreCase(tokens(7)))
    val discount = if (tokens(8).isEmpty) None else Some(new Discount(parseCleanDouble(tokens(8))))

    new Recipt(machineCode, coffee, beans, size, price, priceAfterDiscount, toppings, milk, discount)
  }

  private def transformStringToCoffee(input: String): Option[Coffee] = {
    Map(
      "latte" -> Latte,
      "espresso" -> Espresso,
      "americano" -> Americano,
      "cappuccino" -> Cappuccino,
      "coldbrew" -> ColdBrew,
      "goldenlatte" -> GoldenLatte
    ).get(input.toLowerCase)
  }

  private def parseCleanDouble(s: String): Double = {
    s.trim.replace(",", ".").toDouble
  }
}