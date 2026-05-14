package inputParser
import domainEntities._
import org.scalactic.Prettifier.default
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor2}
import org.scalatest.prop.Tables.Table

import scala.compiletime.constValueTuple
import util.exceptions.OrderParsingException

import scala.deriving.Mirror

class inputParserTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  "Input parser" should "return order" in {
    forAll(ParserTestData.validData) { parserInput =>
      assert({
        inputParser(parserInput) match {
          case Right(order: Order) => true
          case _ => false
        }
      })
    }
  }

  "Input parser" should "return exception" in {
    forAll(ParserTestData.invalidData) { (parserInput, exception) =>
      assert({
        inputParser(parserInput) match {
          case Right(order: Order) => false
          case Left(orderParsingException: OrderParsingException) => orderParsingException `equals` exception
        }
      })
    }
  }
}

object ParserTestData {
  val invalidData: TableFor2[String, OrderParsingException] = Table(
    ("parserInput", "exception"),
    ("Moka-arabica-medium-OatMilk-strawberry", OrderParsingException.wrongCoffeeType("Moka")),
    ("espresso-arabica-medium-OatMilk-strawberry", OrderParsingException.wrongToppingType("OatMilk")),
    ("latte-arabica-medium", OrderParsingException("Milk coffee requires a milk type specification")),
    ("latte-arabica", OrderParsingException("Invalid format: Expected [Discount]-Coffee-Beans-Size-[Milk]-[Toppings]")),
    ("espresso-brazilian-medium-OatMilk-strawberry", OrderParsingException.wrongBeanType("brazilian")),
    ("espresso-arabica-biggest-OatMilk-strawberry", OrderParsingException.wrongSizeType("biggest")),
    ("latte-arabica-medium-cowMilk-strawberry", OrderParsingException.wrongMilkType("cowMilk")),
  )

  val validData: TableFor1[String] = {
    Table("parserInput", validBlackCoffeeOrders++validMilkCoffeeOrders*)
  }

  private def validBlackCoffeeOrders: List[String] = {
    val blackCoffeeMirror = summon[Mirror.SumOf[BlackCoffee]]
    for {
       blackCoffeeType <- constValueTuple[blackCoffeeMirror.MirroredElemLabels].toList.map(_.toString)
       beansType <- Beans.values.map(_.toString)
       sizesType <- Size.values.map(_.toString)
       toppingsType <- Topping.values.map(_.toString)
    } yield s"${blackCoffeeType}-${beansType}-${sizesType}-${toppingsType}"
   }

  private def validMilkCoffeeOrders: List[String] = {
    val milkCoffeeMirror = summon[Mirror.SumOf[MilkCoffee]]
    for {
      milkCoffeeType <- constValueTuple[milkCoffeeMirror.MirroredElemLabels].toList.map(_.toString)
      beansType <- Beans.values.map(_.toString)
      sizesType <- Size.values.map(_.toString)
      milkType <- Milk.values.map(_.toString)
      toppingsType <- Topping.values.map(_.toString)
    } yield s"${milkCoffeeType}-${beansType}-${sizesType}-${milkType}-${toppingsType}"
  }
}