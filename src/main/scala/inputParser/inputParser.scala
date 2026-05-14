package inputParser

import domainEntities.*
import util.exceptions.OrderParsingException

// Order: Optional[Discount]-CoffeType-BeansType-Size-Optional[MilkType]-Optional[Toppings]
// discount -> DISCOUNT[code]
// [CoffeType,BeansType,Size,Milk|Toppings,Toppings...]
//    |           |       |    |            |
//    V           V       V    V            V
//  parCofeT   parBean  pSize  pMilk|Topp  pToppings
//

object inputParser {

  def apply(input: String): Either[OrderParsingException, Order] = {
    val tokens = input.strip().split("-").toList

    val (maybeDiscount, coreParts) = tokens match {
      case discountStr :: rest if isDiscount(discountStr) => (Some(discountStr), rest)
      case rest => (None, rest)
    }

    val parsedDiscount = maybeDiscount match {
      case Some(string) =>
        parseDiscount(string) match {
          case Left(orderParsingException) => Left(orderParsingException)
          case Right(discount) => Right(Some(discount))
        }
      case None => Right(None)
    }

    for {
      order <- coreParts match {
        case coffeeStr :: beanStr :: sizeStr :: rest =>
          for {
            coffee <- parseCoffee(coffeeStr)
            beans  <- parseBeans(beanStr)
            size   <- parseSize(sizeStr)
            res    <- buildOrder(parsedDiscount, coffee, beans, size, rest)
          } yield res
        case _ =>
          Left(OrderParsingException("Invalid format: Expected [Discount]-Coffee-Beans-Size-[Milk]-[Toppings]"))
      }
    } yield order
  }

  private def buildOrder(
                          discountIn: Either[OrderParsingException,Option[Discount]],
                          coffee: Coffee,
                          beans: Beans,
                          size: Size,
                          remaining: List[String]
                        ): Either[OrderParsingException, Order] = {
    coffee match {
      case milkCoffee: MilkCoffee =>
        remaining match {
          case milkStr :: toppings =>
            for {
              discount <- discountIn
              milk <- parseMilk(milkStr)
              ts   <- parseToppingsList(toppings)
            } yield discount match {
              case Some(discount) => DiscountMilkCoffeeOrder(discount,milkCoffee,beans, size, milk, ts)
              case None => DefaultMilkCoffeeOrder(milkCoffee,beans, size, milk, ts)
            }
          case Nil =>
            Left(OrderParsingException("Milk coffee requires a milk type specification"))
        }
      case blackCoffee: BlackCoffee =>
        remaining match {
          case toppings =>
            for {
              discount <- discountIn
              ts   <- parseToppingsList(toppings)
            } yield discount match {
              case Some(discount) => DiscountBlackCoffeeOrder(discount,blackCoffee,beans, size, ts)
              case None => DefaultBlackCoffeeOrder(blackCoffee,beans, size, ts)
            }
        }
    }
  }

  private def parseCoffee(input: String): Either[OrderParsingException,Coffee] = {
    Map(
      "latte" -> Latte,
      "espresso" -> Espresso,
      "americano" -> Americano,
      "cappuccino" -> Cappuccino,
      "coldbrew" -> ColdBrew,
      "goldenlatte" -> GoldenLatte
    )
      .get(input.toLowerCase)
      .toRight(OrderParsingException.wrongCoffeeType(input))
  }

  private def parseDiscount(input: String): Either[OrderParsingException,Discount] = {
    val code = input
      .stripPrefix("DISCOUNT[")
      .stripSuffix("]")
    Discount(code).toRight(OrderParsingException.wrongDiscountValue(code))
  }

  private def isDiscount(input: String): Boolean = {
    input.startsWith("DISCOUNT[") && input.endsWith("]")
  }

  private def parseBeans(input: String): Either[OrderParsingException,Beans] = {
    Beans
      .values
      .find(_.toString.equalsIgnoreCase(input))
      .toRight(OrderParsingException.wrongBeanType(input))
  }

  private def parseSize(input: String): Either[OrderParsingException,Size] = {
    Size
      .values
      .find(_.toString.equalsIgnoreCase(input))
      .toRight(OrderParsingException.wrongSizeType(input))
  }

  private def parseMilk(input: String): Either[OrderParsingException,Milk] = {
    Milk
      .values
      .find(_.toString.equalsIgnoreCase(input))
      .toRight(OrderParsingException.wrongMilkType(input))
  }

  private def parseToppings(input: String): Either[OrderParsingException,Topping] = {
    Topping
      .values
      .find(_.toString.equalsIgnoreCase(input))
      .toRight(OrderParsingException.wrongToppingType(input))
  }

  private def parseToppingsList(toppingsList: List[String]): Either[OrderParsingException, List[Topping]] = {
    val results = toppingsList.map(parseToppings)
    val errors = results.collect { case Left(e) => e }
    val toppings = results.collect { case Right(t) => t }
    if errors.isEmpty then
      Right(toppings)
    else
      Left(
        OrderParsingException(errors.map(_.getMessage).mkString("\n"))
      )
  }
}
