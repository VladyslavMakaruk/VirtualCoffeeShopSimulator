package inputParser

import domainEntities._
import util.exceptions.OrderParsingException

// Order: CoffeType-BeansType-Size-Optional[MilkType]-Optional[Toppings]

// [CoffeType,BeansType,Size,Milk|Toppings,Toppings...]
//    |           |       |    |            |
//    V           V       V    V            V
//  parCofeT   parBean  pSize  pMilk|Topp  pToppings
//

object inputParser {

  def parseOrder(input: String): Either[OrderParsingException, Order] = {
    val orderParts = input.strip().split("-").toList
    orderParts match {
      case coffeeStr :: beanStr :: sizeStr :: rest =>
        for {
          coffee  <- parseCoffee(coffeeStr)
          beans   <- parseBeans(beanStr)
          size    <- parseSize(sizeStr)
          order   <- coffee match {
            case milkCoffee: MilkCoffee =>
              rest match {
                case milkStr :: toppingStrs =>
                  for {
                    milk     <- parseMilk(milkStr)
                    toppings <- parseToppingsList(toppingStrs)
                  } yield MilkCoffeeOrder(milkCoffee, beans, size, milk, toppings)
                case Nil =>
                  Left(OrderParsingException("milk coffee requires milk type"))
              }
            case blackCoffee: BlackCoffee =>
              parseToppingsList(rest)
                .map(toppings => BlackCoffeeOrder(blackCoffee, beans, size, toppings))
          }
        } yield order
      case _ =>
        Left(OrderParsingException("invalid order format"))
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
        OrderParsingException.wrongToppingType(errors.map(_.getMessage).mkString("\n"))
      )
  }
}
