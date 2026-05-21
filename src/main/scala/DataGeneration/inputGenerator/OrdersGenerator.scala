package DataGeneration.inputGenerator

import CoffeeType.{BlackCoffee, MilkCoffee}
import DataGeneration.domainEntities.{Beans, BlackCoffee, Milk, MilkCoffee, Size, Topping}

import scala.compiletime.constValueTuple
import scala.deriving.Mirror
import scala.util.Random

// Order: Optional[Discount]-CoffeeType-BeansType-Size-Optional[MilkType]-Optional[Toppings]
// discount -> DISCOUNT[code]

case class OrdersGenerator(percentageOfValidValues: Double, percentageOfBlackCoffeeOrders: Double) {
  if percentageOfValidValues <= 0.0 || percentageOfValidValues > 1.0 then throw IllegalArgumentException("valid values percentage should be in (0,1] frame")
  if percentageOfBlackCoffeeOrders <= 0.0 || percentageOfBlackCoffeeOrders >= 1.0 then throw IllegalArgumentException("black coffee percentage should be in (0,1) frame")
  //beansVal
  private given listOfBeans:Array[Beans] = Beans.values
  //coffeeVal
  private val milkCoffeeMirror = summon[Mirror.SumOf[MilkCoffee]]
  private val listOfMilkCoffees = constValueTuple[milkCoffeeMirror.MirroredElemLabels].asInstanceOf[Product].productIterator.map(_.toString).toList
  private val blackCoffeeMirror = summon[Mirror.SumOf[BlackCoffee]]
  private val listOfBlackCoffees = constValueTuple[blackCoffeeMirror.MirroredElemLabels].asInstanceOf[Product].productIterator.map(_.toString).toList
  //discountVal
  private val listOfValidDiscounts = List("25SUMMER25", "NEWAVE", "EASYTOGO")
  private val discountTemplate = "DISCOUNT[%s]"
  //milkVal
  private given listOfMilk: Array[Milk] = Milk.values
  //ToppingVal
  private given listOfTopping: Array[Topping] = Topping.values
  //sizeVal
  private given listOfSize: Array[Size] = Size.values

  def generateAtMostNOrders(maxCount:Int = 20): List[String] = {
    List.fill(rndmInt(maxCount))(generateOrder)
  }

  private def generateOrder: String = {
    if Random.nextDouble() < percentageOfBlackCoffeeOrders then generateBlackCoffeeOrder else generateMilkCoffeeOrder
  }

  private def generateBlackCoffeeOrder:String = {
    s"$getDiscount-${getCoffeeType(BlackCoffee)}-${getValue[Beans]}-${getValue[Size]}${getToppings()}"
  }

  private def generateMilkCoffeeOrder:String = {
    s"$getDiscount-${getCoffeeType(MilkCoffee)}-${getValue[Beans]}-${getValue[Size]}-${getValue[Milk]}${getToppings()}"
  }

  private def getDiscount:String = {
    if Random.nextDouble() < percentageOfValidValues
      then discountTemplate.format(listOfValidDiscounts(Random.nextInt(listOfValidDiscounts.size)))
      else discountTemplate.format(getRandomString()).mkString
  }

  private def getCoffeeType(coffeeType: CoffeeType):String = {
    if Random.nextDouble() < percentageOfValidValues then
      coffeeType match {
        case CoffeeType.BlackCoffee =>
          listOfBlackCoffees(Random.nextInt(listOfBlackCoffees.length))
        case CoffeeType.MilkCoffee =>
          listOfMilkCoffees(Random.nextInt(listOfMilkCoffees.length))
      }
    else getRandomString()
  }

  private def getToppings(size: Int = 4):String = {
    val toppings = List.fill(Random.nextInt(size))(getValue[Topping]).mkString("-")
    if toppings.isEmpty then "" else s"-$toppings"
  }

  private def getValue[A](using listOfValue: Array[A]):String = {
    if Random.nextDouble() < percentageOfValidValues then
      listOfValue.map(_.toString).apply(Random.nextInt(listOfValue.length))
    else
      getRandomString()
  }

  private def getRandomString(size: Int = 5): String = {
    val letters = 'A' to 'Z'
    List.fill(size)(letters(Random.nextInt(letters.length))).mkString
  }

  private def rndmInt(upperBound: Int): Int = {
    if upperBound <= 1 then 1
    else {
      val res = Random.nextInt(upperBound)
      if res == 0 then upperBound else res
    }
  }
}

private enum CoffeeType:
  case BlackCoffee, MilkCoffee
