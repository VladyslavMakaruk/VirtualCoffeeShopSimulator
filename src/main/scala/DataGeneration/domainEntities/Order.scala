package DataGeneration.domainEntities

// Order: CoffeType-BeansType-Size-Optional[MilkType]-Optional[Toppings]

sealed abstract class Order(val coffee: Coffee) {
  def getBrewingTime: Int = this.coffee.getCoffeeBrewingTime
}

sealed abstract class DiscountOrder(val discount: Discount, override val coffee: Coffee) extends Order(coffee)


class Discount(amount: Double) {
  def getDiscount: Double = amount
}

trait PricingEngine {
  def countPrice(order: Order): Double
  def countPriceWithDiscount(order: DiscountOrder): Double
}

case class Recipt(
     codeOfMachine: String,
     coffee: Coffee,
     beans: Beans,
     size: Size,
     price: Double,
     priceAfterDiscount: Option[Double],
     topping: List[Topping],
     milk: Option[Milk],
     discount: Option[Discount]
 ) 

object Recipt {
  def apply(
         codeOfMachine: String,
         order: Order,
         pricingEngine: PricingEngine  
  ): Recipt = {
      order match {
        case DefaultBlackCoffeeOrder(coffee, beans, size, topping) => 
          new Recipt(
            codeOfMachine = codeOfMachine,
            coffee = coffee,
            beans = beans,
            size = size,
            price = pricingEngine.countPrice(order), 
            priceAfterDiscount = None,
            topping = topping,
            milk = None,
            discount = None)
          
        case order @ DiscountBlackCoffeeOrder(discount, coffee, beans, size, topping) =>
          new Recipt(
            codeOfMachine = codeOfMachine,
            coffee = coffee,
            beans = beans,
            size = size,
            price = pricingEngine.countPrice(order),
            priceAfterDiscount = Some(pricingEngine.countPriceWithDiscount(order)),
            topping = topping,
            milk = None,
            discount = Some(discount)
          )
          
        case DefaultMilkCoffeeOrder(coffee, beans, size, milk, topping) =>
          new Recipt(
            codeOfMachine = codeOfMachine,
            coffee = coffee,
            beans = beans,
            size = size,
            price = pricingEngine.countPrice(order),
            priceAfterDiscount = None,
            topping = topping,
            milk = Option(milk),
            discount = None
          )
          
        case order @ DiscountMilkCoffeeOrder(discount, coffee, beans, size, milk, topping) =>
          new Recipt(
            codeOfMachine = codeOfMachine,
            coffee = coffee,
            beans = beans,
            size = size,
            price = pricingEngine.countPrice(order),
            priceAfterDiscount = Option(pricingEngine.countPriceWithDiscount(order)),
            topping = topping,
            milk = Option(milk),
            discount = Option(discount)
          )
      }
  }
}



object Discount {
  private  val discountValues: Map[String,Discount] =
    Map(
      "25SUMMER25" -> new Discount(25), 
      "NEWAVE" -> new Discount(15),
      "EASYTOGO" -> new Discount(10) 
    )

  def apply(code: String): Option[Discount] = {
    discountValues.get(code)
  }
}

case class DiscountBlackCoffeeOrder(
  override val discount: Discount,
  override val coffee: BlackCoffee,
  beans: Beans,
  size: Size,
  topping: List[Topping]
) extends DiscountOrder(discount, coffee) 

case class DiscountMilkCoffeeOrder(
   override val discount: Discount,
   override val coffee: MilkCoffee,
   beans: Beans,
   size: Size,
   milk: Milk,
   topping: List[Topping]
 ) extends DiscountOrder(discount, coffee)

case class DefaultBlackCoffeeOrder(
   override val coffee: BlackCoffee,
   beans: Beans,
   size: Size,
   topping: List[Topping]
) extends Order(coffee)

case class DefaultMilkCoffeeOrder(
    override val coffee: MilkCoffee,
    beans: Beans,
    size: Size,
    milk: Milk,
    topping: List[Topping]
) extends Order(coffee)

enum Size:
  case Small
  case Medium
  case Large
  case ExtraLarge

enum Beans(price: Int):
  def getPrice: Int = this.price
  case Arabica extends Beans(2)
  case Robusta extends Beans(3)
  case Liberica extends Beans(4)

enum Milk(price: Int):
  def getPrice: Int = this.price
  case OatMilk extends Milk(3)
  case SkimMilk extends Milk(4)
  case CoconutMilk extends Milk(5)
  case AlmondMilk extends Milk(6)


enum Topping(price: Int):
  def getPrice: Int = this.price
  case Strawberry extends Topping(3)
  case Banana extends Topping(5)
  case Coconut extends Topping(7)


sealed trait Coffee {
  val coffeeBrewingTime:Int
  def getCoffeeBrewingTime: Int = this.coffeeBrewingTime
  val coffeeVolumes: Map[Size,Int]
  def getCoffeeVolume(size: Size):Int = this.coffeeVolumes(size)
}

sealed trait BlackCoffee extends Coffee
sealed trait MilkCoffee extends Coffee {
  val milkVolumes: Map[Size,Int]
  def getMilkVolume(size: Size):Int = this.milkVolumes(size)
}

case object Latte extends MilkCoffee {
  val coffeeBrewingTime: Int = 10000
  val coffeeVolumes: Map[Size,Int] = Map(
    Size.Small -> 10,
    Size.Medium -> 15,
    Size.Large -> 20,
    Size.ExtraLarge -> 25
  )

  val milkVolumes: Map[Size,Int] = Map(
    Size.Small ->  5,
    Size.Medium -> 8,
    Size.Large -> 12,
    Size.ExtraLarge -> 15
  )
}

case object Espresso extends BlackCoffee {
  val coffeeBrewingTime: Int = 10000
  val coffeeVolumes: Map[Size, Int] = Map(
    Size.Small -> 12,
    Size.Medium -> 17,
    Size.Large -> 22,
    Size.ExtraLarge -> 28
  )
}

case object Americano extends BlackCoffee {
  val coffeeBrewingTime: Int = 10000
  val coffeeVolumes: Map[Size, Int] = Map(
    Size.Small -> 8,
    Size.Medium -> 15,
    Size.Large -> 20,
    Size.ExtraLarge -> 25
  )
}

case object Cappuccino extends MilkCoffee {
  val coffeeBrewingTime: Int = 7000
  val coffeeVolumes: Map[Size, Int] = Map(
    Size.Small -> 7,
    Size.Medium -> 15,
    Size.Large -> 20,
    Size.ExtraLarge -> 25
  )

  val milkVolumes: Map[Size, Int] = Map(
    Size.Small -> 9,
    Size.Medium -> 8,
    Size.Large -> 12,
    Size.ExtraLarge -> 15
  )
}

case object ColdBrew extends BlackCoffee {
  val coffeeBrewingTime: Int = 10000
  val coffeeVolumes: Map[Size, Int] = Map(
    Size.Small -> 13,
    Size.Medium -> 15,
    Size.Large -> 20,
    Size.ExtraLarge -> 25
  )
}

case object GoldenLatte extends MilkCoffee {
  val coffeeBrewingTime: Int = 10000
  val coffeeVolumes: Map[Size, Int] = Map(
    Size.Small -> 17,
    Size.Medium -> 20,
    Size.Large -> 24,
    Size.ExtraLarge -> 29
  )

  val milkVolumes: Map[Size, Int] = Map(
    Size.Small -> 7,
    Size.Medium -> 10,
    Size.Large -> 14,
    Size.ExtraLarge -> 15
  )
}




