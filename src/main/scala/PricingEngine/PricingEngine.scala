package PricingEngine
import domainEntities._

object PricingEngine {
  private def coffeePrice(coffee: Coffee,beans: Beans,size: Size): Int = {
    coffee.getCoffeeVolume(size) * beans.getPrice
  } 
  
  private def milkPrice(coffee: MilkCoffee,size: Size,milk: Milk): Int = {
    coffee.getMilkVolume(size) * milk.getPrice
  }
  
  private def toppingsPrice(toppings: Seq[Topping]): Int = {
    toppings.map(_.getPrice).sum
  }
  
  def countPrice(order: Order): Double = {
    order match {
      case DefaultBlackCoffeeOrder(coffee, beans, size, topping) => {
        coffeePrice(coffee,beans,size) + toppingsPrice(topping)
      }
      case DiscountBlackCoffeeOrder(discount, coffee, beans, size, topping) => {
        (coffeePrice(coffee,beans,size) + toppingsPrice(topping))
          / 100.0 * (100 - discount.getDiscount)
      }
      case DefaultMilkCoffeeOrder(coffee, beans, size, milk, topping) => {
        coffeePrice(coffee,beans,size) + milkPrice(coffee, size, milk) +  toppingsPrice(topping)
      }
      case DiscountMilkCoffeeOrder(discount, coffee, beans, size, milk, topping) => {
        (coffeePrice(coffee, beans, size) + milkPrice(coffee, size, milk) + toppingsPrice(topping))
         / 100.0 * (100 - discount.getDiscount) 
      }
    }
  }
}
