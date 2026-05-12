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
                                //Either [Int,Exception]
  def countPrice(order: Order): Int = {
    order match {
      case BlackCoffeeOrder(coffee,beans,size,topping) => {
        coffeePrice(coffee,beans,size) + toppingsPrice(topping)
      }
      case MilkCoffeeOrder(coffee,beans,size,milk,topping) => {
        coffeePrice(coffee,beans,size) + milkPrice(coffee, size, milk) +  toppingsPrice(topping)
      }
    }
  }
}
