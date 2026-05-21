package DataGeneration.PricingEngine

import DataGeneration.domainEntities.{Beans, Coffee, DefaultBlackCoffeeOrder, DefaultMilkCoffeeOrder, DiscountBlackCoffeeOrder, DiscountMilkCoffeeOrder, DiscountOrder, Milk, MilkCoffee, Order, PricingEngine, Size, Topping}


object DefaultPricingEngine extends PricingEngine {
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
        coffeePrice(coffee, beans, size) + toppingsPrice(topping)
      }
      case DiscountBlackCoffeeOrder(_, coffee, beans, size, topping) => {
        coffeePrice(coffee, beans, size) + toppingsPrice(topping)
      }
      case DefaultMilkCoffeeOrder(coffee, beans, size, milk, topping) => {
        coffeePrice(coffee, beans, size) + milkPrice(coffee, size, milk) + toppingsPrice(topping)
      }
      case DiscountMilkCoffeeOrder(_, coffee, beans, size, milk, topping) => {
        coffeePrice(coffee, beans, size) + milkPrice(coffee, size, milk) + toppingsPrice(topping)
      }
    }
  }
                                                  //replace with BIG type
  def countPriceWithDiscount(order: DiscountOrder): Double = {
    countPrice(order)/100.0*(100.0 - order.discount.getDiscount)
  }
}