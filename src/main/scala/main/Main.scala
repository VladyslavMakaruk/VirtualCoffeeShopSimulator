package main
import core.CoffeeShop
import inputParser.*

// scala Test + scala Check
object Main {
  def main(args: Array[String]): Unit = {
    new CoffeeShop(2).ordersProcessing(
      List(
        "latte-arabica-medium-OatMilk-strawberry",
        "latte-arabica-medium-OatMilk-strawberry",
        "cappuccino-arabica-medium-OatMilk-strawberry-banana",
        "espresso-arabica-medium-Banana-strawberry",
        "espresso-arabica-medium-Banana-strawberry",
        "espresso-arabica-biggest-OatMilk-strawberry",
        "espresso-arabica-biggest-OatMilk-strawberry",
      )
    )
    Thread.sleep(30000)
  }
}