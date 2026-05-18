package core
import PricingEngine.DefaultPricingEngine
import domainEntities.{Order, Recipe}
import inputParser.inputParser

import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import util.CodeGenerator
import util.Serializer

class CoffeeShop(coffeeMachinesCount: Int) {
  private val availableMachines = new LinkedBlockingQueue[CoffeeMachine]()
  
  (1 to coffeeMachinesCount).foreach { i =>
    availableMachines.put(new CoffeeMachine(CodeGenerator()))
  }
  
  def ordersProcessing(input: List[String]): Unit = {
    input.map(inputParser(_)).foreach {
      case Left(orderParsingException) => println(orderParsingException.getMessage)
      case Right(order) => processOrder(order).map(Serializer(_)).foreach(println)
    }
  }
  
  
  private def processOrder(order: Order): Future[Recipe] = {
    Future[Recipe]{
      val machine =  availableMachines.take()
      machine.brew(order.getBrewingTime)
      availableMachines.put(machine)
      Recipe(
        machine.codeOfMachine,
        order,
        DefaultPricingEngine
      )
    }
  }
}