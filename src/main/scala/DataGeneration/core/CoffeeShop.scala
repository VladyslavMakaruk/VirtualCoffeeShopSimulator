package DataGeneration.core

import DataGeneration.PricingEngine.DefaultPricingEngine
import DataGeneration.domainEntities.{Order, Recipt}
import DataGeneration.inputParser.inputParser
import DataGeneration.util.{CodeGenerator, Serializer}

import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import java.util.concurrent.atomic.AtomicInteger


case class CoffeeShop(
    coffeeMachinesCount: Int,
    parsedDataQueue: LinkedBlockingQueue[QueueMessage]
){
  private val availableMachines = new LinkedBlockingQueue[CoffeeMachine]()
  private val activeOrdersCount = new AtomicInteger(0)

  (1 to coffeeMachinesCount).foreach { i =>
    availableMachines.put(new CoffeeMachine(CodeGenerator()))
  }
  def hasActiveWork: Boolean = activeOrdersCount.get() > 0

  def apply(input: String): Unit = {
    inputParser(input) match {
      case Left(orderParsingException) => println(orderParsingException.getMessage)
      case Right(order) => {
        println(order)
        activeOrdersCount.incrementAndGet()
        processOrder(order).map(Serializer(_)).onComplete { result =>
          try {
            result.foreach { data =>
              parsedDataQueue.put(DataPayload(data))
              println(data)
            }
          } finally {
            activeOrdersCount.decrementAndGet()
          }
        }
      }
    }
  }

  private def processOrder(order: Order): Future[Recipt] = {
    Future[Recipt] {
      val machine = availableMachines.take()
      try {
        machine.brew(order.getBrewingTime)
        Recipt(
          machine.codeOfMachine,
          order,
          DefaultPricingEngine
        )
      } finally {
        availableMachines.put(machine)
      }
    }
  }
}

