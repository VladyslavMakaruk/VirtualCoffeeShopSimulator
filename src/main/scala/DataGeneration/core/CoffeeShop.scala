package DataGeneration.core

import DataGeneration.PricingEngine.DefaultPricingEngine
import DataGeneration.core.OutputFormat.{CSV, JSON}
import DataGeneration.domainEntities.{Order, Recipt}
import DataGeneration.inputParser.InputParser
import DataGeneration.util.{CodeGenerator, Serializer}
import DataGeneration.util.RecipeFormatter
import DataGeneration.util.CSVFormatter
import java.util.concurrent.{Executors, LinkedBlockingQueue, ThreadFactory}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import java.util.concurrent.atomic.AtomicInteger


case class CoffeeShop(
    coffeeMachinesCount: Int,
    parsedDataQueue: LinkedBlockingQueue[QueueMessage]
){
  private val activeOrdersCount = new AtomicInteger(0)

  private class NamedThreadFactory(prefix: String) extends ThreadFactory {
    override def newThread(runnable: Runnable): Thread = {
      val thread = new Thread(runnable)
      thread.setName(CodeGenerator())
      thread.setDaemon(false)
      thread
    }
  }

  private val customFactory = new NamedThreadFactory("coffee-shop-workers")
  private val executorService = Executors.newFixedThreadPool(coffeeMachinesCount, customFactory)

  implicit val workerPoolContext: ExecutionContext =
    ExecutionContext.fromExecutorService(executorService)

  def hasActiveWork: Boolean = activeOrdersCount.get() > 0

  def handle(input: String,outputFormat: OutputFormat): Unit = {
    InputParser(input) match {
      case Left(orderParsingException) => println(orderParsingException.getMessage)
      case Right(order) => {
        println(order)
        activeOrdersCount.incrementAndGet()
        processOrder(order).map{
            data => Serializer.apply(data)(getRecipeFormatter(outputFormat))
          }
          .onComplete { result =>
          try {
            result.foreach { data =>
              parsedDataQueue.put(DataPayload(data))
              println(data)
            }
          } finally {
            activeOrdersCount.decrementAndGet()
          }
        } (global)
      }
    }
  }

  private def processOrder(order: Order): Future[Recipt] = {
    Future[Recipt] {
      val machine = new CoffeeMachine(Thread.currentThread().getName)
      machine.brew(order.getBrewingTime)
      Recipt(
        machine.codeOfMachine,
        order,
        DefaultPricingEngine
      )
    } (workerPoolContext)
  }

  private def getRecipeFormatter(outputFormat: OutputFormat):RecipeFormatter = {
    outputFormat match {
      case JSON => RecipeFormatter.JSONFormatter
      case CSV => CSVFormatter
    }
  }
}

