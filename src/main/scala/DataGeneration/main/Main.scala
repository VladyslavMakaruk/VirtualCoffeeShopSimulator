package DataGeneration.main

import DataGeneration.core.{CoffeeShop, QueueMessage, ReceiptWriter, ShutdownSignal}
import DataGeneration.inputGenerator.OrdersGenerator
import DataGeneration.util.TimeFormatter


import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

// scala Test + scala Check
object Main {
  def main(args: Array[String]): Unit = {
    //atomic isRunning set to TRUE
    //share it with producer and consumer
    //start generator
    //start coffeeShop
    //add shutDownHook which sets atomic to false and waits until threads will finish their work
    //join both and wait
    val isRunning = new AtomicBoolean(true)
    val inputDataQueue  = new LinkedBlockingQueue[String](10000)
    val recipeDataQueue = new LinkedBlockingQueue[QueueMessage](10000)

    ReceiptWriter.getValidatedFile(TimeFormatter.getFormattedCurrentDate) match {
      case Success(file) =>
        val writer = ReceiptWriter(recipeDataQueue,file)

        val producer = Future {
          println("[Produced] has started generating orders.")
          val ordersGenerator = OrdersGenerator(0.92, 0.5)
          while (isRunning.get()) {
            val possibleData = Try{
              ordersGenerator.generateAtMostNOrders()
            }
            possibleData match {
              case Success(list) => list.foreach(inputDataQueue.put)
              case Failure(exception) => println(exception)
            }
            Thread.sleep(1000)
          }
        }

        val consumer = Future {
          println("[Consumer] has started handling orders.")
          val ordersParser = CoffeeShop(3, recipeDataQueue)
          while (isRunning.get() || !inputDataQueue.isEmpty || ordersParser.hasActiveWork) {
            Option(inputDataQueue.poll(100, TimeUnit.MILLISECONDS)) match {
              case Some(valueToParse) => ordersParser(valueToParse)
              case _ =>
            }
          }
        }

        sys.addShutdownHook {
          isRunning.set(false)
          Await.ready(producer,Duration.Inf)
          println("[Produced] Stopped generating orders.")
          Await.ready(consumer,Duration.Inf)
          println("[Consumer] Finished processing orders")
          recipeDataQueue.put(ShutdownSignal)
          Await.ready(writer,Duration.Inf)
          println("Process was gracefully finished")
        }
        Await.ready(writer,Duration.Inf)

      case Failure(exception) => {
        println(s"Initialization failed: ${exception.getMessage}")      }
    }
  }
}