package DataGeneration.core

import DataGeneration.domainEntities.Order
import scala.concurrent.Future

class CoffeeMachine(val codeOfMachine: String){
  def brew(time: Int):Unit = {
    println(s"${codeOfMachine} is starting")
    Thread.sleep(time)
    println(s"${codeOfMachine} has finished brewing")
  }
}
