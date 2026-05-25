package DataAnalysis.core
import DataAnalysis.deserializer.ReciptWithDate
import DataGeneration.domainEntities.{Coffee, MilkCoffee, Recipt}
import DataAnalysis.dataEntities.{DailyResourcesConsumption, DailySalesData}

import java.time.LocalDate


object DataAnalyzer {

  def dailyMetrics(inputData: List[ReciptWithDate]):Map[(LocalDate,Coffee),DailySalesData]  = {

    val groupedOrders = inputData
      // group by date and coffeeType
      .groupBy(by => (by.date, by.recipe.coffee))
      // List[RecipeWithDate] => List[Recipe]
      .map { case ((date, coffee), data) =>
        ((date, coffee), data.map(_.recipe))
      }

    groupedOrders.map {
      case ((date, coffee), ordersList) => { // ordersList = [recipe[]]
        val totalOrdersCount = ordersList.size
        val totalRevenue = ordersList.map {
          order =>
            order.priceAfterDiscount match {
              case None => order.price
              case Some(value) => value
            }
        }.sum
        val percentageOfOrdersWithDiscount = (ordersList.count(_.discount.nonEmpty) * 1.0 / totalOrdersCount) * 100
        (date, coffee) -> DailySalesData(totalOrdersCount, totalRevenue, percentageOfOrdersWithDiscount)
      }

    }
  }
  
  def resourcesConsumption(inputData: List[ReciptWithDate]):Map[LocalDate,DailyResourcesConsumption] = {
    val groupedOrders = inputData.groupBy(_.date).map(kv => (kv._1,kv._2.map(_.recipe)))

    groupedOrders.map{
      case (date,ordersList) =>
        val beansUsageMap = ordersList.groupBy(_.beans).map{
          beansMap => (beansMap._1,beansMap._2.map{
            recipe => recipe.coffee.getCoffeeVolume(recipe.size)
          }.sum)
        }

        val milkUsageMap = ordersList
          .filter(_.milk.nonEmpty)
          .groupBy(_.milk.get)
          .map{
            milkMap => (milkMap._1,milkMap._2.map{
              recipe => recipe.coffee match {
                case milkCoffee: MilkCoffee =>
                  milkCoffee.getMilkVolume(recipe.size)
                case _ => 0
              }
            }.sum)
          }

        val toppingsUsage = ordersList
          .filter(_.topping.nonEmpty)
          .flatMap(_.topping)
          .groupBy(identity)
          .map((k, v) => (k, v.size))

        (date,DailyResourcesConsumption(beansUsageMap,milkUsageMap,toppingsUsage))
    }

  }
}
