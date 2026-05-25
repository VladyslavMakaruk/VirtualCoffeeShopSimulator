package DataAnalysis.dataEntities
import DataGeneration.domainEntities._

case class DailyResourcesConsumption(
  beansUsage: Map[Beans, Int],
  milkUsage: Map[Milk, Int],
  toppingsUsage: Map[Topping, Int]
) 