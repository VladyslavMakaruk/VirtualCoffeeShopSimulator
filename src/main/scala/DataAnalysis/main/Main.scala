package DataAnalysis.main
import DataAnalysis.deserializer.DirectoryReader

import scala.util.{Failure, Success}
import DataAnalysis.core.DataAnalyzer
import DataGeneration.domainEntities.{Milk, Topping, Beans}

object Main {

  /*
    Data analysis tasks:
    1.
        - group by day and coffee type
        - calculate total orders count
        - total revenue (price after discount || price)
        - % of orders with discount
        - average % of discount
        - order price (variance) or sales variance for some period of time
    2.
        - resources consumption
  */
  def main(array: Array[String]): Unit = {
    DirectoryReader.apply() match {
      case Success(listOfRecipts) =>
        val resources = DataAnalyzer.resourcesConsumption(listOfRecipts)
        val metrics = DataAnalyzer.dailyMetrics(listOfRecipts)

        val allDates = (resources.keys ++ metrics.keys.map(_._1)).toSet.toList.sorted

        allDates.foreach { date =>
          println(s"\n=== DASHBOARD REPORT FOR: $date ===")

          resources.get(date).foreach { data =>
            println("\n[ DAILY RESOURCE CONSUMPTION ]")
            println(f"${"Category"}%-15s | ${"Item"}%-15s | ${"Quantity"}%8s")
            println("-" * 44)

            Beans.values.foreach(b => println(f"${"Beans"}%-15s | ${b.toString}%-15s | ${data.beansUsage.getOrElse(b, 0)}%8d"))
            Milk.values.foreach(m => println(f"${"Milk"}%-15s | ${m.toString}%-15s | ${data.milkUsage.getOrElse(m, 0)}%8d"))
            Topping.values.foreach(t => println(f"${"Toppings"}%-15s | ${t.toString}%-15s | ${data.toppingsUsage.getOrElse(t, 0)}%8d"))
          }

          println("\n[ COFFEE SALES METRICS ]")
          println(f"${"Coffee Type"}%-18s | ${"Count"}%6s | ${"Revenue"}%10s | ${"Margin %"}%8s")
          println("-" * 51)

          metrics.filter(_._1._1 == date).foreach { case ((_, coffee), sales) =>
            println(f"${coffee.toString}%-18s | ${sales.totalOrdersCount}%6d | $$${sales.totalRevenue}%9.2f | ${sales.percentageOfOrdersWithDiscount}%7.1f%%")
          }
          println("=" * 51)
        }

      case Failure(exception) =>
        println(s"Error processing receipts: ${exception.getMessage}")
    }
  }
}
