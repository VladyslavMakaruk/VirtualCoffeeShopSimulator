package DataGeneration.util

import DataGeneration.domainEntities.Recipt

import java.util.Locale


/*
  "machine_code","coffee","beans","size","price","price_after_discount","toppings","milk","discount"
*/

object CSVFormatter extends RecipeFormatter {
  override def format(recipt: Recipt): String = {
    List(
      recipt.codeOfMachine,
      recipt.coffee.toString,
      recipt.beans.toString,
      recipt.size.toString,
      formatDouble(recipt.price),
      recipt.priceAfterDiscount.map(formatDouble).getOrElse(""),
      recipt.topping.map(_.toString).mkString("|"),
      recipt.milk.map(_.toString).getOrElse(""),
      recipt.discount.map(_.getDiscount.toString).getOrElse("")
    )
  }.mkString(",")

  private def formatDouble(d: Double): String = {
    String.format(Locale.US, "%.2f", d)
  }
}
