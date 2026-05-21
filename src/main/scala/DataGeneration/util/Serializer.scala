package DataGeneration.util

import DataGeneration.domainEntities.Recipt
import io.circe.Encoder.encodeString
import io.circe.{Encoder, Json}
import io.circe.syntax.*

trait RecipeFormatter {
  def format(recipe: Recipt): String
}

object RecipeFormatter {
  given JSONFormatter: RecipeFormatter with {
    given recipeEncoder: Encoder[Recipt] = Encoder.instance { r =>
      val fields: List[(String, Json)] = List(
        Some("machine_code" -> r.codeOfMachine.asJson),
        Some("coffee" -> r.coffee.toString.asJson),
        Some("beans" -> r.beans.toString.asJson),
        r.milk.map("milk" -> _.toString.asJson),
        Some("size" -> r.size.toString.asJson),
        Some("price" -> r.price.asJson),
        r.priceAfterDiscount.map("price_after_discount" -> _.asJson),
        if r.topping.nonEmpty then Some("toppings" -> r.topping.map(_.toString).asJson) else None,
        r.discount.map("discount" -> _.getDiscount.asJson),
      ).flatten

      Json.fromFields(fields)
    }

    override def format(recipe: Recipt): String =
      recipeEncoder(recipe).noSpaces
  }
}

object Serializer {
  def apply(recipe: Recipt)(using formatter: RecipeFormatter): String = {
    formatter.format(recipe)
  }
}


