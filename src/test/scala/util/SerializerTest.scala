package util

import DataGeneration.domainEntities.Beans.Arabica
import DataGeneration.domainEntities.{Americano, Order, Recipt}
import DataGeneration.domainEntities.Size.Large
import DataGeneration.util.Serializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor2}
import org.scalatest.prop.Tables.Table
import io.circe.Json
import org.scalatest.matchers.should.Matchers.{should, shouldBe}

class SerializerTest extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  "Serializer" should "return json value" in {
    forAll(SerializerTestData.jsonData) { (recipe,jsonValue) =>
      Serializer(recipe) shouldBe jsonValue
    }
  }
}

object SerializerTestData {
  val jsonData: TableFor2[Recipt,String] = {
    Table(
      ("recipe","serialized value"),
      (new Recipt(
        codeOfMachine = "test",
        coffee = Americano,
        beans = Arabica,
        size = Large,
        price = 10.0,
        priceAfterDiscount = None,
        topping = List(),
        milk = None,
        discount = None
      ), Json.fromFields(List(
        "machine_code" -> Json.fromString("test"),
        "coffee" -> Json.fromString(Americano.toString),
        "beans" -> Json.fromString(Arabica.toString),
        "size" -> Json.fromString(Large.toString),
        "price" -> Json.fromDoubleOrNull(10.0)
        
      )).toString),
    )
  }
}