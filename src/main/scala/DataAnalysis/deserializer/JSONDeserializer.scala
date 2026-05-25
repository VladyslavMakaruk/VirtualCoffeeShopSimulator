package DataAnalysis.deserializer

import DataGeneration.domainEntities.*
import io.circe.parser.parse
import io.circe.{DecodingFailure, Json}

import scala.io.Source
import java.io.File
import java.time.LocalDate
import scala.util.{Try, Using,Success,Failure}

object JSONDeserializer {

  def apply(file: File, date: LocalDate): Try[List[ReciptWithDate]] = {
    for {
      inputList <- Using(Source.fromFile(file))(_.getLines().toList)
      parsedList <- Try {
        val eitherList: List[Either[Exception, Recipt]] = inputList.map(transformOrder)
        val sequenceEither: Either[Exception, List[Recipt]] =
          eitherList.foldRight[Either[Exception, List[Recipt]]](Right(Nil)) { (eitherRecipt, eitherAcc) =>
            for {
              recipt <- eitherRecipt
              acc    <- eitherAcc
            } yield recipt :: acc
          }
        sequenceEither match {
          case Right(receipts) => receipts.map(ReciptWithDate(_, date))
          case Left(exception) => throw exception
        }
      }
    } yield parsedList
  }

  private def transformOrder(inputValue: String): Either[Exception, Recipt] = {
    for {
      json   <- parse(inputValue)
      recipt <- JSONtoReceipt(json)
    } yield recipt
  }

  private def transformStringToCoffee(input: String): Either[Exception, Coffee] = {
    Map(
      "latte"       -> Latte,
      "espresso"    -> Espresso,
      "americano"   -> Americano,
      "cappuccino"  -> Cappuccino,
      "coldbrew"    -> ColdBrew,
      "goldenlatte" -> GoldenLatte
    )
      .get(input.toLowerCase)
      .toRight(new IllegalArgumentException(s"Invalid coffee type: '$input'."))
  }

  private def JSONtoReceipt(json: Json): Either[Exception, Recipt] = {
    val cursor = json.hcursor
    
    for {
      machine_code       <- cursor.downField("machine_code").as[String]
      coffee             <- cursor.downField("coffee").as[String].flatMap(transformStringToCoffee)

      beans              <- cursor.downField("beans").as[String].flatMap { bi =>
        Beans.values.find(_.toString.equalsIgnoreCase(bi)).toRight(DecodingFailure(
          s"Invalid bean type: '$bi'. Allowed values: ${Beans.values.mkString(", ")}",
          cursor.history
        ))
      }

      size               <- cursor.downField("size").as[String].flatMap { si =>
        Size.values.find(_.toString.equalsIgnoreCase(si)).toRight(DecodingFailure(
          s"Invalid size type: '$si'. Allowed values: ${Size.values.mkString(", ")}",
          cursor.history
        ))
      }

      price              <- cursor.downField("price").as[Double]
      priceAfterDiscount <- cursor.downField("price_after_discount").as[Option[Double]]
        // as[Option[List[String]] always returns Right(Option[List[String]] or None)
      toppings <- cursor.downField("toppings").as[Option[List[String]]].flatMap{
        case Some(listOfString) => {
          val eitherListOfToppings =  Try { listOfString.map(strTopping => Topping.values.find(_.toString.equalsIgnoreCase(strTopping)).get) }
          eitherListOfToppings match {
            case Success(listOfToppings) => Right(listOfToppings)
            case Failure(exception) => Left(new IllegalStateException("unknown topping name"))
          }
        }
        case None => Right(Nil)
      }

      milk               <- cursor.downField("milk").as[Option[String]].flatMap {
        case Some(milkStr) =>
          Milk.values
            .find(_.toString.equalsIgnoreCase(milkStr))
            .map(Some(_))
            .toRight(DecodingFailure(
              s"Invalid milk type: '$milkStr'. Allowed values: ${Milk.values.mkString(", ")}",
              cursor.history
            ))
        case None =>
          Right(None)
      }

      discount           <- cursor.downField("discount").as[Option[Double]].flatMap {
        case Some(value) => Right(Some(new Discount(value)))
        case None        => Right(None)
      }

    } yield {
      Recipt(
        codeOfMachine      = machine_code,
        coffee             = coffee,
        beans              = beans,
        size               = size,
        price              = price,
        priceAfterDiscount = priceAfterDiscount,
        topping            = toppings,
        milk               = milk,
        discount           = discount
      )
    }
  }
}