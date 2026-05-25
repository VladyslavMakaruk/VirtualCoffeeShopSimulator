package DataAnalysis.deserializer

import DataAnalysis.deserializer.Format.{CSV, JSON}
import DataGeneration.domainEntities.*

import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success, Try}

object DirectoryReader {
  private val validFileNameRegex = "\\d{2}-\\d{2}-\\d{4}\\.(json|csv)"
  private val defaultDirectory = "/Users/vmakaruk/coffeeShopApp/logs"

  /*
    + get directory name
    + return list of valid files
    + transform each file into list of ReciptWithDate
    + combine them

  */

  def apply(folderName: String = "/Users/vmakaruk/coffeeShopApp/logs"): Try[List[ReciptWithDate]] = {
    for {
      listOfFiles <- getFilesFromFolder(folderName)
      masterList  <- {
        val result: Try[List[ReciptWithDate]] = {
          listOfFiles.map(transform).foldRight[Try[List[ReciptWithDate]]](Success(Nil)) {
            (eitherReciptList, eitherAcc) =>
              for {
                subList <- eitherReciptList
                acc     <- eitherAcc
              } yield subList ++ acc
          }
        }
        result
      }
    } yield masterList
  }

  //get list of valid files
  private def getFilesFromFolder(folderPath: String):Try[List[File]] = Try {
    val directory = new File(folderPath)
    if (directory.exists() && directory.isDirectory) {
      val files = directory.listFiles.filter{
        file => file.isFile && isValidFile(file)
      }.toList
      files
    } else {
      throw new IllegalArgumentException("given path does not exist or it's not a folder")
    }
  }

  //files predicate
  private def isValidFile(file: File):Boolean = {
    // validFileFormat = dd-dd-dddd.(json|csv)
    (file.getName.endsWith(".json") || file.getName.endsWith(".csv"))
      &&
      file.getName.matches(validFileNameRegex)
  }


  //transform file to list of orders objects
  private def transform(file: File):Try[List[ReciptWithDate]] = {
    getFileFormat(file).flatMap{
      case JSON => getDateFromFileName(file,JSON).flatMap(JSONDeserializer(file,_))
      case CSV => getDateFromFileName(file,CSV).flatMap(CSVDeserializer(file,_))
    }
  }

  private def getDateFromFileName(file: File, fileFormat: Format): Try[LocalDate] = Try {
    LocalDate.parse(file.getName.dropRight(fileFormat().length),DateTimeFormatter.ofPattern("dd-MM-yyyy"))
  }

  //get file extension
  private def getFileFormat(file: File): Try[Format] = Try {
    val fileName = file.getName
    if fileName.endsWith(JSON()) then JSON
    else if fileName.endsWith(CSV()) then CSV
    else throw IllegalArgumentException(s"invalid file extension - $fileName")
  }
}

enum Format(extension: String):
  def apply():String = extension
  case CSV extends Format(".csv")
  case JSON extends Format(".json")

case class ReciptWithDate(
                           recipe: Recipt,
                           date: LocalDate
                         )