package DataGeneration.core

import OutputFormat.JSON
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import java.util.concurrent.LinkedBlockingQueue
import scala.util.{Try, Using}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

sealed trait QueueMessage
case class DataPayload(data: String) extends QueueMessage
case object ShutdownSignal extends QueueMessage

case object ReceiptWriter {
  def getValidatedFile(fileName: String, filePath: String = "/Users/vmakaruk/coffeeShopApp/logs", fileFormat: OutputFormat = JSON): Try[File] = {
    Try {
      val fullPath = Paths.get(filePath, fileName+fileFormat())
      val parentDir = fullPath.getParent
      if (parentDir != null && !Files.exists(parentDir)) {
        Files.createDirectories(parentDir)
      }
      val file = fullPath.toFile
      if (file.exists() && !file.canWrite) {
        throw new SecurityException(s"File exists but is read-only: ${file.getAbsolutePath}")
      }
      file
    }
  }

  def apply(queue: LinkedBlockingQueue[QueueMessage], file: File): Future[Unit] = {
    Future {
      Using(new BufferedWriter(new FileWriter(file,true))) { writer =>
        @scala.annotation.tailrec
        def loop(): Unit =
          queue.take() match {
            case DataPayload(payload) =>
              writer.write(payload)
              writer.newLine()
              loop()
            case ShutdownSignal =>
              ()
          }
        loop()
      }
    }
  }
  
}

enum OutputFormat(val fileExtension: String):
  def apply():String = fileExtension
  case JSON extends OutputFormat(".json")
  //add more outputFormats
