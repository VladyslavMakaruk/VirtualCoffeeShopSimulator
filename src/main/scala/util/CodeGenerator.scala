package util
import scala.util.Random

object CodeGenerator {
  def apply(): String = {
    val letters = 'A' to 'Z'
    val digits = '0' to '9'

    def pick(source: IndexedSeq[Char], n: Int): String =
      List.fill(n)(source(Random.nextInt(source.length))).mkString

    val start = pick(letters, 2)
    val middle = pick(digits, 4)
    val end = pick(letters, 2)

    s"$start$middle$end"
  }
}
