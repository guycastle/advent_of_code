package utils

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

trait Syntax:

  extension [A](obj: A)
    def some: Option[A]            = Some(obj)
    def toSingleElementSeq: Seq[A] = Seq(obj)
  end extension

  extension [A: Numeric](number: A)
    def numberOfDigits: Int =
      val num = implicitly[Numeric[A]]
      (math.floor(math.log10(num.toDouble(number))) + 1).toInt
  end extension

  extension (str: String)
    def toIntSequence: Seq[Int]                                           = splitAndConvert(_.toIntOption)
    def toLongSequence: Seq[Long]                                         = splitAndConvert(_.toLongOption)
    private def splitAndConvert[A](function: String => Option[A]): Seq[A] = str.split("\\s+").toSeq.flatMap(function)
  end extension
  
  extension [A](opt: Option[A])
    def toTry: Try[A] = opt.fold[Try[A]](Failure(new IllegalArgumentException("Option is empty")))(Success(_))
  end extension

  extension [A](iterable: Iterable[A])
    def foldLeftWhile[B](initialValue: B)(predicate: (A, B) => Boolean)(function: (A, B) => B): B =
      @tailrec
      def loop(remaining: Iterable[A] = iterable, accumulator: B = initialValue): B = remaining.headOption match
        case Some(value) if predicate(value, accumulator) => loop(remaining.tail, function(value, accumulator))
        case _                                            => accumulator
      loop()
    end foldLeftWhile
    def toTryIterable[B](function: A => Try[B]): Try[Seq[B]] =
      @tailrec
      def tryElement(remaining: Iterable[A] = iterable, accumulator: Seq[B] = Seq.empty): Try[Seq[B]] =
        remaining.headOption.map(function) match
          case Some(Success(value)) => tryElement(remaining.tail, accumulator :+ value)
          case Some(Failure(err)) => Failure(err)
          case None => Success(accumulator)
      end tryElement
      tryElement(iterable)
  end extension

end Syntax
object Syntax extends Syntax
