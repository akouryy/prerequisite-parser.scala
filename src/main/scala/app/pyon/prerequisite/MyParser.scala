package app.pyon.prerequisite

import scala.util.parsing.combinator._
import scala.util.parsing.input._

given Eql[Position, NoPosition.type] = Eql.derived

trait MyParser(debug: Boolean) extends Parsers:
  def rep2sep[T] (p: => Parser[T], q: => Parser[Any]): Parser[List[T]] =
    p ~ q ~ rep1sep(p, q) ^^ :
      case x ~ _ ~ xs => x :: xs

  private var debugIndent = 0

  // http://jim-mcbeath.blogspot.com/2011/07/debugging-scala-parser-combinators.html
  extension [T](name: String):
    def D_(parser: Parser[T]) =
      if debug
        new Parser[T]:
          def apply(in: Input): ParseResult[T] =
            val take3Base =
              if in.atEnd                then Nil
              else if in.rest.atEnd      then Seq(in.first)
              else if in.rest.rest.atEnd then Seq(in.first, in.rest.first)
              else                            Seq(in.first, in.rest.first, in.rest.rest.first)
            val take3 = take3Base.mkString:
              take3Base.headOption match
                case Some(_: Char) => ""
                case _       => ","

            val pos = if in.pos == NoPosition then "?" else in.pos.toString
            println("  ".repeat(debugIndent) + s"begin <$name> $pos$"$take3$"")
            debugIndent += 1

            val res = parser(in)
            val resStr =
              def g(next: Input) =
                if next.pos == NoPosition
                  res.toString.split('\n')(0).replace("[<undefined position>] ", "")
                else
                  res.toString
              res match
                case Success(_, next) => g(next)
                case Failure(_, next) => g(next)
                case Error(_, next)   => g(next)

            debugIndent -= 1
            println("  ".repeat(debugIndent) + s"end   <$name> $pos$"$take3$" --> $resStr")
            res
      else
        parser

  end extension

  extension [A, B, T](fn: (A, B) => T):
    def unary_~ : A ~ B => T =
      case a ~ b => fn(a, b)

  extension [A, B, C, T](fn: (A, B, C) => T):
    def unary_~ : A ~ B ~ C => T =
      case a ~ b ~ c => fn(a, b, c)

  extension [A, B, C, D, T](fn: (A, B, C, D) => T):
    def unary_~ : A ~ B ~ C ~ D => T =
      case a ~ b ~ c ~ d => fn(a, b, c, d)

  extension [A, B, C, D, E, T](fn: (A, B, C, D, E) => T):
    def unary_~ : A ~ B ~ C ~ D ~ E => T =
      case a ~ b ~ c ~ d ~ e => fn(a, b, c, d, e)

  def tuple2[A, B](in: A ~ B) = in match
    case a ~ b => (a, b)

end MyParser

object MyParser:
  class SeqReader[T](seq: Seq[T]) extends Reader[T]:
    override def first = seq.head
    override def atEnd = seq.isEmpty
    override def pos = NoPosition
    override def rest = new SeqReader(seq.tail)

  class PositionedSeqReader[T <: Positional](seq: Seq[T]) extends SeqReader(seq):
    this: SeqReader[T] =>

    override def pos = seq.headOption.fold(NoPosition)(_.pos)
    override def rest = new PositionedSeqReader(seq.tail)
