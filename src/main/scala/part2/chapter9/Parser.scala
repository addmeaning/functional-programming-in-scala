//package part2.chapter9
//
//class Parser[B] {
//
//}
//object Parser{
//  def char(c: Char): Parser[Char] = ???
//  def run[A](parser: Parser[A])(input: String): Either[ParseError, A]
//}
//
//trait Parsers[ParseError, Parser[+_]]
//object Demo extends App{
//  new Parser[Char]
//}