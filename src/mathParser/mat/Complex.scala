package mathParser.mat

import matryoshka._
import matryoshka.implicits._

import spire.math.Complex
import spire.implicits._

sealed trait Expr[A]

sealed abstract class Unitary[A](val name: Symbol,
                                 val apply: (Complex[Double] => Complex[Double])) extends Expr[A]{
  val child:A
}
case class Neg[A](child:A) extends Unitary[A]('-, -_)
case class Sin[A](child:A) extends Unitary[A]('sin, _.sin)
case class Cos[A](child:A) extends Unitary[A]('cos, _.cos)
case class Tan[A](child:A) extends Unitary[A]('tan, _.tan)
case class Asin[A](child:A) extends Unitary[A]('asin, _.asin)
case class Acos[A](child:A) extends Unitary[A]('acos, _.acos)
case class Atan[A](child:A) extends Unitary[A]('atan, _.atan)
case class Sinh[A](child:A) extends Unitary[A]('sinh, _.sinh)
case class Cosh[A](child:A) extends Unitary[A]('cosh, _.cosh)
case class Tanh[A](child:A) extends Unitary[A]('tanh, _.tanh)
case class Exp[A](child:A) extends Unitary[A]('exp, _.exp)
case class Log[A](child:A) extends Unitary[A]('log, _.log)

sealed abstract class Binary[A](val name: Symbol,
                                val apply: ((Complex[Double], Complex[Double]) => Complex[Double])) extends Expr[A]{
  val left:A
  val right:A
}
case class Plus[A](left:A, right:A) extends Binary[A]('+, _ + _)
case class Minus[A](left:A, right:A) extends Binary[A]('-, _ - _)
case class Times[A](left:A, right:A) extends Binary[A]('*, _ * _)
case class Divided[A](left:A, right:A) extends Binary[A]('/, _ / _)
case class Power[A](left:A, right:A) extends Binary[A]('^, _ ** _)

abstract class NamedConstant[A](val name:Symbol,
                                val apply: Complex[Double]) extends Expr[A]
case class e[A]() extends NamedConstant[A]('e, Complex(Math.E, 0))
case class pi[A]() extends NamedConstant[A]('pi, Complex(Math.PI, 0))
case class i[A]() extends NamedConstant[A]('i, Complex(0, 1))


object Mat extends App{
  implicit val exprFunctor = new scalaz.Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: A => B) = fa match{
      case u:Unitary[A] => new Unitary[B](u.name, u.apply){
        val child:B = f(u.child)
      }
      case c:NamedConstant[A] => new NamedConstant[B](c.name, c.apply){}
      case b:Binary[A] => new Binary[B](b.name, b.apply){
        val left:B = f(b.left)
        val right:B = f(b.right)
      }
    }
  }

  val eval: Algebra[Expr, Complex[Double]] = {
    case c:NamedConstant[_] => c.apply
    case u:Unitary[Complex[Double]] => u.apply(u.child)
    case b:Binary[Complex[Double]] => b.apply(b.left, b.right)
  }

  def someExpr[T](implicit T: Corecursive.Aux[T, Expr]): T =
    Sin[T](Plus[T](pi[T]().embed, i[T]().embed).embed).embed

  import matryoshka.data.Mu

  println(someExpr[Mu[Expr]].cata(eval))
}

