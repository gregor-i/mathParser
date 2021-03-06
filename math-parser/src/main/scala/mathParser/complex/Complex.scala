package mathParser.complex

case class Complex(real: Double, imag: Double)

object Complex{
  val e = Complex(Math.E, 0.0)
  val pi = Complex(Math.PI, 0.0)
  val i = Complex(0.0, 1.0)
}
