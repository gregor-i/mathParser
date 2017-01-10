/*
 * Copyright (C) 2016  Gregor Ihmor
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mathParser.double

import mathParser.{Evaluate, Language, Parser}

object DoubleLanguage
  extends Language
  with DoubleSyntaxSugar
  with DoubleDerive
  with Parser
  with Evaluate {

  override type Skalar = Double
  override type Constant = DoubleConstant
  override type UnitaryOperator = DoubleUnitaryOperator
  override type BinaryOperator = DoubleBinaryOperator

  override def unitaryOperators: Seq[UnitaryOperator] = Seq(Neg, Sin, Cos, Tan, Asin, Acos, Atan, Sinh, Cosh, Tanh, Exp, Log)

  override def binaryOperators: Seq[BinaryOperator] = Seq.empty

  override def binaryInfixOperators: Seq[BinaryOperator] = Seq(Plus, Minus, Times, Divided, Power)

  override def constants(): Seq[Constant] = Seq(e, pi)
}

trait DoubleSyntaxSugar {
  _: DoubleLanguage.type =>

  def neg(t:Node): UnitaryNode = UnitaryNode(Neg, t)
  def sin(t:Node): UnitaryNode = UnitaryNode(Sin, t)
  def cos(t:Node): UnitaryNode = UnitaryNode(Cos, t)
  def tan(t:Node): UnitaryNode = UnitaryNode(Tan, t)
  def asin(t:Node): UnitaryNode = UnitaryNode(Asin, t)
  def acos(t:Node): UnitaryNode = UnitaryNode(Acos, t)
  def atan(t:Node): UnitaryNode = UnitaryNode(Atan, t)
  def sinh(t:Node): UnitaryNode = UnitaryNode(Sinh, t)
  def cosh(t:Node): UnitaryNode = UnitaryNode(Cosh, t)
  def tanh(t:Node): UnitaryNode = UnitaryNode(Tanh, t)
  def exp(t:Node): UnitaryNode = UnitaryNode(Exp, t)
  def log(t:Node): UnitaryNode = UnitaryNode(Log, t)

  def plus(t1: Node, t2: Node): BinaryNode = BinaryNode(Plus, t1, t2)
  def minus(t1: Node, t2: Node): BinaryNode = BinaryNode(Minus, t1, t2)
  def times(t1: Node, t2: Node): BinaryNode = BinaryNode(Times, t1, t2)
  def divided(t1: Node, t2: Node): BinaryNode = BinaryNode(Divided, t1, t2)
  def power(t1: Node, t2: Node): BinaryNode = BinaryNode(Power, t1, t2)

  def constant(v: DoubleLanguage.Skalar): ConstantNode = ConstantNode(v)
}