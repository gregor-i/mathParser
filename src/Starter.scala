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

import mathParser.complex.ComplexDerive
import mathParser.{Evaluate, Parser}
import mathParser.double.{DoubleDerive, DoubleLanguage}

object Starter {
  def exampleDoubles() = {
    println("exampleDoubles")
    import mathParser.implicits.doubleParseLiterals

    val lang = DoubleLanguage
    val xVariable = Set('x)
    val t = lang.parse("2^-x+e")(xVariable)
    val e = t.map{ term =>
      lang.eval(term){
        case 'x => 25
      }
    }
    println(t -> e)
  }

  def exampleDoublesDerive() = {
    println("exampleDoublesDerive")
    import mathParser.implicits.doubleParseLiterals

    val lang = DoubleLanguage
    val xVariable = Set('x)
    val t = lang.parse("2^-x")(xVariable)
    t.fold{
      println("input could not be parsed")
    }{ term =>
         println(lang.derive(term)('x))
    }
    println(t)
  }

  def exampleBooleans() = {
    println("exampleBooleans")
    import mathParser.implicits.booleanParseLiterals
    val lang = mathParser.boolean.BooleanLanguage
    println(lang.parse("!false")(Set.empty))
  }

  def exampleComplex() = {
    println("exampleComplex")
    import mathParser.implicits.complexParseLiterals
    val lang = mathParser.complex.ComplexLanguage
    println(lang.parse("sin(25 + 3*i)")(Set.empty))
  }

  def exampleDerive(s:String) = {
    println("exampleDerive")
    import mathParser.implicits.complexParseLiterals

    val lang = mathParser.complex.ComplexLanguage

    val t = lang.parse(s)(Set('x))

    val d = t.map(t => lang.derive(t)('x))

    println(t -> d)
  }


  def main(args: Array[String]): Unit = {
    exampleDoubles()
    exampleDoublesDerive()
    exampleBooleans()
    exampleComplex()
    exampleDerive("5*x")
  }
}
