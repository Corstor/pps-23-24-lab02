package tasks

object functions extends App :
    //Task 2a, svolto da solo
    val isPositive: Int => String = n => n match
        case n if n >= 0 => "positive"
        case _ => "negative"

    def isPositiveDef(n: Int): String = n match
        case n if n >= 0 => "positive"
        case _ => "negative"

    println("Positive Val")

    println(isPositive(10)) //Positive
    println(isPositive(-5)) //Negative

    println("Positive Def")

    println(isPositiveDef(10)) //Positive
    println(isPositiveDef(-5)) //Negative

    def negDef(f: String => Boolean): String => Boolean = 
        s => !f(s)
    
    val neg: (String => Boolean) => (String => Boolean) = f => s => !f(s)

    val empty = (s:String) => s == ""

    val notEmpty = neg(empty)

    println("Neg val")

    println(notEmpty("foo")) //True
    println(notEmpty("")) //False

    val notEmptyDef = negDef(empty)

    println("Neg Def")

    println(notEmptyDef("foo")) //True
    println(notEmptyDef("")) //False

    def negDefGen[A](f: A => Boolean): A => Boolean =
        e => !f(e)
    
    val isPositivePred: Int => Boolean = n => n match
        case n if n >= 0 => true
        case _ => false
    
    val isNegativePred = negDefGen(isPositivePred)
    
    println("Generic neg")
    println(isNegativePred(10)) //False
    println(isNegativePred(-10)) //True

    //Task 2b, svolto da solo
    println("Currying val")

    val p1: (Int) => (Int) => (Boolean) => Boolean = x => y => z => x <= y == z
    println(p1(5)(7)(true)) //True
    println(p1(5)(5)(false)) //False
    println(p1(5)(4)(true)) //False

    println("Non-Currying val")

    val p2: (Int, Int, Boolean) => Boolean = (x, y, z) => x <= y == z

    println(p2(5,7,true)) //True
    println(p2(5,5,false)) //False
    println(p2(5,4,true)) //False

    println("Currying def")

    def p3(x: Int)(y: Int)(z: Boolean): Boolean = x <= y == z

    println(p3(5)(7)(true)) //True
    println(p3(5)(5)(false)) //False
    println(p3(5)(4)(true)) //False

    println("Non-Currying def")

    def p4(x: Int, y: Int, z: Boolean): Boolean = x <= y == z

    println(p4(5,7,true)) //True
    println(p4(5,5,false)) //False
    println(p4(5,4,true)) //False 

    println("Composition")

    def compose(f: Int => Int, g: Int => Int): Int => Int =
        x => f(g(x))
    
    println(compose(_ - 1, _ * 2)(5)) // 9

    println("Composition with generics")

    def composeGen[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

    println(composeGen((x:String) => x + "!", (x:String) => x + " world")("hello")) //hello world!
    println(composeGen((s:String) => ((x:String) => x == s), (n:Int) => n + " hello")(10)("10 hello")) //true

    //Task 3, svolto da solo

    println("GCD")

    @annotation.tailrec
    def gcd(a: Int, b: Int): Int = (a, b) match
        case (a, 0) => a
        case (a, b) if a >= b => gcd(b, a % b)
        case _ => gcd(b, a)

    println(gcd(10, 5)) //5
    println(gcd(14, 8)) //2
    println(gcd(5, 10)) //5
    println(gcd(12, 8)) //4

    //Task 4, svolto da solo

enum Shape():
    case Rectangle(h: Double, b: Double)
    case Circle(r: Double)
    case Square(l: Double)

object Shape:
    def perimeter(shape: Shape): Double = shape match
        case Rectangle(h, b) => h * 2 + b * 2
        case Circle(r) => r * 2 * Math.PI
        case Square(l) => l * 4

    def scale(shape: Shape, alpha: Double): Shape = shape match
        case Rectangle(h, b) => Rectangle(h * alpha, b * alpha)
        case Circle(r) => Circle(r * alpha)
        case Square(l) => Square(l * alpha)
    

import org.junit.* 
import org.junit.Assert.*

import tasks.Shape.*

class Tests {
    @Test def testPerimeter() =
        assertEquals(10.0, perimeter(Rectangle(2, 3)), 0)
        assertEquals(Math.PI * 4, perimeter(Circle(2)), 0)
        assertEquals(16.0, perimeter(Square(4)), 0)
    
    @Test def testScale() =
        assertEquals(Rectangle(4, 6), scale(Rectangle(2, 3), 2))
        assertEquals(Circle(8), scale(Circle(2), 4))
        assertEquals(Square(10), scale(Square(5), 2))

}