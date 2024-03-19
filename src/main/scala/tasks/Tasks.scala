package tasks

object Tasks extends App :
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
    
    //Task 5, svolto da solo


/**
 * Optional is a type that represents a value that may or may not be present.
 * Similar to Optional in Java but using the ADT concept.
 * Therefore, an Optional is a sum type with two cases: Maybe and Empty.
 * Maybe contains the value, and Empty represents the absence of a value.
 *
 * @tparam A
 */
enum Optional[A]:
    case Maybe(value: A)
    case Empty()

object Optional:
    /**
     * isEmpty returns true if the optional is Empty, false otherwise.
     * Example:
     *
     * isEmpty(Empty()) == true
     * isEmpty(Maybe(1)) == false
     *
     * @param optional the optional to check
     * @tparam A the type of the optional
     * @return true if the optional is Empty, false otherwise
     */
    def isEmpty[A](optional: Optional[A]): Boolean = optional match
      case Empty() => true
      case _ => false

    /**
     *
     * getOrElse returns the value of the optional if it is Maybe, otherwise it returns the default value.
     * Example:
     * orElse(Maybe(1), 0) == 1
     * orElse(Empty(), 0) == 0
     *
     * @param optional the optional to get the value from
     * @param default the default value to return if the optional is Empty
     * @tparam A the type of the optional
     * @tparam B the type of the default value
     * @return the value of the optional if it is Maybe, otherwise the default value
     */
    def orElse[A, B >: A](optional: Optional[A], default: B): B = optional match
      case Maybe(value) => value
      case Empty() => default

    /**
     * map applies the function f to the value of the optional if it is Maybe, otherwise it returns Empty.
     * Example:
     *
     * map(Maybe(1), (x: Int) => x + 1) == Maybe(2)
     * map(Empty(), (x: Int) => x + 1) == Empty()
     *
     *
     * @param optional the optional to apply the function to
     * @param f the function to apply to the value of the optional
     * @tparam A the type of the optional
     * @tparam B the type of the result of the function
     * @return the result of applying the function to the value of the optional if it is Maybe, otherwise Empty
     */
    def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
      case Maybe(v) => Maybe(f(v))
      case _ => Empty()

    def filter[A](optional: Optional[A], p: A => Boolean): Optional[A] = optional match
      case Maybe(v) if p(v) => optional
      case _ => Empty()

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

    @Test def mapShouldReturnEmptyWhenEmpty(): Unit =
        val empty: Optional[Int] = Optional.Empty()
        val result = Optional.map(empty, _ + 1)
        assertTrue(Optional.isEmpty(result))

    @Test def mapShouldReturnTransformedValueWhenNonEmpty(): Unit =
        val nonEmpty = Optional.Maybe(0)
        val result = Optional.map(nonEmpty, _ + 1)
        assertEquals(1, Optional.orElse(result, 1))  

    @Test def filterShouldReturnEmptyWhenEmpty(): Unit =
        val empty: Optional[Int] = Optional.Empty()
        val result = Optional.filter(empty, _ < 5)
        assertTrue(Optional.isEmpty(result))

    @Test def filterShouldReturnSearchedValueWhenNotEmpty(): Unit =
        val empty: Optional[Int] = Optional.Empty()
        val nonEmpty = Optional.Maybe(10)
        val result = Optional.filter(nonEmpty, _ > 5)
        assertEquals(nonEmpty, result)
        assertEquals(empty, Optional.filter(nonEmpty, _ > 15))

}