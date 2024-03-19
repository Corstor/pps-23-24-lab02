package task5


object Optionals:
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
import Optionals.*

@Test def mapShouldReturnEmptyWhenEmpty(): Unit = {
  val empty: Optional[Int] = Optional.Empty()
  val result = Optional.map(empty, _ + 1)
  assertTrue(Optional.isEmpty(result))
}

@Test def mapShouldReturnTransformedValueWhenNonEmpty(): Unit = {
  val nonEmpty = Optional.Maybe(0)
  val result = Optional.map(nonEmpty, _ + 1)
  assertEquals(1, Optional.orElse(result, 1))
}  

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
