package tdd

import org.junit.* 
import org.junit.Assert.*

import tasks.Shape.*
import tasks.Shape

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
