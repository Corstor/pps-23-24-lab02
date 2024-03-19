object functions extends App :
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