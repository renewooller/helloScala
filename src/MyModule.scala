import fpinscala.datastructures.List

object MyModule {
  def abs(n: Int) : Int =
    if(n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  def formatAbsInterpol(x: Int) = {
    s"The absolute value of $x is ${abs(x)}."
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if(n<=0) acc
      else go(n-1, n*acc)
    }
    go(n,1)
  }

  // 0, 1, 1, 2, 3, 5, 8
  def fib(n: Int) = {
    @annotation.tailrec
    def go(p: Int, c:Int, x:Int, n: Int): Int = {
      if(x>= n) c
      else go(c, p+c, x+1, n)
    }
    go(0, 1, 1, n)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def isSorted[A](s: Array[A], gt: (A,A) => Boolean): Boolean = {
    def go(i: Int, s: Array[A], gt: (A,A) => Boolean): Boolean = {
      if(i > s.length-1) true
      else if(gt(s(i), s(i-1))) go(i+1, s, gt)
      else false
    }
    go(1, s, gt)

  }

  def partial1[A,B,C] (a: A, f: (A,B) => C): B => C = {
    (b: B) => { f(a, b) }
  }


/*
// anonymous functions like these can't be made generic
println(formatResult("absolute value", -42, abs))
 println(formatResult("factorial", 7, factorial))
 println(formatResult("increment", 7, (x: Int) => x + 1))
 println(formatResult("increment2", 7, (x) => x + 1))
 println(formatResult("increment3", 7, x => x + 1))
 println(formatResult("increment4", 7, _ + 1))
 println(formatResult("increment5", 7, x => { val r = x + 1; r }))
 */

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    def ab (a: A): (B) => C = {
      def bc(b: B): C = {
        f(a, b)
      }
      bc
    }
    ab
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    def ab_c (a: A, b: B): C = {
      f(a)(b)
    }
    ab_c
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    def ac(a: A):C = {
      f(g(a))
    }
    ac
  }

  def main(args: Array[String]): Unit = {
    //    println(formatResult("absolute", -42, abs))
    //    println(formatResult("fib", 1, fib))
    //    println(formatResult("fac", 420, factorial))
    //    println(formatResult("inc1", 3, _ +1))
    //    println(formatResult("inc2", 3, x => x +1))


    val a = List(1, 2, 3, 4)
    val b = List(6, 7, 8, 9)
//    val ad = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
//    val b = List.tail(a)
//    val c = List.drop(a, 4)
//    val d = List.head(a)
//  //  val e = curry(List.dropWhile, a)((i: Int) => i<=5)
//    val f = List.dropWhilec(a)((i: Int) => i<=5)
//    val g = List.dropWhilec(a)(_)
//    val h = g((i: Int) => i<=4)
//    val i = List.setHead(a, 8)
//    val j = List.init(a)
//    val k = List.sum2(a)
//    val l = List.product2(ad)
//    val ln = List.length(a)
//
//    val fl = List.foldLeft(a, 0)(_ + _)

    //val rv = List.reverse(a)
    //val rv = List.reverseRecur(a)
    //val rv = List.reverseFold(a)
    //val flr = List.foldLeftInTermsOfRight(a, 0)(_+_)
    //val frl = List.foldRightInTermsOfLeft(a, 0)(_+_)
    //val flr = List.foldLeftInTermsOfRight(a, 0)(_+_)
    //val afl = List.appendFl(a, b)
    //val afr = List.appendFr(a, b)
    //val cl = List.concatLoL(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9, 10)))
    //val ds = List.dotSum(a)(-11)
    //val ds = List.doubleToString(List(0.1, 0.23, 0.35))
    //val fe = List.filter(a)((xx:Int) => {xx > 2})
    //val flm = List.flatMap(a)((x) => {List(x*2, x*5)})
//    val fflm = List.filterFromFlatMap(a)((xx:Int) => {xx > 2})
    //val zw = List.zipWith(a, b)(_+_)
    var hs = List.hasSubsequence(List(1, 2, 3, 4), List(2, 3))
    println("list example: " + hs)



    // println("list example: " + l)
    //    println("list example: " + List.m)



  }


}