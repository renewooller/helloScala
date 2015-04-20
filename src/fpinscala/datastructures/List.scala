package fpinscala.datastructures

// this renames the imported scala list to ScalaList, so that it doesn't get confused for the
// fpinscala list
import scala.{List => ScalaList}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightInTermsOfLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b:B, a:A) => {f(a, b)})
  }

  def foldLeftInTermsOfRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(l), z)((a:A, b:B) => {f(b, a)})
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // append in terms of fold left
  def appendFl[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((b:List[A], a:A) => {Cons(a, b)})
  }

  // append in terms of fold right
  def appendFr[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((a:A, b:List[A]) => {Cons(a, b)})
  }

  def concatLoL[A](l:List[List[A]]): List[A] = {
    foldLeft(tail(l), head(l))((a:List[A], b:List[A]) => {List.append(a, b)})
  }

  def sum3(l: List[Int]) =
    foldLeft(l, 0.0)(_ + _)

  def product3(l: List[Int]) =
    foldLeft(l, 1.0)(_ * _)

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)


  def product4(l: List[Double]) = {
    foldRight(l, 1.0)((a:Double, b:Double) => {a * b})
  }

  def reverse[A](l: List[A]): List[A] = {
    def go (t: List[A], r: List[A]): List[A] = {
        t match {
          case Cons(h, Nil) => Cons(h, r)
          case Cons(h, t) => go(t, Cons(h, r))
        }
    }
    go(l, List())
  }

  def reverseFold[A](l: List[A]): List[A] = {
    foldLeft(tail(l), List(head(l)))((b:List[A], a:A) => {
      List.append(List(a), b)
    })
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def reverseRecur[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => List(h)
      case Cons(h, t) => List.append(List.reverseRecur(t), List(h))
    }
  }

  def dotFun[A](l: List[A])(f:A=>A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => List(f(h))
      case Cons(h, t) => List.append(List(f(h)), dotFun(t)(f))
    }
  }

  def dotSum(l: List[Int])(i: Int): List[Int] = {
    dotFun(l)((x) => {x+i})
  }

  def tail[A](in: List[A]): List[A] = in match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  //
  def head[A](in: List[A]): A = in match {
    case Cons(h, t) => h
  }

  def drop[A](in: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(i: Int, din: List[A]): List[A] = {
      if (i < n) {
        go(i + 1, tail(din))
      } else {
        din
      }
    }
    go(0, in)
  }

  def dropWhile[A](in: List[A], m: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(i: Int, din: List[A]): List[A] = {
      if (m(head(din))) {
        go(i + 1, tail(din))
      } else {
        din
      }
    }
    go(0, in)
  }

  def dropWhilec[A](in: List[A])(m: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(i: Int, din: List[A]): List[A] = {
      if (m(head(din))) {
        go(i + 1, tail(din))
      } else {
        din
      }
    }
    go(0, in)
  }



  def setHead[A](a: List[A], v: A): List[A] = {
    a match {
      case Nil => List(v)
      case Cons(h, t) => Cons(v, t)
    }
  }

  //    println(formatResult("absolute", -42, abs))
  //    println(formatResult("fib", 1, fib))
  //    println(formatResult("fac", 420, factorial))
  //    println(formatResult("inc1", 3, _ +1))
  //    println(formatResult("inc2", 3, x => x +1))

  // all but the last
  def init[A](l: List[A]): List[A] = {
     l match {
       case Nil => Nil
       case Cons(h, Nil) => Nil
       case Cons(h, t) => Cons(h, init(t))
     }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((a: A, b: Int) => { 1 + b})
  }

  def doubleToString(l: List[Double]):List[String] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => List(h.toString())
      case Cons(h, t) => Cons(h.toString(), doubleToString(t))
    }
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case Cons(h, Nil) => List(f(h))
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(h, Nil) => if (f(h)) Cons(h, Nil) else Nil
      case Cons(h, t) => if (f(h)) Cons(h, t) else filter(t)(f)
    }
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => Nil
      case Cons(h, Nil) => f(h)
      case Cons(h, t) => List.append(f(h), flatMap(t)(f))
    }
  }

  def filterFromFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((x) => {if (f(x)) List(x) else List()})
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = {
    (a, b) match {
      case (Nil, Nil) => Nil
      case (Cons(h, Nil), Cons(h2, Nil)) => Cons(f(h, h2),Nil)
      case (Cons(h, t), Cons(h2, t2)) => Cons(f(h, h2), List.zipWith(t, t2)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasSubsequenceAcc[A](asup: List[A], asub: List[A], back: List[A]): Boolean = {
      (asup, asub) match {
        case (Cons(h1, Nil), Cons(h2, Nil)) => {
          if(h1 == h2) true
          else if(back == null) false
          else hasSubsequenceAcc(back, sub, Nil)
        }
        case (Cons(h1, t1), Cons(h2, Nil)) => {
          if(h1 == h2) true
          else if(back == null) hasSubsequenceAcc(t1, asub, null)
          else hasSubsequenceAcc(back, sub, null)
        }
        case (Cons(h1, Nil), Cons(h2, t2)) => {
          false
        }
        case (Cons(h1, t1), Cons(h2, t2)) => {
          if (h1 == h2 && back == null) {
            hasSubsequenceAcc(t1, t2, t1)
          } else if (h1 == h2 && back != null)
            hasSubsequenceAcc(t1, t2, back)
          else if (back == null)
            hasSubsequenceAcc(t1, sub, null)
          else
            hasSubsequenceAcc(back, sub, null)
        }
      }
    }
    return hasSubsequenceAcc(sup, sub, null)
  }

  //def length[A](l: List[A]): Int =
//    foldRight(l, 0)()}

//  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
//    l match {
//      case Nil => z
//      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//    }


//
//      @annotation.tailrec
//      def go (i: Int): List[A] = {
//        if(m(head(in))) {
//          dropWhile(tail(in), m)
//          go(i+1)
//        } else {
//          in
//        }
//      }
//      go(0)



  // could implement this without the go, but would also want to put a interation limit have have it in tail form
//  def dropWhile[A](in: List[A], m: A => Boolean): List[A] = {
//    @annotation.tailrec
//    def go (i: Int): List[A] = {
//      if(m(head(in))) {
//        dropWhile(tail(in), m)
//        go(i+1)
//      } else {
//        in
//      }
//    }
//    go(0)
//  }

  // List(1, 2, 3, 4, 5, 6).dropWhile2((x:Int)=>{x < 4})
  // List(1, 2, 3, 4, 5, 6).dropWhile((x:Int)=>{x < 4})
//  @annotation.tailrec
//  def dropWhilee[A](in: List[A], m: A => Boolean): List[A] = {
//    if(m(head(in))) {
//      dropWhilee(tail(in), m)
//    } else {
//      in
//    }
//  }


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)

  val m = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
}