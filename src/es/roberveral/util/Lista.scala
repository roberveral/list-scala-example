package es.roberveral.util

/**
  * A linked list ADT
  *
  * @tparam A type of list elements
  */
sealed trait Lista[+A]

/* A list can be either an empty Node or a Cons node */
case object Nil extends Lista[Nothing]

// An empty node is represented by Nil

case class Cons[+A](head: A, tail: Lista[A]) extends Lista[A]

// A node is represented by a Cons, composed by an element an a node

/**
  * A companion object with functions for creating and working with lists
  */
object Lista {
  /**
    * Creates a list with the arguments
    *
    * @param as array of arguments provided
    * @tparam A element type
    * @return a new List containing as elements
    */
  def apply[A](as: A*): Lista[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Returns a list with the result of applying a function to each element
    * of the input list
    *
    * @param ls input list
    * @param f  function to apply
    * @tparam A type of the source list elements
    * @tparam U type of the target list elements
    * @return target list
    */
  def map[A, U](ls: Lista[A])(f: A => U): Lista[U] =
    ls match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

  /**
    * Extracts from a list the elements that match a given predicate
    *
    * @param ls   source list
    * @param pred predicate function
    * @tparam A source list elements type
    * @return filtered list
    */
  def filter[A](ls: Lista[A])(pred: A => Boolean): Lista[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) if pred(x) => Cons(x, filter(xs)(pred))
    case Cons(_, xs) => filter(xs)(pred)
  }

  /**
    * Cocatenates two lists
    *
    * @param ls first list
    * @param rs second list
    * @tparam A type of list elements
    * @return appended list
    */
  def concat[A](ls: Lista[A], rs: Lista[A]): Lista[A] = ls match {
    case Nil => rs
    case Cons(x, xs) => Cons(x, concat(xs, rs))
  }

  /**
    * Performs a left folding on the given list
    *
    * @param ls source list
    * @param z  initial accumulator
    * @param f  function to combine an element with the aggregate
    * @tparam A type of the list elements
    * @tparam U type of the result to obtain
    * @return result of apply f to all the elements of ls and combine the results
    */
  def foldl[A, U](ls: Lista[A], z: U)(f: (U, A) => U): U = ls match {
    case Nil => z
    case Cons(x, xs) => foldl(xs, f(z, x))(f)
  }

  /**
    * Obtains the list in reversed order
    *
    * @param ls source list
    * @tparam A type of list elements
    * @return reversed list
    */
  def reverse[A](ls: Lista[A]): Lista[A] =
    foldl(ls, Lista[A]())((acc, h) => Cons(h, acc))

  /**
    * Performs a right folding of the given list
    *
    * @param ls source list
    * @param z  empty accumulator value
    * @param f  function to add an element to the accumulator
    * @tparam A type of source list elements
    * @tparam U type of the result
    * @return accumulated result for all list elements
    */
  def foldr[A, U](ls: Lista[A], z: U)(f: (A, U) => U): U =
    foldl(reverse(ls), z)((acc, h) => f(h, acc))

  /* This one has a worse performance than the previous one, because
   * of foldr uses foldl, and foldl uses tail recursion */
  def foldr2[A, U](ls: Lista[A], z: U)(f: (A, U) => U): U = ls match {
    case Nil => z
    case Cons(x, xs) => f(x, foldr2(xs, z)(f))
  }

  /**
    * Obtains the number of elements in a list
    *
    * @param ls source list
    * @tparam A type of list elements
    * @return number of elements
    */
  def length[A](ls: Lista[A]): Int =
    foldl(ls, 0)((acc, _) => acc + 1)

  /**
    * Applies a function to each element of the source list that
    * returns a List, and then concatenates all the resulting list into
    * one.
    *
    * @param ls source list
    * @param f  function to apply
    * @tparam A type of source list elements
    * @tparam U type of target list elements
    * @return target list
    */
  def flatMap[A, U](ls: Lista[A])(f: A => Lista[U]): Lista[U] =
    foldl(map(ls)(f), Nil: Lista[U])((acc, h) => concat(acc, h))

  /**
    * Obtains the first element of a list, if is not empty
    *
    * @param ls source list
    * @tparam A type of source list elements
    * @return the first element (None if ls is Empty)
    */
  def head[A](ls: Lista[A]): Option[A] = ls match {
    case Nil => None
    case Cons(x, _) => Some(x)
  }

  /**
    * Obtains all the elements without the first of a list
    *
    * @param ls source list
    * @tparam A type of source list elements
    * @return list without the first element
    */
  def tail[A](ls: Lista[A]): Lista[A] = ls match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  /**
    * Removes the first n elements of a list
    *
    * @param ls source list
    * @param n  number of elements to remove
    * @tparam A type of source list elements
    * @return list without the first n elements
    */
  def drop[A](ls: Lista[A], n: Int): Lista[A] = ls match {
    case Nil => Nil
    case _ if n <= 0 => ls
    case Cons(_, xs) => drop(xs, n - 1)
  }

  /**
    * Obtains the first n elements of a list
    *
    * @param ls source list
    * @param n  number of elements to obtain
    * @tparam A type of source list elements
    * @return list with the first n elements
    */
  def take[A](ls: Lista[A], n: Int): Lista[A] =
    if (n == 0) Nil
    else ls match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x, take(xs, n - 1))
    }

  /**
    * Checks if the given list contains an element
    *
    * @param ls source list
    * @param e  element to check
    * @tparam A type of source list elements
    * @return true if the element exists, else false
    */
  def contains[A](ls: Lista[A], e: A): Boolean = ls match {
    case Nil => false
    case Cons(x, _) if x == e => true
    case Cons(_, xs) => contains(xs, e)
  }

  /**
    * Obtains the i-th element of the list
    *
    * @param ls source list
    * @param i  index to obtain
    * @tparam A type of source list elements
    * @return the element contained, None if the index is not valid
    */
  def get[A](ls: Lista[A], i: Int): Option[A] =
    if (i < 0) None
    else ls match {
      case Nil => None
      case Cons(x, _) if i == 0 => Some(x)
      case Cons(_, xs) => get(xs, i - 1)
    }

  /**
    * Performs a zip between two given lists
    * @param ls first source list
    * @param rs second source list
    * @param f function to combine elements of the given list
    * @tparam A type of first source list elements
    * @tparam B type of second source list elements
    * @tparam C type of result list elements
    * @return result list
    */
  def zipWith[A, B, C](ls: Lista[A], rs: Lista[B])(f: (A, B) => C): Lista[C] = (ls, rs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }
}

object PruebaLista {
  def main(args: Array[String]) {
    println(Lista.flatMap(Lista(1, 2, 3))((x) => Lista(x.toString, (x + 2).toString)))
    println(Lista.get(Lista(1, 2, 3), 1))
    println(Lista.get(Lista(1, 2, 3), 3))
    println(Lista.take(Lista(1, 2, 3), 2))
    println(Lista.drop(Lista(1, 2, 3), 2))
    println(Lista.contains(Lista(1, 2, 3), 2))
    println(Lista.contains(Lista(1, 2, 3), 4))
    println(Lista.head(Lista(1, 2, 3)))
    println(Lista.zipWith(Lista(1,2,3),Lista('a','b','c'))((_,_)))
  }
}
