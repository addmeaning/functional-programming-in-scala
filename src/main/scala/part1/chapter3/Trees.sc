

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//Write a function size that counts the number of nodes (leaves and branches) in a tree.

def size[A](tree: Tree[A]): Int = tree match {
  case _: Leaf[A] => 1
  case b: Branch[A] => 1 + size(b.left) + size(b.right)
}

val tree = new Branch[Int](new Branch[Int](new Leaf[Int](1), new Leaf[Int](2)), new Branch[Int](new Leaf[Int](3), new Leaf[Int](4)))

size(tree)

def maximum(tree: Tree[Int]) : Int = tree match {
  case l:Leaf[Int] => l.value
  case b: Branch[Int] => maximum(b.left) max maximum(b.right)
}

maximum(tree)

//Write a function depth that returns the maximum path length from the root of a tree to any leaf.

def depth[A](tree: Tree[A]): Int = tree match {
  case _: Leaf[A] => 0
  case b: Branch[A] => 1 + (depth(b.right) max depth(b.left))
}

depth(tree)
//Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function
def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
  case Leaf(value) => Leaf(f(value))
  case Branch(left, right) => Branch(map(left)(f), map(right)(f))
}
map(tree)(x => x*x)

def fold[A, B](tree: Tree[A])(f: A => B)(g:(B, B) => B) : B = tree match {
  case Leaf(value) => f(value)
  case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
}

fold(tree)(x => 3*x)(_+_)

def printHelper[B](f: Tree[Int] => B)(folded: Tree[Int] => B): Unit = println(s"result: ${f(tree)}, is correct: ${f(tree) == folded(tree)} ")

def foldSize[A](tree: Tree[A]): Int = fold[A, Int](tree)(_ => 1)(1 + _ + _)

printHelper[Int](size)(foldSize)

def foldMaximum(tree: Tree[Int]) : Int = fold(tree)(identity)(_ max _)

printHelper[Int](maximum)(foldMaximum)

def foldDepth[A](tree: Tree[A]): Int = fold[A, Int](tree)(_ => 0)((a, b) => 1 + (a max b))

printHelper[Int](depth)(foldDepth)

def argInverter

def foldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree)(a => Leaf(f(a)))(Branch(_, _))

