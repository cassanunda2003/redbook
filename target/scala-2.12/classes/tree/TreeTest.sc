import tree.{Branch, Leaf, Tree}

val t = Branch(Leaf(2),Branch(Leaf(3), Branch(Leaf(5),Leaf(6))))

def size[A](br: Tree[A]):Int =
    br match {
      case Leaf(_) => 1
      case Branch(x,y) => 1 + size(x) + size(y)
    }

def maximum(br: Tree[Int]): Int =
    br match {
      case Leaf(x) => x
      case Branch(x,y) => maximum(x) max maximum(y)
    }


val sz = size(t)

val mx = maximum(t)