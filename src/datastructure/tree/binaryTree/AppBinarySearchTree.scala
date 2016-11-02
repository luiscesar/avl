package datastructure.tree.binaryTree

object AppBinarySearchTree extends App {
  var bst1 = new EmptyBinarySearchTree[Double]()
  println(bst1)
  var bst2 = bst1.insert(1.0)
  println(bst2)
  var bst3 = bst2.insert(2.0)
  println(bst3)
  var bst4 = bst3.insert(0.5)
  println(bst4)
  var bst5 = bst4.insert(4.0).insert(3.0).insert(5.0)
  println (bst5)
  println(bst5.search(4.0))
  var bst6 = bst5.delete(4.0);
  println(bst6)
  var bst7 = bst6.delete(3.0)
  println(bst7)
  var bst8 = bst7.delete(1.0)
  println(bst8)
  var bst9 = bst8.delete(0.5)
  println(bst9)
  var bst10 = bst9.delete(2.0);
  println(bst10)
  var bst11 = bst10.delete(5.0)
  println(bst11)
}