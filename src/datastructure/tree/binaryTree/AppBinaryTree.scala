package datastructure.tree.binaryTree

object AppBinaryTree extends App {
  var bt = new EmptyBinaryTree[Double]
  var t1 = new BinaryTree[Double](1.0,bt,bt)
  println("bt = " + bt)
  println("t1 = " + t1)
  var t2 = new BinaryTree[Double](2.0,t1,bt)
  println("t2 = " + t2)
  println("t2.value = " + t2.v)
  var bst1 = new EmptyBinarySearchTree[Double]()
  println("bst1 = " + bst1)
  var bst2 = new BinarySearchTree[Double](1.0,bst1,bst1)
  println("bst2 = " + bst2)
  var bst3 = new BinarySearchTree[Double](2.0,bst2,bst1)
  println("bst3 = " + bst3)
  var btnew = bt.insert(2.0);
  println("btnew = " + btnew)
  var bt2 = new EmptyBinaryTree[Double]
  try {
    bt2.delete(2.0)  
  } catch {
    case e: TreeException => println(e.msg)
  }
  var t4 = new BinaryTree[Double](2.1,bt2,bt2)
  println("t4 = " + t4)
  var t5 = t4.insert(3.0)
  println("t5 = " + t5)
  var t6 = t5.insert(3.1).insert(4.0).insert(7.0).insert(8.0).insert(9.0).insert(10.0)
  println("t6 = " + t6)
  var found = t6.search(4.0)
  println("found = " + found)
  var t7 = new EmptyBinaryTree[Double]()
  found = t7.search(2.0)
  println(found)
  var t8 = new BinaryTree(2.0,t7,t7)
  println(t8.search(2.0))
  
  var t9 = t8.insert(3.0);
  println(t9)
  println(t9.search(3.0))
  

}