package datastructure.tree.avl

import datastructure.tree.binaryTree.TreeException
import scala.collection.immutable.TreeSet

object AppAVLTree {
  
  def main(args: Array[String]) {
    val s : TreeSet[Int] = TreeSet()
    
    var avl1 = new EmptyAVLTree[Double]()
    println(avl1)
    try {
      var avl2 = avl1.insert(2.0).insert(1.0).insert(0.5).insert(1.5).insert(3.0).insert(4.0).insert(5.0);
      //var avl2 = avl1.insert(2.0);
      println(avl2)
      var avl3 = avl2.asInstanceOf[AbsAVLTree[Double]].rightRotation()
      println(avl3)
      var avl4 = avl3.asInstanceOf[AbsAVLTree[Double]].leftRotation()
      println(avl4)
      var avl5 = avl1.insert(5).insert(3).insert(10).insert(7).insert(12).insert(6).insert(8)
      println(avl5)
      var avl6 = avl5.asInstanceOf[AbsAVLTree[Double]].leftDoubleRotation();
      println(avl6)
      var avl7 = avl1.insert(9).insert(5).insert(10).insert(1).insert(7).insert(6).insert(8)
      println(avl7)
      var avl8 = avl7.asInstanceOf[AbsAVLTree[Double]].rightDoubleRotation();
      println(avl8)
      var avl9 = avl1.insert(1).insert(2).insert(3).insert(4).insert(5).insert(6).insert(7).insert(9).insert(8).insert(20)
      println("avl9 = " + avl9)
      var avl10 = avl1.insert2(1).insert2(2).insert2(3).insert2(4).insert2(5).insert2(6).insert2(7)
      println("avl10 = " + avl10);
      var avl11 = avl10.delete(1)
      println("avl11 = " + avl11)
      avl11 = avl11.delete(6);
      println("avl11 = " + avl11)
      avl11 = avl11.delete(7);
      println("avl11 = " + avl11)
    } catch {
      case e:TreeException => println(e.msg)
    }
    
  }
  
}