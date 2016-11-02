package datastructure.tree.avl

import datastructure.tree.binaryTree.AbsBinarySearchTree
import datastructure.tree.binaryTree.TreeException

object AVLTree {
  def max(x:Int,y:Int) : Int = (x,y) match {
    case (x,y) if (x >= y) => x
    case _ => y
  }
}

abstract class AbsAVLTree[T<%Ordered[T]](var h:Int) extends AbsBinarySearchTree[T] {
  def rightRotation(): AbsAVLTree[T]
  def leftRotation(): AbsAVLTree[T]
  def rightDoubleRotation(): AbsAVLTree[T]
  def leftDoubleRotation() : AbsAVLTree[T]
  def insert2(item: T) : AbsAVLTree[T]
  
  def balanceTree(t: AbsAVLTree[T]) : AbsAVLTree[T] = t match {
    case EmptyAVLTree() => t
    case AVLTree(x,sf,l,r) if (sf == 0)=> t
    case AVLTree(x,sf,l,r) if (sf == -1) => t
    case AVLTree(x,sf,l,r) if (sf == 1)=> t
    case AVLTree(x,sf1,AVLTree(y,sf2,t1,t2),r) if ( (sf1 == -2) && (sf2 == -1) ) => t.asInstanceOf[AVLTree[T]].rightRotation()
    case AVLTree(x,sf1,l,AVLTree(y,sf2,t1,t2)) if ( (sf1 == 2) && (sf2 == 1) ) => t.asInstanceOf[AVLTree[T]].leftRotation()
    case AVLTree(x,sf1,AVLTree(y,sf2,t1,AVLTree(z,_,t2,t3)),r) if ( (sf1 == -2) && (sf2 == 0) ) => t.asInstanceOf[AVLTree[T]].rightDoubleRotation()
    case AVLTree(x,sf1,l,AVLTree(y,sf2,AVLTree(z,_,t1,t2),t3)) if ( (sf1 == 2) && (sf2 == 0) ) => t.asInstanceOf[AVLTree[T]].leftDoubleRotation()
  }
}

case class EmptyAVLTree[T<%Ordered[T]]() extends AbsAVLTree[T](-1) {
  
  override def toString = "["+h+"]"
  
  def insert(item:T) : AbsAVLTree[T] = new AVLTree[T](item,0,new EmptyAVLTree[T](),new EmptyAVLTree[T]())
  def insert2(item:T) : AbsAVLTree[T] = new AVLTree[T](item,0,new EmptyAVLTree[T](),new EmptyAVLTree[T]())

  def delete(item: T) : AbsAVLTree[T] = this.asInstanceOf[AbsAVLTree[T]]
  
  def search(item: T) : Boolean = false
  
  def rightRotation() = this.asInstanceOf[AbsAVLTree[T]]
  def leftRotation() = this.asInstanceOf[AbsAVLTree[T]]
  def rightDoubleRotation() = this.asInstanceOf[AbsAVLTree[T]]
  def leftDoubleRotation() = this.asInstanceOf[AbsAVLTree[T]]
}

case class AVLTree[T<%Ordered[T]](v:T,var sf:Int,left:AbsAVLTree[T],right:AbsAVLTree[T]) extends AbsAVLTree[T](1+AVLTree.max(left.h,right.h)) {
    
  override def toString = "[" +v+","+sf+","+h+","+left+","+right+ "]"
  
  def insert(item:T) : AbsAVLTree[T] = this.asInstanceOf[AbsAVLTree[T]] match {
    case AVLTree(x,sf,l,r) if (item < x) => {val t = l.insert(item).asInstanceOf[AVLTree[T]];balanceLeftInsertion(item,t)}
    case AVLTree(x,sf,l,r) if (item > x) => {val t = r.insert(item).asInstanceOf[AVLTree[T]];balanceRightInsertion(item,t)}
    case _ => this.asInstanceOf[AbsAVLTree[T]]    
  }

  def insert2(item:T) : AbsAVLTree[T] = this.asInstanceOf[AbsAVLTree[T]] match {
    case AVLTree(x,sf,l,r) if (item < x) => {val t = l.insert(item).asInstanceOf[AVLTree[T]]; val t1= new AVLTree(x,r.h-t.h,t,r);
      println("t1 = "  + t1);balanceTree(t1)}
    case AVLTree(x,sf,l,r) if (item > x) => {val t = r.insert(item).asInstanceOf[AVLTree[T]];val t1 = new AVLTree(x,t.h-l.h,l,t);println("t1 = " + t1);
      balanceTree(t1)}
    case _ => this.asInstanceOf[AbsAVLTree[T]]    
  }

  def delete(item: T) : AbsAVLTree[T] = this.asInstanceOf[AbsBinarySearchTree[T]] match {
    case AVLTree(x,_,l,r) if (item < x) => {val t=l.delete(item).asInstanceOf[AbsAVLTree[T]]; val t1 = new AVLTree(x,r.h-t.h,t,r); println("t11 = "  + t1);balanceTree(t1)}
    case AVLTree(x,_,l,r) if (item > x) => {val t = r.delete(item).asInstanceOf[AbsAVLTree[T]]; val t1=new AVLTree(x,t.h-l.h,l,t);println("t12 = "  + t1);balanceTree(t1)}
    
    case AVLTree(x,_,EmptyAVLTree(),EmptyAVLTree()) if (x == item) => new EmptyAVLTree()
    case AVLTree(x,_,EmptyAVLTree(),r) if (x == item) => r
    case AVLTree(x,_,l,EmptyAVLTree()) if (x == item) => l
    
    case AVLTree(x,_,l,r) if (x == item) => {var y = inorderPredecesor(l); val t=l.delete(y).asInstanceOf[AbsAVLTree[T]];
                          val t1=new AVLTree(y,r.h-t.h,t,r);println("t13 = "  + t1);balanceTree(t1)}
  }
  
  protected def inorderPredecesor(t:AbsAVLTree[T]) : T = t match {
    case EmptyAVLTree() => throw new TreeException("No inorder predecesor in empty tree")    
    case AVLTree(x,_,EmptyAVLTree(),EmptyAVLTree()) => x
    case AVLTree(x,_,l,EmptyAVLTree()) => x
    case AVLTree(x,_,l,r) => inorderPredecesor(r)
  }
  
  def search(item: T) : Boolean = this.asInstanceOf[AbsAVLTree[T]] match {
    case AVLTree(x,sf,l,r) if (item == x)=> true
    case AVLTree(x,sf,l,r) if (item < x) => l.search(item)
    case AVLTree(x,sf,l,r) if (item > x) => r.search(item)    
  }

  def balanceRightInsertion(item:T,t:AVLTree[T]) : AbsAVLTree[T] = this.asInstanceOf[AVLTree[T]] match {
    case AVLTree(x,sf,l,r) if (scala.math.abs(t.h-l.h) < 2) => new AVLTree(x,t.h-l.h,l,t)
    case AVLTree(x,sf,l,r) if (item > r.asInstanceOf[AVLTree[T]].v) => (new AVLTree(x,t.h-l.h,l,t)).leftRotation() 
    case AVLTree(x,sf,l,r) => (new AVLTree(x,t.h-l.h,l,t)).rightDoubleRotation()
  }

  def balanceLeftInsertion(item:T,t:AVLTree[T]) : AbsAVLTree[T] = this.asInstanceOf[AVLTree[T]] match {
    case AVLTree(x,sf,l,r) if (scala.math.abs(r.h-t.h) < 2) => new AVLTree(x,r.h-t.h,t,r)
    case AVLTree(x,sf,l,r) if (item < l.asInstanceOf[AVLTree[T]].v) => (new AVLTree(x,r.h-t.h,t,r)).rightRotation() 
    case AVLTree(x,sf,l,r) => (new AVLTree(x,r.h-t.h,t,r)).leftDoubleRotation()
  }
  
  /**
   * op rotDer: AVL{X} -> [AVL{X}] .
		eq rotDer(arbolBin(R1, arbolBin(R2, I2, D2), D1)) == arbolBin(R2, I2, arbolBin(R1, D2, D))
   */
  def rightRotation() : AbsAVLTree[T] = this.asInstanceOf[AbsAVLTree[T]] match {
    case AVLTree(x,_,AVLTree(x2,_,l2,r2),r) => new AVLTree[T](x2,1+AVLTree.max(r2.h, r.h)-l2.h,l2,new AVLTree(x,r.h-r2.h,r2,r))
    case _ => this
  }
   
  /**
   * op rotIzq: AVL{X} -> [AVL{X}] .
			eq rotIzq(arbolBin(R1, I, arbolBin(R2, I2, D2))) == arbolBin(R2, arbolBin(R1, I, I2), D2) .
   */
  def leftRotation() : AbsAVLTree[T] = this.asInstanceOf[AbsAVLTree[T]] match {
    case AVLTree(x,_,l,AVLTree(x2,_,l2,r2)) => new AVLTree[T](x2,r2.h-(1+AVLTree.max(l.h, l2.h)),new AVLTree[T](x,l2.h-l.h,l,l2),r2)
    case _ => this
  }
  
  def rightDoubleRotation() : AbsAVLTree[T] = this.asInstanceOf[AbsAVLTree[T]] match {
    case AVLTree(x,_,l,r) => {val t1 = l.leftRotation();val t2 = new AVLTree(x,r.h-t1.h,t1,r);t2.rightRotation()}
    case _ => this
  }

  def leftDoubleRotation() : AbsAVLTree[T] = this.asInstanceOf[AbsAVLTree[T]] match {
    case AVLTree(x,_,l,r) => {val t1 = r.rightRotation();val t2 = new AVLTree(x,t1.h-l.h,l,t1);t2.leftRotation()}
    case _ => this
  }

}