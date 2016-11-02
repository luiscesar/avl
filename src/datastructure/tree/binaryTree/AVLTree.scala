package datastructure.tree.binaryTree

abstract class AbsAVLTree[T <% Ordered[T]] extends AbsBinarySearchTree[T]

case class EmptyAVLTree[T <% Ordered[T]]() extends AbsAVLTree[T] {
  def insert(item:T) : AbsAVLTree[T] = null.asInstanceOf[AbsAVLTree[T]]
  def delete(item:T) : AbsAVLTree[T] = null.asInstanceOf[AbsAVLTree[T]]
  def search(item:T) : Boolean = false
  
}

case class AVLTree[T <% Ordered[T]](x:T,l:AbsAVLTree[T],r:AbsAVLTree[T]) extends AbsAVLTree[T] {
  def insert(item:T) : AbsAVLTree[T] = null.asInstanceOf[AbsAVLTree[T]]
  def delete(item:T) : AbsAVLTree[T] = null.asInstanceOf[AbsAVLTree[T]]
  def search(item:T) : Boolean = false  
}
