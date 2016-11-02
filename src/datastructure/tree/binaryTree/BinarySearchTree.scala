package datastructure.tree.binaryTree

abstract class AbsBinarySearchTree[T <% Ordered[T]] extends AbsBinaryTree[T]

case class EmptyBinarySearchTree[T <% Ordered[T]]() extends AbsBinarySearchTree[T] {
  override def toString = "[]"

  def insert(item:T) : AbsBinarySearchTree[T] = new BinarySearchTree(item,new EmptyBinarySearchTree[T],EmptyBinarySearchTree[T])
  def delete(item: T) : AbsBinarySearchTree[T] = this.asInstanceOf[AbsBinarySearchTree[T]] 
  def search(item:T) : Boolean = false
}

case class BinarySearchTree[T <% Ordered[T]](v:T,left:AbsBinarySearchTree[T],right:AbsBinarySearchTree[T]) 
  extends AbsBinarySearchTree[T] {
  
  override def toString = "BST["+v+","+left+","+right+"]"

  def insert(item:T) : AbsBinarySearchTree[T] = this.asInstanceOf[AbsBinarySearchTree[T]] match {
    case BinarySearchTree(x,l,r) if (item < x) => new BinarySearchTree(x,l.insert(item).asInstanceOf[AbsBinarySearchTree[T]],r)
    case BinarySearchTree(x,l,r) if (item > x) => new BinarySearchTree(x,l,r.insert(item).asInstanceOf[AbsBinarySearchTree[T]])
    case _ => this.asInstanceOf[AbsBinarySearchTree[T]]
  }
  
  def delete(item: T) : AbsBinarySearchTree[T] = this.asInstanceOf[AbsBinarySearchTree[T]] match {
    case BinarySearchTree(x,l,r) if (item < x) => new BinarySearchTree(x,l.delete(item).asInstanceOf[AbsBinarySearchTree[T]],r)
    case BinarySearchTree(x,l,r) if (item > x) => new BinarySearchTree(x,l,r.delete(item).asInstanceOf[AbsBinarySearchTree[T]])
    
    case BinarySearchTree(x,EmptyBinarySearchTree(),EmptyBinarySearchTree()) if (x == item) => new EmptyBinarySearchTree()
    case BinarySearchTree(x,EmptyBinarySearchTree(),r) if (x == item) => r
    case BinarySearchTree(x,l,EmptyBinarySearchTree()) if (x == item) => l
    
    case BinarySearchTree(x,l,r) if (x == item) => {var y = inorderPredecesor(l); new BinarySearchTree(y,l.delete(y).asInstanceOf[AbsBinarySearchTree[T]],r)}
  }
  
  def search(item:T) : Boolean = this.asInstanceOf[AbsBinarySearchTree[T]] match {
    case BinarySearchTree(x,l,r) if (item == x)=> true
    case BinarySearchTree(x,l,r) if (item < x) => l.search(item)
    case BinarySearchTree(x,l,r) if (item > x) => r.search(item)
  }

  protected def inorderPredecesor(t:AbsBinarySearchTree[T]) : T = t match {
    case EmptyBinarySearchTree() => throw new TreeException("No inorder predecesor in empty tree")    
    case BinarySearchTree(x,EmptyBinarySearchTree(),EmptyBinarySearchTree()) => x
    case BinarySearchTree(x,l,EmptyBinarySearchTree()) => x
    case BinarySearchTree(x,l,r) => inorderPredecesor(r)
  }
}