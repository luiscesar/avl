package datastructure.tree.binaryTree


abstract class AbsBinaryTree[T] {
  def insert(item:T) : AbsBinaryTree[T]
  def delete(item:T) : AbsBinaryTree[T]
  def search(item:T) : Boolean
}

case class EmptyBinaryTree[T]() extends AbsBinaryTree[T] {
  
  override def toString = "[]"
  def insert(item:T) : AbsBinaryTree[T] = new BinaryTree(item,new EmptyBinaryTree[T],EmptyBinaryTree[T])
  //def delete(item: T) : AbsBinaryTree[T] = throw new TreeException("Cannot delete from empty tree")
  def delete(item: T) : AbsBinaryTree[T] = this.asInstanceOf[AbsBinaryTree[T]]
  def search(item:T) : Boolean = false
}

case class BinaryTree[T](v:T, var left:AbsBinaryTree[T], var right:AbsBinaryTree[T]) extends AbsBinaryTree[T] {
  
  override def toString = "BinT["+v+","+left+","+right+"]"
  val random = scala.util.Random 
  
  def insert(item:T) : AbsBinaryTree[T] = this.asInstanceOf[AbsBinaryTree[T]] match {
    case EmptyBinaryTree() => new BinaryTree(item,new EmptyBinaryTree[T](),EmptyBinaryTree[T]()) 
    case BinaryTree(x,l,r) if (search(item)) => this.asInstanceOf[AbsBinaryTree[T]]
    case BinaryTree(x,l,r) => randomInsert(random.nextBoolean(),item,x,l,r)
  }

  private def randomInsert(condition: Boolean, item: T, x:T, l:AbsBinaryTree[T], r: AbsBinaryTree[T]) : 
      AbsBinaryTree[T] = condition match {
    case true => new BinaryTree(item,new BinaryTree(x,l,new EmptyBinaryTree()),r)
    case false => new BinaryTree(item,l,new BinaryTree(x,r,new EmptyBinaryTree()))
  }
  
  def delete(item: T) : AbsBinaryTree[T] = this.asInstanceOf[AbsBinaryTree[T]] match {
    case _ if (!this.search(item)) => this.asInstanceOf[AbsBinaryTree[T]]
    case BinaryTree(x,l,r) if l.search(item) => new BinaryTree(x,l.delete(item),r)
    case BinaryTree(x,l,r) if r.search(item) => new BinaryTree(x,l,r.delete(item))
  }
  
  def search(item:T) : Boolean = this.asInstanceOf[AbsBinaryTree[T]] match {
    case BinaryTree(x,l,r) if (x == item) => true
    case BinaryTree(x,l,r) => {val found = l.search(item); if (!found) r.search(item) else true}  
  }

}
