object Matrix_Problems {

  //MergeAndReduce

  def mergeReduce(a: List[Int], b: List[Int],mergeFunc: (Int, Int) => Int,reduceFunc: (Int,Int) => Int): Int = {
  
   //mergeHelper with the help of mergeFunc will convert the two list to a single List
    def mergeHelper(a: List[Int], b: List[Int], acc: List[Int] = List[Int]()): List[Int] = {
      (a,b) match {
        case (Nil, Nil) => acc
        case (headA::tailB,Nil) => acc
        case (Nil,headB::tailB) => acc
        case (headA :: tailA, headB :: tailB) => mergeHelper(tailA, tailB, mergeFunc(headA, headB) :: acc)
      }
    }
   //reduceHelper using reduce function will convert the List to Int
    def reduceHelper(a: List[Int]): Int  = {
      a match {
        case Nil => 0
        case head :: Nil => head
        case head :: tail => reduceFunc(head, reduceHelper(tail))
      }
    }
    reduceHelper(mergeHelper(a,b))
  }

  //Transpose

  type Row = List[Int]
  type Matrix = List[Row]
  
  //row function will return the head of the lists of list untill Matrix m is empty
  def row(m: Matrix): Row = {
    if (m.isEmpty) Nil
    else m.head.head :: row(m.tail)
  }
  
  //Col Matrix will return the tail of the lists of list untill Matrix m is Empty
  def col(m: Matrix): Matrix={
    if(m.isEmpty) Nil
    else m.head.tail :: col(m.tail)
  }

  //transpose function calls row and col function
  def transpose(m: Matrix): Matrix = {
    if(m.isEmpty) Nil
    else if(m.head.isEmpty) Nil
    else row(m) :: transpose(col(m))
  }
 
//Matrix Multiplication

//dotProd will help to multiply head of the two list and add it until List a is Empty
def dotProd(a: List[Int], b: List[Int]): Int = {
		 if (a.isEmpty) 0
		else
			a.head * b.head + dotProd(a.tail, b.tail)	
}

def multiply(a:Matrix,b:Matrix):Matrix={

        //multiplyRows will use dotProd to multiply head of Matrix a 
		//and head of Matrix b, continue until Matrix b is Empty.
        def multiplyRows(a:Row,b:Matrix):Row={
		  if(b.isEmpty) Nil
		  else dotProd(a,b.head) :: multiplyRows(a,b.tail)
		}
        //eachRow will call multiplyRows with head of just Matrix a
		//and whole of Matrix b, continue until Matrix a is Empty.
        def eachRow(a:Matrix,b:Matrix):Matrix={
          if(a.isEmpty) Nil
          else multiplyRows(a.head,b) :: eachRow(a.tail,b)
	 
    }
	
	eachRow(a,b.transpose)	
}
}