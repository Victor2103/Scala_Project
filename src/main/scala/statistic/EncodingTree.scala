package compress.statistic

/** Trait for binary encoding trees (integer-labeled binary trees)
  * @tparam S type of symbols (in leaves only)
  */
sealed abstract class EncodingTree[S](val label : Int)
  {
    
    
    def nbBits(longeur:Int,i:Int):Int={
		if (longeur<=math.pow(2,i)){
			return(i)
		} else {
			return(nbBits(longeur,i+1))
		}
	}
	
	
	def g(x:S):Seq[S]=return(Seq(x))
	def g2(x:Seq[S],y:Seq[S]):Seq[S]=return(x++y)
    val laListe= this.reduceWith[Seq[S]](g)(g2)
    val arbre1:ShannonFano[S]=new ShannonFano(laListe)
    val nombreBits:Int=nbBits(this.arbre1.occurrences.size,0)
    
    
    
    /* OPERATIONS ON VALUES */


	def test(x:S,retour:EncodingTree[S]):Boolean= retour match {
		case EncodingNode(label,left,right) => return(test(x,left) | test(x,right))
		case EncodingLeaf(label,value) => return(x==value) 
	}
    /** Checks if tree contains given value
      * @param x value to search
      * @return true if the tree has a leaf with value `x`
      */
    def has(x : S) : Boolean = return(test(x,this))

    /** Reduce operation on tree values when applying a function on leaves beforehand
      * @param f the function applied to each leaf value
      * @param op the aggregation operation on a node
      * @tparam U the result type of `f`
      * @return the aggregated value of the tree
      */
    def reduceWith[U](f : S => U)(op : (U, U) => U) : U = this match
      {
        case EncodingLeaf(_, v   ) => f(v)
        case EncodingNode(_, l, r) => op((l reduceWith f)(op), (r reduceWith f)(op))
      }

    /** Reduce operation on tree values
      *
      * `t reduce op` is a shorthand for `(t reduceWith {v => v})(op)`
      * @param op the aggregation operation on a node
      * @return the aggregated value of the tree
      */
    def reduce(op : (S, S) => S) : S = (this reduceWith {v => v})(op)


    /* OPERATIONS ON LABELS */

    /** Reduce operation on tree labels when applying a function on leaves beforehand
      * @param fL the function applied to each leaf label
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @tparam A the result type of `f`
      * @return the result of aggregation operation recursively applied to tree
      */
    def reduceLabelWith[A](fL : Int => A)(opL : (Int, A, A) => A) : A = this match
      {
        case EncodingLeaf(lbl, _   ) => fL(lbl)
        case EncodingNode(lbl, l, r) => opL(lbl, (l reduceLabelWith fL)(opL), (r reduceLabelWith fL)(opL))
      }

    /** Reduce operation on tree labels
      *
      * `t reduceLabel opL` is a shorthand for `(t reduceLabelWith {lbl => lbl})(opL)`
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @return the aggregated label of the tree
      */
    def reduceLabel(opL : (Int, Int, Int) => Int) : Int = (this reduceLabelWith identity)(opL)


    /* ENCONDING/DECODING OPERATIONS */

    /** Computes the bit sequence corresponding to a tentative leaf value.
      * @param x value to encode
      * @return the corresponding bit sequence of `x` is a leaf of encoding tree, `None` otherwise
      */
    def creation(i:Int,n:Int,res:Seq[String]):Seq[String]={
		if (i==n){
			return(res)
		} else {
			val tmp=i.toBinaryString
			return(creation(i+1,n,res:+tmp))
		}
	}
    
    def stringEnBit(debut:String,i:Int,res:Seq[Bit]):Seq[Bit]={
		if (i==debut.length){
			return(res)
		} else {
			if (debut(i)=='0'){
				return(stringEnBit(debut,i+1,res:+Zero))
			} else {
				return(stringEnBit(debut,i+1,res:+One))
			}
		}
	}
		
		
		
    
    def transformation(debut:Seq[String],i:Int):Seq[String]={
		if (i==debut.length){
			return(debut)
		} else {
			if (debut(i).length==nombreBits){
				return(transformation(debut,i+1))
			} else {
				val tmp='0'+debut(i)
				return(transformation(debut.patch(i,Seq(tmp),1),i))
			}
		}
	}
    
    def passageEnBinaire(longeur:Int,i:Int,res:Seq[String]):Seq[String]={
		if (i==longeur){
			return(res)
		} else {
			return(passageEnBinaire(longeur,i+1,res:+i.toBinaryString))
		}
	}   
    
    
    def recup(x:S,i:Int):Int={
		if (this.arbre1.orderedCounts(i)._1==x){
			return(i)
		} else {
			return(recup(x,i+1))
		}
	}
    
    
    val sequenceEnBits:Seq[String]=transformation(passageEnBinaire(this.arbre1.orderedCounts.length,0,Seq()),0)
    
    def encode(x : S) : Option[Seq[Bit]] = {
		if (this.has(x)){
			val tmp2=recup(x,0)
			return(Some(stringEnBit(sequenceEnBits(tmp2),0,Seq())))
		} else {
			return(None)
		}
	}
			

    /** Computes the next value corresponding to the beginning of bit sequence (if possible)
      * @param res the bit sequence to decode
      * @return the decoded value and the bit sequence left to be decoded or `None` if current bit sequence does not lead to a leaf in enconding tree
      */
    def search(sequence:Seq[Bit],i:Int):Option[Int]={
		if (i==sequence.length){
			return(None)
		} else {
			if (stringEnBit(this.sequenceEnBits(i),0,Seq())==sequence){
				return(Some(i))
			}else {
				return(search(sequence,i+1))
			}
		}
	}
    
    
    def creationSeqBit(debut:Seq[Bit],i:Int,res:Seq[Bit]):Seq[Bit]={
		if (i==nombreBits){
			return(res)
		}else {
			return(creationSeqBit(debut,i+1,res:+debut(i)))
		}
	}
    
    
    
    def decodeOnce(res : Seq[Bit]) : Option[(S, Seq[Bit])] = {
		if (res.length<nombreBits){
			return(None)
		} else {
			val tmp=creationSeqBit(res,0,Seq())
			if (search(tmp,0)==None){
				return(None)
			} else {
				val tmp2=search(tmp,0).get
				return(Some((this.arbre1.orderedCounts(tmp2)._1,tmp)))
			}
		}
	}

   
    
    
    
    def decodeAdd(res : Seq[Bit],retour:Seq[S]) : Option[Seq[S]] = {
		if (res.length==0){return(Some(retour))}
		if (res.length%nombreBits!=0){
			return(None)
		} else {
			val tmp=decodeOnce(res)
			if (tmp==None){
				return(None)
			}else {
				return(decodeAdd(res.drop(0),retour:+tmp.get._1))
			}
		}
	}
	
	
	 /** Computes the sequence of values from the sequence of bits
      * @param res the bit sequence to decode
      * @return the sequence of decoded values or `None` otherwise
      */
	def decode(res:Seq[Bit]):Option[Seq[S]]={
		return(decodeAdd(res,Seq()))
	}

    /* MISCELLANEOUS */
	

    
    def calculLongeur(test:Seq[S],test2:Seq[(S,Int)],i:Int,res:Double):Double={
		if (test2.length==i){
			return(res)
		} else {
			return(calculLongeur(test,test2,i+1,res+test2(i)._2.toDouble/test.length.toDouble*nombreBits))
		}
	}
	
	
	/** Mean length of code associated to encoding tree */
	lazy val meanLength : Double = calculLongeur(this.laListe,this.arbre1.orderedCounts,0,0)
		

    /** @inheritdoc */
    override def toString : String = this match
     {
       case EncodingLeaf(lbl, v   ) => (v, lbl).toString()
       case EncodingNode(lbl, l, r) => s"EncodingNode([$lbl], $l, $r)"
     }
  }
case class EncodingNode[S](override val label : Int, left : EncodingTree[S], right : EncodingTree[S]) extends EncodingTree[S](label)
case class EncodingLeaf[S](override val label : Int, value : S) extends EncodingTree[S](label)

