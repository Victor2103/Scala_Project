package compress.statistic

import compress.Compressor

/** A statistic compressor relies on the statistics of symbols in source
  * @param source the input source
  * @tparam S type of symbol used in source
  */
abstract class StatisticCompressor[S](source : Seq[S]) extends Compressor[S, Seq[Bit]]
  {
    
    def compter(j:Int,cpt:Int,car:S):(S,Int)={
		if (j==source.length){
			return((car,cpt))
		} else {
			if (source(j)==car){
				return(compter(j+1,cpt+1,car))
			}else {
				return(compter(j+1,cpt,car))
			}
		}
	}
    
    def occ(retour:Map[S,Int],i:Int):Map[S,Int]={
		if (i==source.length){
			return(retour)
		}else {
			if (retour.contains(source(i))){
				return(occ(retour,i+1))
			} else {
				val tmp=compter(0,0,source(i))
				return(occ(retour++Map(tmp._1->tmp._2),i+1))
			}
		}
	}
    
    
    
    /** A map giving the occurrences of each symbol in the source sequence */
    val occurrences : Map[S, Int] = occ(Map(),0)
		
	def getBaseLog(x:Int,y:Double):Double ={
		return(math.log(y) / math.log(x))
	}
	
	def calculEntropy(occur:Map[S,Int],i:Int,res:Double,N:Int):Double={
		if (i==N){
			return(-res)
		} else {
			val tmp=occur.keys.head
			return(calculEntropy(occur.drop(1),i+1,res+(occur(tmp).toDouble/source.length.toDouble*getBaseLog(2,occur(tmp).toDouble/source.length.toDouble)),N))
		}
	}
    
    
    /** SHANNON entropy of source */
    val entropy : Double = calculEntropy(occurrences,0,0,occurrences.size)

    def min(occur:Map[S,Int],mint:(S,Int),j:Int,N:Int):(S,Int)={
		if (j==N){
			return(mint)
		} else {
			val tmp=occur.keys.head
			if (occur(tmp)<mint._2){
				return(min(occur.drop(1),(tmp,occur(tmp)),j+1,N))
			} else {
				return(min(occur.drop(1),mint,j+1,N))
			}
		}
	}
    
    
    
    def tri(res:Seq[(S,Int)],i:Int,occur:Map[S,Int],N:Int):Seq[(S,Int)]={
		if (N==i){
			return(res)
		} else {
			val tmp3=min(occur,(occur.keys.head,occur(occur.keys.head)),0,occur.size)
			return(tri(res:+tmp3,i+1,occur.filterKeys(_!=tmp3._1),N))
		}
	}
    
    /** The sequence of occurrences sorted by count */
    val orderedCounts : Seq[(S, Int)] = tri(Seq(),0,occurrences,occurrences.size)

    /** The encoding tree (in most cases, depends from `source`) */
    def tree : Option[EncodingTree[S]]

    def compressAdd(msg:Seq[S],i:Int,retour:Seq[Bit]):Seq[Bit]={
		if (i==msg.length){
			return(retour)
		} else {
			val tmp:Seq[Bit]=tree.get.encode(msg(i)).get
			return(compressAdd(msg,i+1,retour++tmp))
		}
	}
    
    
    
    /** @inheritdoc */
    def compress(msg: Seq[S]): Seq[Bit] = return(compressAdd(msg,0,Seq()))

    /** @inheritdoc */
    def uncompress(res: Seq[Bit]): Option[Seq[S]] = return(Some(Seq()))
  }
