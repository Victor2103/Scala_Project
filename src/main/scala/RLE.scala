package compress {

/** The Run-length encoding compression method */
class RLE[T] extends Compressor[T, Seq[(T, Int)]]
  {
    /** @inheritdoc */
    def Ajouter(msg:Seq[T],cpt:Int,i:Int):((T,Int),Int)={
		if (i+1==msg.length){
			return(((msg(i),cpt),i))
		} else {
			if (msg(i)!=msg(i+1)){
				return(((msg(i),cpt),i))
			}else {
				return(Ajouter(msg,cpt+1,i+1))
			}
		}
	}
	
    
    
    
    def compress_add(msg:Seq[T],retour:Seq[(T,Int)],i:Int):Seq[(T,Int)]={
		if (msg.length==i){
			return(retour)
		} else {
			val tmp=Ajouter(msg,1,i)
			return(compress_add(msg,retour:+tmp._1,tmp._2+1))
		}
	}
    
    
    
    def compress(msg : Seq[T]) : Seq[(T, Int)] = {
		return(compress_add(msg,Seq(),0))
	}
				

    def creer(tup:(T,Int),retour:Seq[T]):Seq[T]={
		if (tup._2==0){
			return(retour)
		}else {
			return(creer((tup._1,tup._2-1),retour:+tup._1))
		}
	}
    
    def uncompress_add(seq : Seq[(T,Int)],i:Int,retour:Seq[T]):Seq[T]={
		if (seq.length==i) {
			return(retour)
		} else {
			val tmp=creer(seq(i),retour) 
			return(uncompress_add(seq,i+1,tmp))
		}
	}
			
    
    
    /** @inheritdoc */
    def uncompress(seq : Seq[(T, Int)]) : Option[Seq[T]] = {
		return(Option(uncompress_add(seq,0,Seq())))
  }
}

}
