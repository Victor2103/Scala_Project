package compress.statistic

/** The SHANNON-FANO compression method */
class ShannonFano[S](source : Seq[S]) extends StatisticCompressor[S](source)
  {
    
    val arbre=EncodingLeaf[S](2,this.source(0))
    val arbre2=EncodingLeaf[S](3,this.source(1))
    val arbre3=EncodingNode[S](5,arbre,arbre2)
    
    
    /** @inheritdoc */
    lazy val tree : Option[EncodingTree[S]] = Some(arbre3)
  }
