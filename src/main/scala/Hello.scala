import compress._
import compress.statistic._

object Hello {
    def main(args: Array[String]) = {
        println("Hello, world")
        val test:Seq[Char]="Belle Pizza"
        val compressor = new RLE[Char]
        println(compressor.uncompress(compressor.compress(test)))
        val test2:Seq[Int]=Seq(1,1,1,1,2,3,4,3,6,6,4)
        val compressor2=new RLE[Int]
        println(compressor2.uncompress(compressor2.compress(test2)))
        /*val compressor3=new ShannonFano[Char]("aabbb")
         *println(compressor3.compress(test)) Ces commandes ne fonctionnent pas. 
         * Dans la classe ShannonFano j'ai instancié directement un arbre je n'ai pas eu le temps de faire la méthode
         * Malheuresement, la compression ne marche pas comme on peut le voir sur la ligne 13 et 14. */
}
}


