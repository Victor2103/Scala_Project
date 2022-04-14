import main._

object Hello {
    def main(args: Array[String]) = {
        println("Hello, world")
        val test:Seq[Char]="Belle Pizza"
        val compressor = new RLE[Char]
        println(compressor.uncompress(compress(test)))
}
}


