class Query(val words: Seq[String]) extends Weighted[String]{
    
    override def getItems() = { words }

    override def getWeights() = {
        (for(word <- words) yield 1.0).toSeq
    }
}

class QueryLength(override val words: Seq[String]) extends Query(words){
    override def getWeights() = {
        (for(word <- words) yield word.length.toDouble).toSeq
    }
}