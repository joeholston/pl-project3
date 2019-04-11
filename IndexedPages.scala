import scala.collection.mutable.Map

class IndexedPages() extends Seq[Page] with Weighted[Page] {
    var pages = List[Page]();

    def iterator = pages.iterator;
    def apply(idx: Int): Page = {
        pages(idx)
    }
    def length: Int = pages.length;

    override def getWeights() = {
        (for(page <- pages) yield 1.0).toSeq
    }

    override def getItems() = { pages }

    def add(p:Page) = {
        if(pages.filter((page:Page)=>{page.url == p.url}).length == 0){
            pages = pages :+ p;
        }
    }

    // Note that your search algorithm should use the weights of the pages and the weights of the Query to determine the most relevant pages.
    def search(q:Query): SearchResults = {
        val queryWeights = q.getWeights();
        var scores = List[(String,Int)]();
        // Get list of Search results sorted by score
        for(page <- pages){
            var pageScore = 0;
            for(word <- q.words){
                if(page.topWord() == word){
                    pageScore = pageScore + 1;
                }
            }
            scores = scores :+ (page.url, pageScore)
        }
        new SearchResults(q, pages.length, scores.map(_._1), scores.map(_._2.toDouble))
    }
}

class IndexedPagesUrlLength() extends IndexedPages(){
    override def getWeights() = {
        (for(page <- pages) yield -page.url.length.toDouble).toSeq
    }
}

class IndexedPagesLinksCount() extends IndexedPages(){
    def linkCount = Map[String,Int]();
    override def getWeights() = {
        (for(p <- pages) yield linkCount.getOrElse(p.url,0).toDouble).toSeq
    }
    override def add(p:Page) = {
        if(pages.filter((page:Page)=>{page.url == p.url}).length == 0){
            linkCount += (p.url -> linkCount.getOrElse(p.url, 0))
            pages = pages :+ p;
        }
    }
}