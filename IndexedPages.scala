import scala.collection.mutable.Map

class IndexedPages() extends Seq[Page] with Weighted[Page] {
    var pages = List[Page]();
    var docsWithWord = Map[String,Int]();

    def iterator = pages.iterator;
    def apply(idx: Int): Page = {
        pages(idx)
    }
    def length: Int = pages.length;

    override def getWeights():Seq[Double] = {
        (for(page <- pages) yield 1.0).toSeq
    }

    override def getItems() = { pages }

    def add(p:Page) = {
        // If the page has not been indexed before
        if(pages.filter((page:Page)=>{page.url == p.url}).length == 0){
            // Add the page to the list
            pages = pages :+ p;

            // Add the word counts to our list
            val wordCounts = p.wordCounts()
            wordCounts.map(_._1).foreach((x)=>{
                docsWithWord += (x -> (docsWithWord.getOrElse(x,0)+1))
            })
        }
    }

    // Note that your search algorithm should use the weights of the pages and the weights of the Query to determine the most relevant pages.
    def search(q:Query): SearchResults = {
        val queryWeights = q.getWeights();
        val pageWeights = getWeights();
        var scores = List[(String,Double)]();
        // Get list of Search results sorted by score
        for(i <- 0 until pages.length){
            val page = pages(i)
            var pageScore = 1.0;
            val wordCounts = page.wordCounts();
            for(i <- 0 until q.words.length){
                val word = q.words(i)

                val tf = math.log(1 + wordCounts.getOrElse(word,0)).toDouble;
                val idf = math.log(pages.length/(1 + docsWithWord.getOrElse(word,0)).toDouble).toDouble;

                pageScore += queryWeights(i) * tf * idf
            }
            // Add page weight
            pageScore = pageScore * pageWeights(i);
            
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