trait Weighted[A] {

  def getItems: Seq[A]
  def getWeights: Seq[Double]
  
  def sumIf(f: A => Boolean): Double = {
    //assumption, getItems.length = getWeights.length
    val items = getItems;
    val weights = getWeights;
    (for(i <- 0 until items.length) yield if(f(items(i))) weights(i) else 0.0).foldLeft(0.0){_+_}
  }
  
}
