package example

trait Variance {
  def mean(list: Seq[Double]): Option[Double] = if(list.nonEmpty) Some(list.sum/list.size) else None
  def variance(seq: Seq[Double]): Option[Double] = mean(seq) flatMap(m => mean(seq.map(x => Math.pow(x-m,2))))
}

object Variance extends Variance
