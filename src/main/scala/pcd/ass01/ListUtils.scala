package pcd.ass01

object ListUtils :
  def partitionByNumber[E](elems: List[E], numberOfPartitions: Int): List[List[E]] =
    object ModMatch :
      def unapply(x: Int): Option[Int] = Some(x % numberOfPartitions)
    (for
      ind <- 0 until numberOfPartitions
    yield
      elems.zipWithIndex collect:
        case (e, ModMatch(`ind`)) => e
    ).toList

  def partitionBySize[T](lst: List[T], batchSize: Int): List[List[T]] =
    partitionByNumber(lst, divideCeil(lst.size, batchSize))

  def batchLazyListByNumber[T](lst: List[T], batchNumber: Int): LazyList[List[T]] = partitionByNumber(lst, batchNumber) to LazyList

  def batchLazyListBySize[T](lst: List[T], batchSize: Int): LazyList[List[T]] = partitionBySize(lst, batchSize) to LazyList

  private def divideCeil(dividend: Int, divisor: Int) = (dividend + divisor - 1) / divisor

  @main
  def testBatchLazyList(): Unit =
    val numbers = (1 to 26).toList
    val batchSize = 10
    val batchNumber = 5

    batchLazyListBySize(numbers, batchSize) foreach :
      (batch: List[Int]) => println(s"Processing batch by size: $batch")

    batchLazyListByNumber(numbers, batchNumber).foreach:
      (batch: List[Int]) => println(s"Processing batch by number: $batch")
