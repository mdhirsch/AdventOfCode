import jdk.internal.util.xml.impl.Input

object Day2 {
  def computeRatioChecksum(input: String): Int = {
    val lines = parseInput(input)
    lines.map(l => computeIntegerRatio(l)).sum
  }

  def computeMinMaxChecksum(input: String): Int = {
    val lines = parseInput(input)
    lines.map(l => findMaxDifference(l)).sum
  }

  def computeIntegerRatio(nums: List[Int]): Int = {
    computeIntegerRatio(nums.head, nums.tail)
  }
  
  @annotation.tailrec
  def computeIntegerRatio(n:Int, tail: List[Int]): Int = {
    {
      val multiple: List[Int] = tail.filter(m => n%m == 0).take(1)
      if (multiple.nonEmpty) return n / multiple.head
    }
    {
      val multiple: List[Int] = tail.filter(m => m % n == 0).take(1)
      if (multiple.nonEmpty) return multiple.head / n
    }
    computeIntegerRatio(tail.head, tail.tail)
  }


  def findMaxDifference(nums: List[Int]): Int = {
    val minMax: (Int, Int) = findMinAndMax(nums)
    minMax._2 - minMax._1
  }

  def findMinAndMax(nums: List[Int]): (Int, Int) = {
    (nums.min, nums.max)
  }

  
  def parseInput(input: String): List[List[Int]] = {
    val lines = input.split('\n')
    lines.map(l => parseRow(l)).toList
  }


  def parseRow(str: String) : List[Int] = {
    val strings = str.split("[ \t]")
    strings.map(x => x.toInt).toList
  }

}
