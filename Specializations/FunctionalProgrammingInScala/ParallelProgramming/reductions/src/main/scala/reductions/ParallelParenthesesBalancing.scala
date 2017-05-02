package reductions

import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {
  @volatile var seqResult = false
  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }

    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }

    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {
  /** Returns `true` iff the parentheses in the input `chars` are balanced. */
  def balance(chars: Array[Char]): Boolean = {
    def loop(index: Int, opened: Int): Boolean = {
      if (opened < 0) false
      else if (index == chars.length) opened == 0
      else if (chars(index) == '(') loop(index + 1, opened + 1)
      else if (chars(index) == ')') loop(index + 1, opened - 1)
      else loop(index + 1, opened)
    }

    loop(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced. */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, left: Int, right: Int): (Int, Int) =
      if (idx == until) (left, right)
      else if (chars(idx) == '(') traverse(idx + 1, until, left + 1, right)
      else if (chars(idx) == ')' && left == 0) traverse(idx + 1, until, left, right + 1)
      else if (chars(idx) == ')' && left > 0) traverse(idx + 1, until, left - 1, right)
      else traverse(idx + 1, until, left, right)

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((ll, lr), (rl, rr)) = parallel(reduce(from, mid), reduce(mid, until))

        val delta = ll - rr

        if (delta > 0) (rl + delta, lr)
        else (rl, lr - delta)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }
}
