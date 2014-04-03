/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   Contributed by Rex Kerr
   (inspired by the C++ version by Andrew Moon)
   Updated for Scala 2.10 by Mark Hammons
*/

import akka.actor.ActorSystem

import concurrent.{Await, ExecutionContext, Future}
import concurrent.duration.Duration


import java.io._

object knucleotide {
  val BlockSize = 1024*1024

  // Iterators are not specialized so we need our own
  abstract class LongIter {
    def hasNext: Boolean
    def next: Long
    def foreach(f: Long => Unit) { while (hasNext) f(next) }
  }

  val table = Array.tabulate[Byte](256) {
    case 'a' | 'A' => 0
    case 't' | 'T' => 1
    case 'g' | 'G' => 2
    case 'c' | 'C' => 3
    case '\n' => -3
    case '>' => -2
    case _ => -1
  }

  // More efficient to store DNA sequence data as bits instead of bytes
  class Bits(val data: Array[Int]) {
    self =>
    var size = 0
    var index = 0
    var n = 0

    def add2(b: Byte) {
      size += 1
      if (n>30) { index += 1; n = 0 }
      data(index) |= (b&0x3)<<n
      n += 2
    }

    def addLots(bs: Bits) {
      if (n==0 || n>30) {
        if (n>30) { index += 1; n = 0 }
        System.arraycopy(bs.data,0,data,index,bs.index)
        index += bs.index
        if (index > 0 && n == 0) { index -= 1; n = 32 }
      }
      else {
        var i = 0
        while (i < bs.index) {
          val j = bs.data(i)
          data(index) |= j << n
          index += 1
          data(index) |= j >>> (32-n)
          i += 1
        }
        size
      }
      size += bs.index*16
      if (bs.n != 0) {
        var n = bs.n
        var i = bs.data(bs.index)
        while (n > 0) {
          add2( i.toByte )
          i >>>= 2
          n -= 2
        }
      }
    }

    def scan(n: Int, offset: Int) = new LongIter {
      var i = offset % 16
      var j = offset / 16
      val mask = (1L << (2*n)) - 1
      def hasNext = j*16 + i + n <= self.size
      def next = {
        if (i+n <= 16) {
          val l = ((data(j) >>> (2*i)) & mask)
          i += n
          if (i>=16) { j += 1; i -= 16 }
          l
        }
        else {
          val l = (((data(j) >>> (2*i))).toLong | (data(j+1).toLong << 2*(16-i))) & mask
          j += 1
          i += n - 16
          if (i>=16) { j += 1; i -= 16 }
          l
        }
      }
    }
  }

  // Load a UTF-8 DNA file from standard in, picking out requested sequence
  def load(is: InputStream, target: Array[Byte]) = {
    var need = 1
    var found,nl,done = false
    def read: Bits = {
      val data = new Array[Byte](BlockSize)
      val n = is.read(data)
      var i = 0
      while (i<n && need<target.length) {
        if (data(i)==target(need)) need += 1 else need = 0
        i += 1
      }
      if (need >= target.length && !found) {
        while (i<n && data(i)!='\n') i += 1
        if (i<n) found = true
      }
      if (found && !done)
      {
        val bits = new Bits(new Array[Int](1+((n-i)>>4)))
        while (i < n) {
          val x = table(data(i)&0xFF)
          if (x >= 0) { bits.add2(x); nl = false }
          else if (x == -3) nl = true
          else if (nl && x == -2) { i = n; done = true }
          i += 1
        }
        bits
      }
      else if (n==BlockSize && !done) read
      else new Bits(new Array[Int](0))
    }
    val data = Iterator.continually(read).takeWhile(_.size > 0).toArray
    val all = new Bits(new Array[Int](data.map(_.size).sum/16+1))
    data.foreach(all.addLots)
    all
  }

  // Utility to go from binary to text representation
  val decode = Map(0L->"A", 1L->"T", 2L->"G", 3L->"C")
  def l2s(l: Long, n: Int): String = {
    if (n <= 0) ""
    else decode(l&0x3) + l2s(l>>>2, n-1)
  }

  // Custom counted hash set (neither Java nor Scala provides one)
  class DnaHash(z: Int) {
    var size = 16
    var n = 0
    var keys = new Array[Long](size)
    var counts = new Array[Int](size)
    final def hc(l: Long) = (l.toInt + (l>>17).toInt) & (size-1)
    final def nx(i: Int) = (i+1) & (size - 1)
    def +=(key: Long, count: Int = 1) {
      val index = hc(key)
      if (counts(index) == 0) {
        keys(index) = key
        counts(index) = count
        n += 1
      }
      else if (keys(index) == key) counts(index) += count
      else if (6*n > size) {
        val (oldk, oldc, olds) = (keys, counts, size)
        size *= 2
        keys = new Array[Long](size)
        counts = new Array[Int](size)
        n = 0
        var i = 0
        while (i < olds) {
          if (oldc(i) > 0) this += (oldk(i), oldc(i))
          i += 1
        }
        this += key
      }
      else {
        var i = nx(index)
        while (counts(i) != 0 && keys(i) != key) i = nx(i)
        if (counts(i) == 0) {
          keys(i) = key
          counts(i) = count
          n += 1
        }
        else counts(i) += count
      }
    }
    def apply(key: Long) = {
      var i = hc(key)
      while (counts(i) > 0 && keys(i) != key) i = nx(i)
      counts(i)
    }
    def printSorted {
      val factor = 100.0/counts.sum
      (counts.map(_*factor) zip keys.map(l2s(_,z))).filter(_._1 > 0).sortWith((a,b) =>
        a._1 > b._1 || (a._1 == b._1 && a._2 < b._2)
      ).foreach{ case (freq, label) => printf("%s %.3f\n",label,freq) }
      println
    }
    def print(s: String) {
      val key = s.getBytes.map(x => table(x & 0xFF).toLong).reduceRight((l,r) => 4*r + l)
      printf("%d\t%s\n",this(key),s)
    }
  }

  // Required function that adds data with offset to hash set
  def addToHash(data: Bits, hash: DnaHash, n: Int, offset: Int) = data.scan(n,offset).foreach(hash += _)

  def main(args: Array[String]) {
    val as = ActorSystem.create("futures")
    implicit val ec: ExecutionContext = as.dispatcher
    val sizes = List(1,2,3,4,6,12,18)
    val sequence = "GGTATTTTAATTTATAGT"
    val data = load(System.in, "\n>THREE".getBytes)
    val answers = sizes.map(n => n -> Future {
      val h = new DnaHash(n)
      for (i <- 0 until n) addToHash(data,h,n,i)
      h
    }).toMap
    Await.result(answers(1), Duration.Inf).printSorted
    Await.result(answers(2), Duration.Inf).printSorted
    sizes.drop(2).foreach(n => Await.result(answers(n), Duration.Inf).print(sequence.substring(0,n)))

    as.shutdown()
  }
}
