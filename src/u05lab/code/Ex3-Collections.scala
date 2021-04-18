package u05lab.code

import java.util.concurrent.TimeUnit
import scala.collection.immutable._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {
  val verbose = false

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty && verbose) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def multipleMeasure[T](msg: String)(expr: => T)(repetitions: Int = 5): MeasurementResults[T] = {
    println("----- TESTING -- " + msg + " -----")
    var totDuration = FiniteDuration(0, TimeUnit.NANOSECONDS)
    for (_ <- 1 to repetitions) {
      totDuration += measure(msg)(expr).duration
    }
    println("\tRESULTS: " + totDuration.toNanos / repetitions + " nanos, " + totDuration.toMillis / repetitions + "ms, with " + repetitions + " executions.")
    MeasurementResults(expr, totDuration / repetitions)
  }

  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

}


object CollectionsTest extends App {

  import PerformanceUtils._

  val start = 1
  val stop = 100000
  val range = start to stop

  def linearMeasurements(): Unit = {
    /* Linear sequences: List, ListBuffer */
    //creation
    var list = multipleMeasure("LIST CREATION")(range.toList)().result
    var list2 = multipleMeasure("LIST CREATION 2")(List.range(start, stop))().result
    var listBuff = multipleMeasure("LIST BUFFER CREATION")(ListBuffer.from(range))().result
    var listBuff2 = multipleMeasure("LIST BUFFER CREATION 2")(ListBuffer.range(start, stop))().result
    println()
    //reading
    multipleMeasure("LIST READING HEAD")(list.head)()
    multipleMeasure("LIST READING LAST")(list.last)()
    multipleMeasure("LIST READING SIZE")(list.size)()
    multipleMeasure("LIST CONTAINS NUMBER")(list.contains(70000))()

    multipleMeasure("LIST BUFFER READING HEAD")(listBuff.head)()
    multipleMeasure("LIST BUFFER READING LAST")(listBuff.last)()
    multipleMeasure("LIST BUFFER READING SIZE")(listBuff.size)()
    multipleMeasure("LIST BUFFER CONTAINS NUMBER")(listBuff.contains(70000))()
    println()
    //update
    multipleMeasure("LIST UPDATING APPEND")(list :+ (stop + 2))()
    multipleMeasure("LIST UPDATING PREPEND")((stop + 2) :: list)()

    multipleMeasure("LIST BUFFER UPDATING APPEND")(listBuff.append(stop + 2))()
    multipleMeasure("LIST BUFFER UPDATING PREPEND")(listBuff.prepend(stop + 2))()
    println()
    //delete
    multipleMeasure("LIST DROP ONE")(list.drop(1))()
    multipleMeasure("LIST DROP ALL")(list.drop(list.size - 1))()
    multipleMeasure("LIST DROP ONE IN MIDDLE")(list.splitAt(stop / 2)._1 +: list.splitAt(stop / 2)._2.drop(1))()

    multipleMeasure("LIST BUFFER DROP ONE")(listBuff.drop(1))()
    multipleMeasure("LIST BUFFER DROP ALL")(listBuff.drop(listBuff.size - 1))()
    multipleMeasure("LIST BUFFER DROP ONE IN MIDDLE")(listBuff.remove(stop / 2))()

  }

  def indexedMeasurements(): Unit = {
    /* Indexed sequences: Vector, Array, ArrayBuffer */
    var vector = multipleMeasure("VECTOR CREATION")(range.toVector)().result
    var array = multipleMeasure("ARRAY CREATION")(range.toArray)().result
    var arrayBuff = multipleMeasure("ARRAY BUFFER CREATION")(ArrayBuffer.from(range))().result

    println()
    //reading
    multipleMeasure("VECTOR READING HEAD")(vector.head)()
    multipleMeasure("VECTOR READING LAST")(vector.last)()
    multipleMeasure("VECTOR READING SIZE")(vector.size)()
    multipleMeasure("VECTOR CONTAINS NUMBER")(vector.contains(70000))()

    multipleMeasure("ARRAY READING HEAD")(array.head)()
    multipleMeasure("ARRAY READING LAST")(array.last)()
    multipleMeasure("ARRAY READING SIZE")(array.length)()
    multipleMeasure("ARRAY CONTAINS NUMBER")(array.contains(70000))()

    multipleMeasure("ARRAY BUFFER READING HEAD")(arrayBuff.head)()
    multipleMeasure("ARRAY BUFFER READING LAST")(arrayBuff.last)()
    multipleMeasure("ARRAY BUFFER READING SIZE")(arrayBuff.size)()
    multipleMeasure("ARRAY BUFFER CONTAINS NUMBER")(arrayBuff.contains(70000))()
    println()
    //update
    multipleMeasure("VECTOR UPDATING APPEND")(vector.appended(stop + 2))()
    multipleMeasure("VECTOR UPDATING PREPEND")(vector.prepended(stop + 2))()

    multipleMeasure("ARRAY UPDATING APPEND")(array.appended(stop + 2))()
    multipleMeasure("ARRAY UPDATING PREPEND")(array.prepended(stop + 2))()

    multipleMeasure("ARRAY BUFFER UPDATING APPEND")(arrayBuff.append(stop + 2))()
    multipleMeasure("ARRAY BUFFER UPDATING PREPEND")(arrayBuff.prepend(stop + 2))()
    println()
    //delete
    multipleMeasure("VECTOR DROP ONE")(vector.drop(1))()
    multipleMeasure("VECTOR DROP ALL")(vector.drop(vector.size - 1))()
    multipleMeasure("VECTOR DROP ONE IN MIDDLE")(vector.splitAt(stop / 2)._1 +: vector.splitAt(stop / 2)._2.drop(1))()

    multipleMeasure("ARRAY DROP ONE")(array.drop(1))()
    multipleMeasure("ARRAY DROP ALL")(array.drop(array.length - 1))()
    multipleMeasure("ARRAY DROP ONE IN MIDDLE")(array.splitAt(stop / 2)._1 +: array.splitAt(stop / 2)._2.drop(1))()

    multipleMeasure("ARRAY BUFFER DROP ONE")(arrayBuff.drop(1))()
    multipleMeasure("ARRAY BUFFER DROP ALL")(arrayBuff.drop(arrayBuff.size - 1))()
    multipleMeasure("ARRAY BUFFER DROP ONE IN MIDDLE")(arrayBuff.remove(stop / 2))()

  }

  def setMeasurements(): Unit = {
    /* Sets */
    var set = multipleMeasure("SET CREATION")(range.toSet)().result
    var setMut = multipleMeasure("MUTABLE SET CREATION")(mutable.Set.from(range))().result

    println()
    //reading
    multipleMeasure("SET READING HEAD")(set.head)()
    multipleMeasure("SET READING LAST")(set.last)()
    multipleMeasure("SET READING SIZE")(set.size)()
    multipleMeasure("SET CONTAINS NUMBER")(set.contains(70000))()

    multipleMeasure("MUTABLE SET READING HEAD")(setMut.head)()
    multipleMeasure("MUTABLE SET READING LAST")(setMut.last)()
    multipleMeasure("MUTABLE SET READING SIZE")(setMut.size)()
    multipleMeasure("MUTABLE SET CONTAINS NUMBER")(setMut.contains(70000))()
    println()
    //update
    multipleMeasure("SET UPDATING")(set + (stop + 2))()

    multipleMeasure("MUTABLE SET BUFFER UPDATING APPEND")(setMut.add(stop + 2))()
    println()
    //delete
    multipleMeasure("SET DELETE ONE")(set - 1)()

    multipleMeasure("MUTABLE SET DROP ONE")(setMut.remove(1))()

  }

  def mapMeasurements(): Unit = {
    /* Maps */
    var map = multipleMeasure("MAP CREATION")(range.map(i => i -> i).toMap)().result
    var mapMut = multipleMeasure("MUTABLE MAP CREATION")(mutable.Map.from(range.map(i => i -> i)))().result

    println()
    //reading
    multipleMeasure("MAP READING HEAD")(map.head)()
    multipleMeasure("MAP READING LAST")(map.last)()
    multipleMeasure("MAP READING SIZE")(map.size)()
    multipleMeasure("MAP CONTAINS NUMBER")(map.contains(70000))()

    multipleMeasure("MUTABLE MAP READING HEAD")(mapMut.head)()
    multipleMeasure("MUTABLE MAP READING LAST")(mapMut.last)()
    multipleMeasure("MUTABLE MAP READING SIZE")(mapMut.size)()
    multipleMeasure("MUTABLE MAP CONTAINS NUMBER")(mapMut.contains(70000))()
    println()
    //update
    multipleMeasure("MAP UPDATING")(map + ((stop / 2) -> (stop + 2)))()
    multipleMeasure("MAP INSERTING")(map + ((stop + 2) -> (stop + 2)))()

    multipleMeasure("MUTABLE MAP BUFFER UPDATING APPEND")(mapMut.put(stop / 2, stop + 2))()
    multipleMeasure("MUTABLE MAP BUFFER INSERTING APPEND")(mapMut.put(stop + 2, stop + 2))()
    println()
    //delete
    multipleMeasure("MAP DELETE ONE")(map - 1)(1)

    multipleMeasure("MUTABLE MAP DROP ONE")(mapMut.remove(1))(1)

  }

  /* Comparison */
  println("COMPARING LINEAR AND INDEXED")
  linearMeasurements()
  indexedMeasurements()

  println("COMPARING SET AND MAP")
  setMeasurements()
  mapMeasurements()

  /*val lst = (1 to 1000000).toList
  val vec = (1 to 1000000).toVector

  assert(measure("lst last") {
    lst.last
  } > measure("vec last") {
    vec.last
  })

  assert(multipleMeasure("lst last") {
    lst.last
  }(20) > multipleMeasure("vec last") {
    vec.last
  }(20))*/

}