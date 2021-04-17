package u05lab

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._
import code._

class SomeTest {


  @Test
  def testZipRight() {
    val l = List("a", "b", "c")
    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a",0),("b",1),("c",2)), l.zipRight)
  }

  @Test
  def testIncremental() {
    assert(true)
  }
}