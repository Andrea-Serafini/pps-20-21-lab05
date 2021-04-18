package u05lab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u05lab.code.Ex4Optional.sequence
import scala.collection.immutable.List


class Ex4OptionalTest {

  @Test
  def test() {
    assertEquals(Some(List(1, 2, 3)),  sequence(List(Some(1),Some(2),Some(3))))
    assertEquals(None, sequence(List(Some(1),None,Some(3))))
  }

}