package bowl

import org.specs2.mutable._
import org.specs2.mock._
import org.specs2.specification.Scope


/**
 * Created with IntelliJ IDEA.
 * User: k_morishita
 * Date: 2013/10/31
 * Time: 18:18
 * To change this template use File | Settings | File Templates.
 */

class SpecStreamBowling extends Specification with Mockito {
  implicit def listInt2IteratorString(list: List[Int]) = list.map(i => i.toString).toIterator

  "ALL 10" should {
    "totalScore is 300" in {
      val sb = new StreamBowling(new IOObject {
        def input: Iterator[String] = List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)

        def error(s: String): Unit = {}

        def output(s: String): Unit = {}
      })
      sb.run()
      sb.game.frame(10).totalScore must_== Some(300)
      sb.game.isFinish must beTrue
    }
  }

  "Output Timing" should {
    "no output before frame score is fixed" in new NormalCase {
      throwBowls(List(5))
      there was no(iomock).output(anyString)
    }

    "output immediately after frame score is fixed" in new NormalCase {
      throwBowls(List(5,3))
      there was one(iomock).output(anyString)
    }

    "when spare" in new NormalCase {
      throwBowls(List(6,4))
      there was no(iomock).output(anyString)
      throwBowls(List(6))
      there was one(iomock).output(anyString)
    }

    "when strike" in new NormalCase {
      throwBowls(List(10, 5))
      there was no(iomock).output(anyString)
      throwBowls(List(3))
      there was two(iomock).output(anyString)
    }

    "when strike and spare" in new NormalCase {
      throwBowls(List(10, 5))
      there was no(iomock).output(anyString)
      throwBowls(List(5))
      there was one(iomock).output(anyString)
      throwBowls(List(2))
      there was two(iomock).output(anyString)
    }
  }

  trait NormalCase extends Scope {
    val iomock = mock[IOObject]
    val game = new BowlingGame
    val sb = new StreamBowling(iomock, game)
    def throwBowls(iter: Iterator[String]) = {
      iomock.input returns(iter)
      sb.run()
    }
  }
}

