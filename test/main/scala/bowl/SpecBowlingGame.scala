package bowl

import org.specs2.mutable._
import org.specs2.specification.Scope


/**
 * Created with IntelliJ IDEA.
 * User: k_morishita
 * Date: 2013/10/30
 * Time: 14:09
 * To change this template use File | Settings | File Templates.
 */


class BowlingGameSpec extends Specification {
  "In the first frame" should {
    "post 2 -> firstScore == 2" in new NormalScope {
      game.post(2)
      game.frame(1).firstScore.get must_== 2
    }

    "post 2 3 -> secondScore == 3" in new NormalScope {
      game.post(2)
      game.post(3)
      game.frame(1).secondScore.get must_== 3
    }

    "post 2 3 -> frameScore == 5" in new NormalScope {
      game.post(2)
      game.post(3)
      game.frame(1).frameScore.get must_== 5
    }

    "post 2 3 -> totalScore == 5" in new NormalScope {
      game.post(2)
      game.post(3)
      game.frame(1).totalScore.get must_== 5
    }

    "post 2 -> isFix is false " in new NormalScope {
      game.post(2)
      game.frame(1).isFix must_== false
    }

    "post 2  3 -> isFix is true" in new NormalScope {
      game.post(2)
      game.post(3)
      game.frame(1).isFix must_== true
    }
  }

  "after (4, 3): second frame" should {
    "post 5 -> totalScore is None" in new SecondFrame {
      game.post(5)
      game.frame(2).totalScore must_== None
    }

    "post (4, 6) -> totalScore(2) is None" in new SecondFrame {
      for (s <- List(4, 6)) game.post(s)
      game.frame(2).frameScore must_== None
      game.frame(2).totalScore must_== None
    }

    "post (4, 6, 7) -> frame(2) is 17/24" in new SecondFrame {
      for (s <- List(4, 6, 7)) game.post(s)
      game.frame(2).frameScore must_== Some(17)
      game.frame(2).totalScore must_== Some(24)
    }

    "post (4, 6, 7, 2) -> frame(3) is 9/33" in new SecondFrame {
      for (s <- List(4, 6, 7, 2)) game.post(s)
      game.frame(3).frameScore must_== Some(9)
      game.frame(3).totalScore must_== Some(33)
    }

    "post (10, 5) -> frame(2) is None/None" in new SecondFrame {
      for (s <- List(10, 5)) game.post(s)
      game.frame(2).frameScore must_== None
      game.frame(2).totalScore must_== None
    }

    "post (10, 5, 5) -> frame(2) is 20/27" in new SecondFrame {
      for (s <- List(10, 5, 5)) game.post(s)
      game.frame(2).frameScore must_== Some(20)
      game.frame(2).totalScore must_== Some(27)
      game.frame(3).frameScore must_== None
      game.frame(3).totalScore must_== None
    }

    "post (10, 10) -> frame(2) is None/None" in new SecondFrame {
      for (s <- List(10, 10)) game.post(s)
      game.frame(2).frameScore must_== None
      game.frame(2).totalScore must_== None
    }

    "post (10, 10, 10) -> frame(2) is 30/37" in new SecondFrame {
      for (s <- List(10, 10, 10)) game.post(s)
      game.frame(2).frameScore must_== Some(30)
      game.frame(2).totalScore must_== Some(37)
    }

    "post (10, 10, 10, 8) -> frame(3) is 28/65" in new SecondFrame {
      for (s <- List(10, 10, 10, 8)) game.post(s)
      game.frame(3).frameScore must_== Some(28)
      game.frame(3).totalScore must_== Some(65)
    }
  }

  "after the 9th frame" should {
    "final frame is open" in new After9thFrame {
      for (s <- List(6, 3)) game.post(s)
      game.frame(9).frameScore must_== Some(16)
      game.frame(9).totalScore must_== Some(80)
      game.frame(10).frameScore must_== Some(9)
      game.frame(10).totalScore must_== Some(89)
      game.isFinish must_== true
    }

    "final frame is spare" in new After9thFrame {
      for (s <- List(6, 4)) game.post(s)
      game.frame(10).frameScore must_== None
      game.frame(10).totalScore must_== None
      game.isFinish must_== false
      game.post(10)
      game.frame(10).frameScore must_== Some(20)
      game.frame(10).totalScore must_== Some(100)
      game.isFinish must_== true
    }

    "final frame is strike" in new After9thFrame {
      game.post(10)
      game.frame(10).frameScore must_== None
      game.frame(10).totalScore must_== None
      game.isFinish must_== false
      game.post(10)
      game.frame(10).frameScore must_== None
      game.frame(10).totalScore must_== None
      game.isFinish must_== false
      game.post(10)
      game.frame(10).frameScore must_== Some(30)
      game.frame(10).totalScore must_== Some(114)
      game.isFinish must_== true
    }
  }

  "When Illegal Score is posted" should {
    "negative value" in new After9thFrame {
      game.post(-1) must throwAn[IllegalArgumentException]
    }

    "more than 10 value" in new After9thFrame {
      game.post(11) must throwAn[IllegalArgumentException]
    }

    "sum is more than 10 value in frame(2)" in new SecondFrame {
      game.post(7)
      game.post(4) must throwAn[IllegalArgumentException]
    }

    "sum is more than 10 value in frame(10)" in new After9thFrame {
      game.post(7)
      game.post(4) must throwAn[IllegalArgumentException]
    }

    "sum is more than 10 value in frame(10) third score" in new After9thFrame {
      game.post(10)
      game.post(3)
      game.post(8) must throwAn[IllegalArgumentException]
    }
  }
}


trait NormalScope extends Scope {
  val game = new BowlingGame
}

trait SecondFrame extends Scope {
  val game = new BowlingGame
  game.post(4)
  game.post(3)
}

trait After9thFrame extends Scope {
  val game = new BowlingGame
  //              1    2    3    4    5    6    7    8    9
  for (s <- List(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 6)) game.post(s)
}