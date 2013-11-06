package bowl

/**
 * Created with IntelliJ IDEA.
 * User: k_morishita
 * Date: 2013/10/29
 * Time: 17:13
 * To change this template use File | Settings | File Templates.
 */

object main {
  def main(args: Array[String]) {
    val sb = new StreamBowling(new IOObject {
      def input: Iterator[String] = io.Source.stdin.getLines()

      def error(s: String): Unit = Console.err.println(s)

      def output(s: String): Unit = Console.out.println(s)
    })
    sb.run()
  }
}

trait IOObject {
  def input: Iterator[String]

  def output(s: String): Unit

  def error(s: String): Unit
}

class StreamBowling(val io: IOObject, val game: BowlingGame = new BowlingGame) {
  def run() = while (game.currentFrame != None && io.input.hasNext) try {
    val startCurrentFrame = game.frames.find(!_.isFix).get
    game.post(io.input.next().toInt)
    printFrameInfoTillNotFixFrame(startCurrentFrame)
  }
  catch {
    case e: IllegalArgumentException => io.error("スコアが不正です。正しいスコアを入力してください。")
    case n: NumberFormatException => // NOP
  }

  def printFrameInfoTillNotFixFrame(frame: Frame): Unit = {
    if (frame != null && frame.isFix) {
      io.output(s"${frame.frameNo} ${frame.frameScore.get} ${frame.totalScore.get}")
      printFrameInfoTillNotFixFrame(frame.nextFrame)
    }
  }
}

class BowlingGame {
  protected var firstFrame: Frame = new Frame(1, null)
  protected val frameTable: Array[Frame] = new Array[Frame](11)

  def frames: Iterator[Frame] = (for (i <- Range(1, 11)) yield frame(i)).iterator

  def currentFrame: Option[Frame] = frames.collectFirst {
    case f if !f.isFix => f
  }

  def post(score: Int) = {
    require(0 <= score && score <= 10)
    firstFrame.post(score)
  }

  def frame(i: Int): Frame = {
    require(1 <= i && i <= 10)
    frameTable(i) match {
      case _: Frame => // NOP
      case _ => frameTable(i) = takeFrame(i)
    }
    frameTable(i)
  }

  def isFinish: Boolean = frame(10).isFix

  private def takeFrame(i: Int, f: Frame = firstFrame): Frame = i match {
    case 1 => f
    case _ => takeFrame(i - 1, f.nextFrame)
  }
}

class Frame(val frameNo: Int, val prevFrame: Frame) {
  var score1: Option[Int] = None
  var score2: Option[Int] = None

  val nextFrame: Frame = frameNo match {
    case 10 => null
    case 9 => new FinalFrame(frameNo + 1, this)
    case _ => new Frame(frameNo + 1, this)
  }

  def post(score: Int): Unit = isDone match {
    case true => nextFrame.post(score)
    case false => (score1, score2) match {
      case (None, _) => score1 = Some(score)
      case (Some(x), None) if x + score <= 10 => score2 = Some(score)
      case _ => throw new IllegalArgumentException
    }
  }

  def firstScore: Option[Int] = score1

  def secondScore: Option[Int] = (score1, score2) match {
    case (Some(10), _) => nextFrame.firstScore
    case _ => score2
  }

  def frameScore: Option[Int] = (score1, score2) match {
    case (Some(10), _) => sumList(List(score1, nextFrame.firstScore, nextFrame.secondScore))
    case (Some(x), Some(y)) if x + y == 10 => sumList(List(score1, score2, nextFrame.firstScore))
    case (Some(x), Some(y)) => Some(x + y)
    case _ => None
  }

  protected def sumList(list: List[Option[Int]]): Option[Int] = {
    if (list.flatten.length == list.length) {
      Some(list.flatten.sum)
    } else {
      None
    }
  }

  def totalScore: Option[Int] = prevFrame match {
    case _: Frame => sumList(List(prevFrame.totalScore, frameScore))
    case _ => frameScore
  }

  def isFix: Boolean = frameScore.isDefined

  def isDone: Boolean = (score1, score2) match {
    case (Some(10), _) => true
    case (Some(x), Some(y)) => true
    case _ => false
  }
}

class FinalFrame(frameNo: Int, prevFrame: Frame) extends Frame(frameNo, prevFrame) {
  var score3: Option[Int] = None

  override def post(score: Int): Unit = {
    require(!isDone, "This game was over!")
    (score1, score2, score3) match {
      case (None, _, _) => score1 = Some(score)
      case (Some(10), None, _) => score2 = Some(score)
      case (Some(x), None, _) if x + score <= 10 => score2 = Some(score)
      case (Some(10), Some(10), None) => score3 = Some(score)
      case (Some(10), Some(y), None) if y + score <= 10 => score3 = Some(score)
      case (Some(x), Some(y), None) if x != 10 => score3 = Some(score)
      case _ => throw new IllegalArgumentException
    }
  }

  override def secondScore: Option[Int] = score2

  override def frameScore: Option[Int] = isDone match {
    case true => Some(score1.get + score2.get + score3.getOrElse(0))
    case _ => None
  }

  override def isFix: Boolean = isDone

  override def isDone: Boolean = (score1, score2, score3) match {
    case (_, _, Some(x)) => true
    case (Some(x), Some(y), _) if x + y < 10 => true
    case _ => false
  }
}

class InvalidPostException extends Exception
