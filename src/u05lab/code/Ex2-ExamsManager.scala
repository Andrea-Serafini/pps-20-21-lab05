package u05lab.code

object ExamsManagerTest extends App {

  /* See: https://bitbucket.org/mviroli/oop2018-esami/src/master/a01b/e1/Test.java */

  sealed trait Kind {
    override def toString: String = this.getClass.getSimpleName
  }

  sealed trait ExamResult {

    def getKind: Kind

    def getEvaluation: Option[Int]

    def cumLaude(): Boolean

    override def toString: String = getKind.toString match {
      case "SUCCEEDED" => "SUCCEEDED(" + getEvaluation.get + (if (cumLaude()) "L)" else ")")
      case _ => getKind.toString
    }

    object Kind {
      case class RETIRED() extends Kind

      case class FAILED() extends Kind

      case class SUCCEEDED() extends Kind
    }
  }

  case class SuccExamResult(value: Int, laude: Boolean) extends ExamResult{
    override def getKind: Kind = Kind.SUCCEEDED()

    override def getEvaluation: Option[Int] = Option(value)

    override def cumLaude(): Boolean = laude
  }
  case class FailExamResult() extends ExamResult{
    override def getKind: Kind = Kind.FAILED()

    override def getEvaluation: Option[Int] = Option.empty

    override def cumLaude(): Boolean = false
  }

  case class RetirExamResult() extends ExamResult{
    override def getKind: Kind = Kind.RETIRED()

    override def getEvaluation: Option[Int] = Option.empty

    override def cumLaude(): Boolean = false
  }

  sealed trait ExamResultFactory {
    def succeeded(i: Int): ExamResult

    def succeededCumLaude(): ExamResult

    def retired(): ExamResult

    def failed(): ExamResult

  }

  sealed trait ExamsManager {

    def createNewCall(call: String): Unit

    def addStudentResult(call: String, student: String, result: ExamsManagerTest.ExamResult): Unit

    def getAllStudentsFromCall(call: String): Set[String]

    def getEvaluationsMapFromCall(call: String): Map[String, Int]

    def getResultsMapFromStudent(student: String): Map[String, String]

    def getBestResultFromStudent(student: String): Option[Int]
  }

  object ExamResultFactory {
    def apply(): ExamResultFactory = ExamResultFactoryImpl()

    case class ExamResultFactoryImpl() extends ExamResultFactory {
      override def succeeded(i: Int): ExamResult = {
        require(i <= 30 && i >= 18)
        SuccExamResult(i,false)
      }
      override def succeededCumLaude(): ExamResult = SuccExamResult(30,true)
      override def retired(): ExamResult = RetirExamResult()
      override def failed(): ExamResult = FailExamResult()
    }
  }

  object ExamsManager {
    def apply(): ExamsManager = ExamsManagerImpl()

    case class ExamsManagerImpl() extends ExamsManager {

      var calls: Map[String, Map[String, ExamResult]] = Map.empty

      override def createNewCall(call: String): Unit = calls += (call -> Map.empty)

      override def addStudentResult(call: String, student: String, result: ExamResult): Unit = calls += call -> (calls(call) + (student -> result))

      override def getAllStudentsFromCall(call: String): Set[String] = if (!calls.contains(call)) Set.empty else calls(call).keySet

      override def getEvaluationsMapFromCall(call: String): Map[String, Int] = if (!calls.contains(call)) {
        Map.empty
      } else {
        calls(call).filter(a => a._2.getKind.toString == "SUCCEEDED") map (a => (a._1, a._2.getEvaluation.get))
      }

      override def getResultsMapFromStudent(student: String): Map[String, String] = calls.filter(_._2.contains(student)).map(a=>a._1->a._2(student).toString)

      /*{
        var res: Map[String, String] = Map.empty
        calls foreach (c => {
          if (c._2.contains(student)) {
            res += (c._1 -> c._2(student).toString)
          }
        })
        res
      }*/

      override def getBestResultFromStudent(student: String): Option[Int] = calls.filter(_._2.contains(student)).map(_._2(student).getEvaluation).max
      /*{
        var res: Set[Option[Int]] = Set.empty
        calls foreach (c => {
          if (c._2.contains(student)) {
            res += c._2(student).getEvaluation
          }
        })
        if (res.isEmpty) {
          Option.empty
        } else {
          var option: Option[Int] = Option.empty
          res foreach (e => {
            if (option.isEmpty && e.nonEmpty) {
              option = e
            } else if (e.nonEmpty && option.nonEmpty)
              if (e.get > option.get) {
                option = e
              }
          })
          option
        }
      }*/
    }
  }
}