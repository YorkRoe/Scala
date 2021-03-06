package circuits

object Simulation {
  def main(args: Array[String]): Unit = {

  }
}

abstract class Simulation{

  type Action = () => Unit

  case class WorkItem(time:Int, action: Action)

  private var curtime = 0
  def currentTime: Int = curtime

  private var agenda: List[WorkItem] = List()

  private def insert(ag: List[WorkItem], item: WorkItem):List[WorkItem] = {
    if (ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)
  }

  def afterDelay(delay: Int)(block: => Unit) = {
    val item = WorkItem(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def next() = {
    (agenda: @unchecked) match {
      case item :: rest =>
        agenda = rest
        curtime = item.time
        item.action()
    }
  }

  def run() ={
    afterDelay(0){
      println("*** simulation started, time = " + currentTime + " ***")
    }
    while (!agenda.isEmpty) next()
  }

}

abstract class BasicCircuitSimulation extends Simulation{

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire{
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal = sigVal

    def setSignal(s:Boolean)={
      if(s!=sigVal){
        sigVal=s
        actions foreach(_ ())
      }
    }

    def addAction(a:Action)={
      actions = a::actions
      a()
    }

    def inverter(input:Wire, output:Wire) ={
      def invertAction() ={
        val inputSig = input.getSignal
        afterDelay(InverterDelay){
          output setSignal !inputSig
        }
      }
      input addAction invertAction
    }
  }
}




