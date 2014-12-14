import scala.collection.mutable.MutableList
import scala.util.Random

/**
 * Elevator implementation
 */
case class Elevator(floor: Int, capacity: Int, name: String, dir: Int => Int) {
  println(s"Hello from $name! Current floor is $floor. I can carry $capacity people.")
}

/**
 * System containing elevators
 */
class System(n: Int) {
  
  /*
   * Private Values
   */
  private val up: Int => Int = i => i+1
  private val down: Int => Int = i => i-1
  private val move: (Elevator => Elevator) = e => {
    e.floor match {
      case x if x == 9 && e.dir == up => Elevator(e.dir(e.floor), e.capacity, e.name, down)
      case x if x == 2 && e.dir == down => Elevator(e.dir(e.floor), e.capacity, e.name, up)
      case _ => Elevator(e.dir(e.floor), e.capacity, e.name, e.dir)
    }
  }
  
  /*
   * Construction
   */
  private val elevators = new MutableList[Elevator]
  for(i <- 1 to n) elevators += Elevator(Random.nextInt(7) + 2, 5, s"Elevator $i", up)
  
  /*
   * Public
   */
  def tick = elevators foreach { x => elevators.update(elevators.indexOf(x), move(x)) }
}