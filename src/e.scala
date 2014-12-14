
/**
 * This is where the fun begins.
 */
object e extends App {
  
  val system = new System(2)
  
  def fix[A, B](f: (A=>B) => (A=>B)): A=>B = f(fix(f))(_) // <3 that shiat.
  val loop = fix[Unit, Unit](f => a => { system.tick; Thread.sleep(2000); f()})
  loop()

}