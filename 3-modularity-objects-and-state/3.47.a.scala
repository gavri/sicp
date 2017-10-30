import scala.annotation.tailrec

class Semaphore(var n: Int) {
  val mutex = new Object

  @tailrec
  final def acquire: Unit = {
    var acquired = false
    mutex.synchronized {
      if (n > 0) {
        n -= 1
        acquired = true
      }
    }
    if (!acquired) {
      acquire
    }
  }

  def release: Unit = {
    mutex.synchronized { n += 1 }
  }
}
