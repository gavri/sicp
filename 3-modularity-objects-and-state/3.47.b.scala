import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec

class Semaphore(var n: Int) {
  val flag = new AtomicBoolean(false)

  @tailrec
  final def acquire: Unit = {
    if (flag.compareAndSet(false, true)) {
      n += 1
      flag.set(false)
    }
    else {
      acquire
    }
  }

  @tailrec
  final def release: Unit = {
    if (flag.compareAndSet(false, true)) {
      n += 1
      flag.set(false)
    }
    else {
      release
    }
  }
}
