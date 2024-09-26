import scala.util.boundary, boundary.break
/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  *
  * You should finish this procedure according to
  * the reference described in `README.md` to complete
  * the assignment.
  */
@main def calculator(commands: String*): Unit = {
  /** Converts given string `s` to integer.
    *
    * Throws [[NumberFormatException]] if `s` can't be converted to integer,
    * but you shouldn't worry about it at this moment.
    */
  def parseInt(s: String): Int = s.toInt

  /** Representation of `acc` register. */
  var acc: Int = 0
  var A: Int = 0
  var B: Int = 0
  var blink: Boolean = false

  boundary:
    for (c <- commands) {
      c match {
        case "+" => acc = A + B; blink = false
        case "-" => acc = A - B; blink = false
        case "*" => acc = A * B; blink = false
        case "/" =>
          if (B == 0)
            A = 0
            acc = 0
          else acc = A / B
          blink = false
        case "swap" => var sth: Int = A; A = B; B = sth
        case "blink" => blink = !blink
        case "acc" =>
          if (blink == false) A = acc
          else B = acc
          blink = !blink
        case "break" => break()
        case _ => var value: Int = parseInt(c); if (blink == false) A = value; else B = value; blink = !blink
      }
  }
  println(acc)
}
