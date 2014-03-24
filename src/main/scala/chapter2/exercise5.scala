package fpinscala

object Exercise5 {
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
