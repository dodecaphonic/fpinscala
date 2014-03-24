package fpinscala

object Exercise3 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)
}
