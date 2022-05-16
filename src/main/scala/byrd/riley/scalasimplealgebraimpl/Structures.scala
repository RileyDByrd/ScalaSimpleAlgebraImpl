package byrd.riley.scalasimplealgebraimpl

object Structures {
  trait Semigroup[A] {
    def combine(x: A, y: A): A

    def associativeLaw(x: A, y: A, z: A): Unit =
      assert(combine(x, combine(y, z)) == combine(combine(x, y), z))
  }

  // Parent of commutative structures
  trait CommutativeSemigroup[A] extends Semigroup[A] {
    def commutativeLaw(x: A, y: A): Unit =
      assert(combine(x, y) == combine(y, x))
  }

  // Parent of identity structures
  trait Monoid[A] extends Semigroup[A] {
    def empty: A

    def identityLaw(x: A): Unit =
      assert(combine(x, empty) == x && combine(empty, x) == x)
  }

  // Parent of idempotent structures
  trait Band[A] extends Semigroup[A] {
    def idempotentLaw(x: A): Unit =
      assert(combine(x, x) == x)
  }

  // Parent of inverse structures
  trait Group[A] extends Monoid[A] {
    def inverse(x: A): A

    def inverseLaw(x: A): Unit = {
      val inverseX = inverse(x)
      val inverseCombination = combine(x, inverseX)
      assert(inverseCombination == combine(inverseX, x))
      assert(inverseCombination == empty)
    }
  }

  // The alternative types (not traits) below are not used but might be helpful to understanding the traits.
  trait Semilattice[A] extends CommutativeSemigroup[A] with Band[A]
  type CommutativeBand[A] = Semilattice[A]
  trait CommutativeMonoid[A] extends CommutativeSemigroup[A] with Monoid[A]
  trait CommutativeGroup[A] extends CommutativeSemigroup[A] with Group[A]

  trait BoundedSemilattice[A] extends Semilattice[A] with Monoid[A]
  type CommutativeIdempotentMonoid[A] = BoundedSemilattice[A]
  type IdentitySemilattice[A] = BoundedSemilattice[A]
}
