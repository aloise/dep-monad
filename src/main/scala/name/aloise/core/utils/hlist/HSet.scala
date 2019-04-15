package name.aloise.core.utils

import shapeless.{=:!=, DepFn1, DepFn2, Generic, HList, HNil}

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

object hset {

  sealed trait HSet

  final case class :+:[H, +T <: HSet](head : H, tail : T) extends HSet {
    override def toString: String = {
      def gatherTypes(t: HSet): Set[String] = t match {
        case elem: :+:[_, _] => gatherTypes(elem.tail) + elem.head.toString
        case HEmpty => Set()
      }

      val allTypes = gatherTypes(this)

      "(" + allTypes.mkString(", ") + ")"
    }
  }

  sealed trait HEmpty extends HSet {
    def :+:[H](h : H) = hset.:+:(h, this)

    override def toString: String = "(<empty set>)"
  }

  case object HEmpty extends HEmpty

  final class HSetOps[L <: HSet](l : L) {
    def :+:[E](e: E)(implicit nc: NotContains[L, E]) = new :+:(e, l)

    def select[U](implicit s: Selector[L, U]): U = s(l)

    def join[R <: HSet, Out <: HSet](r: R)(implicit join: JoinWithoutDuplicates.Aux[L, R, Out]): Out = join(l, r)

    def replace[U](elem: U)(implicit replace: Replace[L, U]): L = replace(l, elem)

    def remove[U](implicit rm: Remove[L, U]): rm.Out = rm(l)

    def union[R <: HSet, Out <: HSet](r: R)(implicit uni: Union.Aux[L, R, Out]): Out =
      uni(l, r)

  }

  object HSet {
    import shapeless.::

    implicit def hlistOps[L <: HSet](l: L): HSetOps[L] = new HSetOps(l)

    implicit def genericHEmpty: Generic.Aux[HEmpty, HNil] = new Generic[HEmpty] {
      override type Repr = HNil
      override def to(t: HEmpty): HNil = HNil
      override def from(r: HNil): HEmpty = HEmpty
    }

    implicit def generic[H, T <: HSet, Out1 <: HList](implicit gen2: Generic.Aux[T, Out1], notContains: NotContains[T, H]): Generic.Aux[H :+: T, H :: Out1] = new Generic[H :+: T] {
      override type Repr = H :: Out1
      override def to(t: H :+: T): H :: Out1 = t.head :: gen2.to(t.tail)
      override def from(r: H :: Out1): H :+: T = r.head :+: gen2.from(r.tail)
    }

  }


  trait Add[A, L <: HSet] extends DepFn2[A, L] {
    type Out <: HSet
  }


  object Add {

    type Aux[A1, L1 <: HSet, Out1 <: HSet] = Add[A1, L1]{ type Out = Out1 }

    def apply[A, L <: HSet](implicit add: Add[A, L]): Add[A, L] = add

    implicit def addToHSet[A, L <: HSet](implicit notContains: NotContains[L, A]): Add.Aux[A, L, A :+: L] = new Add[A, L] {
      override type Out = A :+: L
      override def apply(t: A, u: L): A :+: L = :+:(t, u)
    }

  }

  @implicitNotFound("This HSet already contains element of type ${U}.")
  trait NotContains[L <: HSet, U]

  object NotContains {

    def apply[L <: HSet, U](implicit ncc: NotContains[L, U]): NotContains[L, U] = ncc

    implicit def hnilNotContains[U]: NotContains[HEmpty, U] = new NotContains[HEmpty, U] {}
    implicit def hlistNotContains[H, T <: HSet, U](implicit nc: T NotContains U, neq: U =:!= H): NotContains[H :+: T, U] =
      new NotContains[H :+: T, U] {}
  }

  trait Replace[L <: HSet, U] extends DepFn2[L, U] { type Out = L }

  trait LowPriorityReplace {
    implicit def replace[H, T <: HSet, U](implicit eqH: U =:= H, nc: NotContains[T, H]): Replace[H :+: T, U] = new Replace[H :+: T, U] {
      override def apply(t: H :+: T, replaceWith: U): H :+: T = eqH(replaceWith) :+: t.tail
    }
  }

  object Replace extends LowPriorityReplace {
    def apply[L <: HSet, U](implicit replace: Replace[L, U]): Replace[L, U] = replace

    implicit def recurse[H, T <: HSet, U](implicit rpl: Replace[T, U], neq: U =:!= T, nc: NotContains[T, H]): Replace[H :+: T, U] = new Replace[H :+: T, U] {
      override def apply(t: H :+: T, replaceWith: U): H :+: T = t.head :+: rpl(t.tail, replaceWith)
    }
  }

  trait Selector[L <: HSet, U] extends DepFn1[L] { type Out = U }

  object Selector {
    def apply[L <: HSet, U](implicit selector: Selector[L, U]): Selector[L, U] = selector

    implicit def select[H, T <: HSet]: Selector[H :+: T, H] = new Selector[H :+: T, H] {
      override def apply(t: H :+: T): H = t.head
    }

    implicit def recurse[H, T <: HSet, U](implicit st: Selector[T, U]): Selector[H :+: T, U] =
      (t: H :+: T) => st(t.tail)
  }

  trait JoinWithoutDuplicates[L <: HSet, R <: HSet] extends DepFn2[L, R] {
    type Out <: HSet
  }

  trait LowPriorityJoinWitohutDuplicates {
    type Aux[L1 <: HSet, R1 <: HSet, Out1 <: HSet] = JoinWithoutDuplicates[L1, R1]{
      type Out = Out1
    }

    // let ∅ ∪ M = M
    implicit def hsetEmptyJoin[M <: HSet]: Aux[HEmpty, M, M] = new JoinWithoutDuplicates[HEmpty, M] {
      override type Out = M
      def apply(l: HEmpty, m: M): Out = m
    }
  }

  object JoinWithoutDuplicates extends LowPriorityJoinWitohutDuplicates {
    def apply[L <: HSet, M <: HSet](implicit union: JoinWithoutDuplicates[L, M]): Aux[L, M, union.Out] = union

    // (h :+: T) ∪ M => T ∪ ( h :+: M ), result is U
    implicit def hsetJoin[H, T <: HSet, M <: HSet, U <: HSet]
    (implicit unionTail: JoinWithoutDuplicates.Aux[T, H :+: M, U], notContains: NotContains[M, H]): Aux[H :+: T, M, U] =
      new JoinWithoutDuplicates[H :+: T, M] {
        override type Out = U
        override def apply(t: H :+: T, m: M): Out =
          unionTail(t.tail, t.head :+: m)
      }

  }

  trait Union[L <: HSet, R <: HSet] extends DepFn2[L, R] {
    type Out <: HSet
  }

  trait LowPriorityUnion {
    type Aux[L1 <: HSet, R1 <: HSet, Out1 <: HSet] = Union[L1, R1]{
      type Out = Out1
    }
    // let ∅ ∪ M = M
    implicit def hsetEmptyUnion[M <: HSet]: Aux[HEmpty, M, M] = new Union[HEmpty, M] {
      override type Out = M
      def apply(l: HEmpty, m: M): Out = m
    }

  }

  object Union extends LowPriorityUnion {
    def apply[L <: HSet, R <: HSet](implicit union: Union[L, R]): Aux[L, R, union.Out] = union
    
    // (h :+: T) ∪ M => T ∪ ( h :+: (M - hType) ), result is U
    implicit def hsetUnion[H, T <: HSet, M <: HSet, U <: HSet, MR <: HSet]
      (implicit
       remove: Remove.Aux[M, H, MR],
       unionTail: Union.Aux[T, H :+: MR, U]
      ): Aux[H :+: T, M, U] =
        new Union[H :+: T, M] {
          override type Out = U
          override def apply(t: H :+: T, m: M): Out =
            unionTail(t.tail, :+:(t.head, remove(m)))
        }
  }

  sealed trait Exclude[L <: HSet, R <: HSet] extends DepFn2[L, R] {
    type Out <: HSet
  }

  trait Priority0Exclude {
    type Aux[L1 <: HSet, R1 <: HSet, Out1 <: HSet] = Exclude[L1, R1]{
      type Out = Out1
    }

    implicit def excludeFromEmpty[R <: HSet]: Aux[HEmpty, R, HEmpty] = new Exclude[HEmpty, R] {
      override type Out = HEmpty

      override def apply(t: HEmpty, u: R): HEmpty = t
    }

  }

  trait Priority1Exclude extends Priority0Exclude {
    implicit def emptyExclude[L <: HSet]: Aux[L, HEmpty, L] = new Exclude[L, HEmpty] {
      override type Out = L
      override def apply(t: L, u: HEmpty): L = t
    }
  }

  object Exclude extends Priority1Exclude {

    implicit def nonEmptyExclude[L <: HSet, RH, RT <: HSet, Out1 <: HSet, Out2 <: HSet]
    (implicit remove: Remove.Aux[L, RH, Out1],
     exclude2: Aux[Out1, RT, Out2]
    ): Exclude.Aux[L, RH :+: RT, Out2] =
      new Exclude[L, RH :+: RT] {
        override type Out = Out2
        override def apply(t: L, u: RH :+: RT): Out2 = exclude2(remove(t), u.tail)
      }
  }

  trait Remove[L <: HSet, U] extends DepFn1[L] {
    type Out <: HSet
  }

  trait LowPriorityRemove {
    type Aux[L1 <: HSet, U, Out1 <: HSet] = Remove[L1, U] { type Out = Out1 }

    implicit def hEmptyRemove[U]: Remove.Aux[HEmpty, U, HEmpty] = new Remove[HEmpty, U] {
      override type Out = HEmpty

      override def apply(t: Out): HEmpty = HEmpty
    }
  }

  object Remove extends LowPriorityRemove {
    def apply[L <: HSet, U, Out <: HSet](implicit remove: Remove.Aux[L, U, Out]) = remove

    implicit def removeFromHSet[H, T <: HSet]: Remove.Aux[H :+: T, H, T] = new Remove[H :+: T, H] {
      type Out = T
      override def apply(t: H :+: T): T = t.tail
    }

    implicit def recurseRemoveFromHSet[H, T <: HSet, U, Out1 <: HSet]
    (implicit rm: Remove.Aux[T, U, Out1],
     neq: H =:!= U,
     ct: NotContains[Out1, H]
    ): Remove.Aux[H :+: T, U, H :+: Out1] =
      new Remove[H :+: T, U] {
        type Out = H :+: Out1
        override def apply(t: H :+: T): Out = t.head :+: rm(t.tail)
      }
  }
}