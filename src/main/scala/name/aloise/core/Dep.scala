package name.aloise.core


private[core] object hset {

  import scala.language.implicitConversions
  import shapeless.{::, =:!=, DepFn1, DepFn2, HList, HNil, NotContainsConstraint}

  final class HListSetOps[L <: HList](l : L){
    def :+:[E](e: E)(implicit nc: NotContainsConstraint[L, E]) = ::(e, l)
  }

  implicit def hlistSetOps[L <: HList](l: L): HListSetOps[L] = new HListSetOps(l)

  implicit def notContainsConstraintHNilType[A]: NotContainsConstraint[HNil.type, A] =
    new NotContainsConstraint[HNil.type, A] {}

  trait SetUnion[L <: HList, R <: HList] extends DepFn2[L, R] {
    type Out <: HList
  }

  trait LowPrioritySetUnion {
    type Aux[L1 <: HList, R1 <: HList, Out1 <: HList] = SetUnion[L1, R1]{
      type Out = Out1
    }

    // let ∅ ∪ M = M
    implicit def hsetEmptyUnionHNil[M <: HList]: Aux[HNil, M, M] = new SetUnion[HNil, M] {
      override type Out = M
      def apply(l: HNil, m: M): Out = m
    }

  }

  trait MediumPrioritySetUnion extends LowPrioritySetUnion {
    // let ∅ ∪ M = M
    implicit def hsetEmptyUnion[M <: HList]: Aux[HNil.type, M, M] = new SetUnion[HNil.type, M] {
      override type Out = M
      def apply(l: HNil.type , m: M): Out = m
    }
  }

  object SetUnion extends MediumPrioritySetUnion {
    def apply[L <: HList, R <: HList](implicit union: SetUnion[L, R]): Aux[L, R, union.Out] = union

    // (h :: T) ∪ M => T ∪ ( h :: (M - hType) ), result is U
    implicit def hsetUnion[H, T <: HList, M <: HList, U <: HList, MR <: HList]
    (implicit
     remove: SetRemove.Aux[M, H, MR],
     unionTail: SetUnion.Aux[T, H :: MR, U]
    ): Aux[H :: T, M, U] =
      new SetUnion[H :: T, M] {
        override type Out = U
        override def apply(t: H :: T, m: M): Out =
          unionTail(t.tail, ::(t.head, remove(m)))
      }
  }

  sealed trait SetExclude[L <: HList, R <: HList] extends DepFn2[L, R] {
    type Out <: HList
  }

  trait Priority0SetExclude {
    type Aux[L1 <: HList, R1 <: HList, Out1 <: HList] = SetExclude[L1, R1]{
      type Out = Out1
    }

    implicit def excludeFromEmpty[R <: HList]: Aux[HNil, R, HNil] = new SetExclude[HNil, R] {
      override type Out = HNil

      override def apply(t: HNil, u: R): HNil = t
    }

  }

  trait Priority1SetExclude extends Priority0SetExclude {
    implicit def emptyExclude[L <: HList]: Aux[L, HNil, L] = new SetExclude[L, HNil] {
      override type Out = L
      override def apply(t: L, u: HNil): L = t
    }
  }

  object SetExclude extends Priority1SetExclude {

    implicit def nonEmptyExclude[L <: HList, RH, RT <: HList, Out1 <: HList, Out2 <: HList]
    (implicit remove: SetRemove.Aux[L, RH, Out1],
     exclude2: Aux[Out1, RT, Out2]
    ): SetExclude.Aux[L, RH :: RT, Out2] =
      new SetExclude[L, RH :: RT] {
        override type Out = Out2
        override def apply(t: L, u: RH :: RT): Out2 = exclude2(remove(t), u.tail)
      }
  }

  trait SetRemove[L <: HList, U] extends DepFn1[L] {
    type Out <: HList
  }

  trait LowPrioritySetRemove {
    type Aux[L1 <: HList, U, Out1 <: HList] = SetRemove[L1, U] { type Out = Out1 }

    implicit def hEmptyRemoveHnil[U]: SetRemove.Aux[HNil , U, HNil] = new SetRemove[HNil , U] {
      override type Out = HNil

      override def apply(t: Out): HNil = HNil
    }

  }

  trait MediumPrioritySetRemove extends LowPrioritySetRemove {
    implicit def hEmptyRemove[U]: SetRemove.Aux[HNil.type , U, HNil.type] = new SetRemove[HNil.type, U] {
      override type Out = HNil.type

      override def apply(t: Out): HNil.type = HNil
    }
  }

  object SetRemove extends MediumPrioritySetRemove {
    def apply[L <: HList, U, Out <: HList](implicit remove: SetRemove.Aux[L, U, Out]) = remove

    implicit def removeFromHList[H, T <: HList]: SetRemove.Aux[H :: T, H, T] = new SetRemove[H :: T, H] {
      type Out = T
      override def apply(t: H :: T): T = t.tail
    }

    implicit def recurseRemoveFromHList[H, T <: HList, U, Out1 <: HList]
    (implicit rm: SetRemove.Aux[T, U, Out1],
     neq: H =:!= U,
     ct: NotContainsConstraint[Out1, H]
    ): SetRemove.Aux[H :: T, U, H :: Out1] =
      new SetRemove[H :: T, U] {
        type Out = H :: Out1
        override def apply(t: H :: T): Out = t.head :: rm(t.tail)
      }
  }
}

trait Dep[A] { self =>

  import hset._

  import shapeless.ops.hlist.{Align, Intersection}
  import shapeless.{::, Generic, HList, HNil, NotContainsConstraint}

  type HSet = HList

  type Env <: HList // environment minus internally provided types
  type Inj <: HSet // all injected and captured params - ie all output "A" types

  protected def runWithEnv(environment: Env): (A, Inj)

  def run(implicit envNil: HNil =:= Env): A =
    runWithEnv(envNil(HNil))._1

  def run[ENVG <: Product](environment: ENVG)(implicit generic: Generic.Aux[ENVG, Env]): A =
    self.runWithEnv(generic.to(environment))._1

  def run[S](environment: S)(implicit ev: (S :: HNil) =:= Env): A =
    self.runWithEnv(ev(environment :: HNil))._1

  def run(env: Env): A = runWithEnv(env)._1

  def map[B](f: A => B)(implicit nc: NotContainsConstraint[Inj, B]): Dep.Aux[Env, B :: Inj, B] = new Dep[B] {
    override type Env = self.Env
    override type Inj = B :: self.Inj

    override protected def runWithEnv(environment: Env): (B, Inj) = {

      val (parentRun, inj) = self.runWithEnv(environment)

      val mapped = f(parentRun)
      (mapped, mapped :+: inj)
    }
  }

  def flatMap[
  Env2 <: HList,
  Inj2 <: HSet,
  CombinedInj <: HSet,
  CombinedEnv <: HSet,
  CombinedEnvMinusInjected <: HSet,
  EnvWithInjected <: HSet,
  EnvReconstructed <: HSet,
  CombinedEnvWithInject <: HSet,
  Env2Reconstructed <: HSet,
  B]
  (f: A => Dep.Aux[Env2, Inj2, B])
  (implicit injectedCombined: SetUnion.Aux[Inj, Inj2, CombinedInj],
   combinedEnv: SetUnion.Aux[self.Env, Env2, CombinedEnv],
   combinedEnvMinusInjected: SetExclude.Aux[CombinedEnv, CombinedInj, CombinedEnvMinusInjected],
   envReconstructed: Intersection.Aux[CombinedEnvMinusInjected, self.Env, EnvReconstructed],
   envAligned: Align[EnvReconstructed, self.Env],
   combinedEnvWithInject: SetUnion.Aux[CombinedEnvMinusInjected, self.Inj, CombinedEnvWithInject],
   env2Reconstructed: Intersection.Aux[CombinedEnvWithInject, Env2, Env2Reconstructed],
   env2Aligned: Align[Env2Reconstructed, Env2]
  ): Dep.Aux[CombinedEnvMinusInjected, CombinedInj, B] = new Dep[B] {
    override type Env = CombinedEnvMinusInjected
    override type Inj = CombinedInj

    override protected def runWithEnv(environment: Env): (B, Inj) = {
      val parenEnv: self.Env = envAligned(envReconstructed(environment))
      val (parentValue, parentInjected) = self.runWithEnv(parenEnv)

      val nested = f(parentValue)
      val env2: Env2 = env2Aligned(env2Reconstructed(combinedEnvWithInject(environment, parentInjected)))
      val (nestedResult, nestedInj) = nested.runWithEnv(env2)

      (nestedResult, injectedCombined(parentInjected, nestedInj))
    }
  }
}

object Dep {
  import shapeless.{HNil, ::}

  type Aux[E, I, A1] = Dep[A1]{
    type Env = E
    type Inj = I
  }

  def pure[B](f: => B): Aux[HNil, B :: HNil, B] = new Dep[B] {
    override type Env = HNil
    override type Inj = B :: HNil

    override protected def runWithEnv(environment: Env): (B, Inj) = {
      val result = f
      (result, result :: HNil)
    }
  }

  def reader[A,B](f: A => B): Aux[A :: HNil, B :: HNil, B] = new Dep[B] {
    override type Env = A :: HNil
    override type Inj = B :: HNil

    override protected def runWithEnv(env: Env): (B, Inj) = {
      val result = f(env.head)
      (result, result :: HNil)
    }
  }

  def reader[A1, A2, B](f: (A1, A2) => B): Aux[A1 :: A2 :: HNil, B :: HNil, B] = new Dep[B] {
    override type Env = A1 :: A2 :: HNil
    override type Inj = B :: HNil

    override protected def runWithEnv(env: Env): (B, Inj) = {
      val result = f(env.head, env.tail.head)
      (result, result :: HNil)
    }
  }

  def reader[A1, A2, A3, B](f: (A1, A2, A3) => B): Aux[A1 :: A2 :: A3 :: HNil, B :: HNil, B] = new Dep[B] {
    override type Env = A1 :: A2 :: A3 :: HNil
    override type Inj = B :: HNil

    override protected def runWithEnv(env: Env): (B, Inj) = {
      val result = f(env.head, env.tail.head, env.tail.tail.head)
      (result, result :: HNil)
    }
  }

  def reader[A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => B): Aux[A1 :: A2 :: A3 :: A4 :: HNil, B :: HNil, B] = new Dep[B] {
    override type Env = A1 :: A2 :: A3 :: A4 :: HNil
    override type Inj = B :: HNil

    override protected def runWithEnv(env: Env): (B, Inj) = {
      val result = f(env.head, env.tail.head, env.tail.tail.head, env.tail.tail.tail.head)
      (result, result :: HNil)
    }
  }
}

object Test2 {

  import Dep._

  // inputs
  case class A1(i: Int)
  case class A2(i: Int)
  case class A3(i: Int)
  case class A4(i: Int)
  case class A5(i: Int)

  // outputs
  case class B1(i: Int)
  case class B2(i: Int)
  case class B3(i: Int)
  case class B4(i: Int)
  case class B5(i: Int)
  case class B6(s: String)
  case class B8(str: String)

  case class Final(i: Int)
  case class AnotherFinal(i: Int)

  case class Dep2B1(i: Int)


  val dep = for {
    b1 <- pure(B1(5))
    b2 <- pure(B2(15))
    _ <- reader{(str: String, str2: B1) => str.length }
    _ <- reader{(sameStr: String, b2: B2, b3: B3) => B4(1) }
    _ <- reader{(str: String, str2: B1) => str.length }
    _ <- reader{(sameStr3: String, b2: B2, b3: B3) => B4(4) }
    _ <- reader{(str: String, str2: B1) => str.length }
    _ <- reader{(sameStr4: String, b2: B2, b3: B3) => B5(5) }
    _ <- reader{b: B5 => B6("test")}
    b8 <- reader{ _:B2 => B8("aloise2") }
  } yield Final(b1.i*2*b8.str.length)

  val dep2 = for {
    len <- reader{(str: String, str2: B1) => Dep2B1(str.length) }
    len2 = len.i*2
    _ <- reader{b1: B1 => B2(b1.i)}
    _ <- reader{(sameStr4: String, b2: B2, b3: B3) => B5(5) }
    fn <- reader{ f: Final => f.i * 2L}
  } yield AnotherFinal(len2+fn.toInt)

  val bigDep = for {
    d0 <- dep
    d1 <- dep2
  } yield d0.i*d1.i

  def main(args: Array[String]): Unit = {

    case class Env(b3: B3, str: String)

    println(bigDep.run(B3(1), "Input222333"))

    println(bigDep.run(Env(B3(2), "xxx")))

  }


}