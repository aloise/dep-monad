package name.aloise.core

import cats.{FlatMap, Functor, Id}
import name.aloise.core.utils.hlist.Detach
import shapeless.ops.hlist
import shapeless.{::, =:!=, Generic, HList, HNil, IsDistinctConstraint, NotContainsConstraint}
import shapeless.ops.hlist._

import scala.language.higherKinds

trait Dep2[A] { self =>

  type Env <: HList
  type Inj <: HList // all injected and captured params - ie all "A" types
  type EnvWoInj <: HList

  protected def runWithInjected(environment: EnvWoInj, injected: Inj): A

  def run(implicit envNil: HNil =:= EnvWoInj, injNil: HNil =:= Inj): A =
    runWithInjected(envNil(HNil), injNil(HNil))

  def run(env: EnvWoInj)(implicit eq: HNil =:= Inj): A =
    runWithInjected(env, eq(HNil))

  def map[B, EnvWoInjOut <: HList](f: A => B)(implicit filter: FilterNot.Aux[Env, B, EnvWoInjOut]): Dep2.Aux[Env, B :: Inj, EnvWoInjOut, B] = new Dep2[B] {
    override type Env = self.Env
    override type Inj = B :: self.Inj
    override type EnvWoInj = EnvWoInjOut

    override protected def runWithInjected(environment: EnvWoInj, injected: Inj): B = {
      val parentRun = self.runWithInjected(environment, injected.tail)
      val mapped = f(parentRun)
      mapped
    }
  }

  def flatMap[
                Env2 <: HList,
                Inj2 <: HList,
                EwoI <: HList,
                CombinedEnv <: HList,
                CombinedEnvMinusInjected <: HList,
                B]
            (f: A => Dep2.Aux[Env2, Inj2, EwoI, B])
            (implicit union: Union.Aux[Env, Env2, CombinedEnv],
             injUnique: NotContainsConstraint[self.Inj, B],
             diff: Diff.Aux[CombinedEnv, B :: self.Inj, CombinedEnvMinusInjected]
            ): Dep2.Aux[CombinedEnv, B :: Inj, CombinedEnvMinusInjected, B] = new Dep2[B] {
    override type Env = CombinedEnv
    override type Inj = B :: self.Inj
    override type EnvWoInj = CombinedEnvMinusInjected

    override protected def runWithInjected(environment: EnvWoInj, injected: Inj): B = ???
  }
}

object Dep2 {
  type Aux[E, I, EwoI, A1] = Dep2[A1]{
    type Env = E
    type Inj = I
    type EnvWoInj = EwoI
  }

  def pure[A](f: => A): Aux[HNil, A :: HNil, HNil, A] = new Dep2[A] {
    override type Env = HNil
    override type Inj = A :: HNil
    override type EnvWoInj = HNil

    override protected def runWithInjected(environment: EnvWoInj, injected: Inj): A = f
  }

  def reader[A,B](f: A => B): Aux[A :: HNil, B :: HNil, A :: HNil, B] = new Dep2[B] {
    override type Env = A :: HNil
    override type Inj = B :: HNil
    override type EnvWoInj = A :: HNil

    override protected def runWithInjected(environment: EnvWoInj, injected: Inj): B = f(environment.head)
  }
}

object Test {

  import Dep2._

  val ad: Aux[HNil, Int :: HNil, HNil, Int] = pure(1)
  val bd: Aux[HNil, String :: HNil, HNil, String] = pure("str")

  val x = ad.flatMap(a => bd).map(x => x.length)

//  val dep = for {
//    a <- ad
//    b <- bd
//  } yield a + b.length
//
//  def main(args: Array[String]): Unit = {
//    println(dep.run)
//  }


}
