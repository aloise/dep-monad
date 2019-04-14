package name.aloise.core

import cats.{FlatMap, Functor, Id}
import name.aloise.core.utils.hlist.Detach
import shapeless.ops.hlist
import shapeless.{::, =:!=, Generic, HList, HNil, IsDistinctConstraint, NotContainsConstraint}
import shapeless.ops.hlist._

import scala.language.higherKinds

trait Dep2[A] { self =>

  type Env <: HList // environment minus internally provided types
  type Inj <: HList // all injected and captured params - ie all output "A" types

  protected def runWithEnv(environment: Env): A

  def run(implicit envNil: HNil =:= Env): A =
    runWithEnv(envNil(HNil))

  def run(env: Env): A =
    runWithEnv(env)

  def map[B](f: A => B)(implicit injUnique: NotContainsConstraint[self.Inj, A]): Dep2.Aux[Env, A :: Inj, B] = new Dep2[B] {
    override type Env = self.Env
    override type Inj = A :: self.Inj

    override protected def runWithEnv(environment: Env): B = {

      val parentRun = self.runWithEnv(environment)

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
                AllEnv <: HList,
                B]
            (f: A => Dep2.Aux[Env2, Inj2, B])
            (implicit union: Union.Aux[Env, Env2, CombinedEnv],
             injUnique: NotContainsConstraint[self.Inj, B],
             detach: Detach[Env2, Env, CombinedEnv],
             union2: Union.Aux[CombinedEnvMinusInjected, A :: Inj, AllEnv],
             diff: Diff.Aux[CombinedEnv, B :: self.Inj, CombinedEnvMinusInjected]
            ): Dep2.Aux[CombinedEnvMinusInjected, A :: Inj, B] = new Dep2[B] {
    override type Env = CombinedEnvMinusInjected
    override type Inj = A :: self.Inj

    override protected def runWithEnv(environment: CombinedEnvMinusInjected): B = {
      val globalEnv = union2(environment, ???)
      val (env2, env) = detach(combinedWithInjected)
      val parentResult = self.runWithEnv(env)

      f(parentResult).runWithEnv(env2)
    }
  }
}

object Dep2 {
  type Aux[E, I, A1] = Dep2[A1]{
    type Env = E
    type Inj = I
  }

  def pure[A](f: => A): Aux[HNil, HNil, A] = new Dep2[A] {
    override type Env = HNil
    override type Inj = HNil

    override protected def runWithEnv(environment: Env): A = f
  }

  def reader[A,B](f: A => B): Aux[A :: HNil, A :: HNil, B] = new Dep2[B] {
    override type Env = A :: HNil
    override type Inj = A :: HNil

    override protected def runWithEnv(environment: Env): B = f(environment.head)
  }
}

object Test {

  import Dep2._

  val dep = for {
    a <- pure(5)
    b <- pure("str")
  } yield a + b.length

  def main(args: Array[String]): Unit = {
    println(dep.run)
  }


}
