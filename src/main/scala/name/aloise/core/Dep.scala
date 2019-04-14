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

  protected def runWithInjected(environment: Env, injected: Inj): A

  def run(env: EnvWoInj)(implicit eq: HNil =:= Inj, eq2: EnvWoInj =:= Env): A =
    runWithInjected(eq2(env), eq(HNil))

  def map[B, Out <: HList](f: A => B)(implicit en: FilterNot.Aux[Env, B, Out]): Dep2.Aux[Env, B :: Inj, B] = new Dep2[B] {
    override type Env = self.Env
    override type Inj = B :: self.Inj
    override type EnvWoInj = Out

    override protected def runWithInjected(environment: Env, injected: Inj): B = {
      val parentRun = self.runWithInjected(environment, injected.tail)
      val mapped = f(parentRun)
      mapped
    }
  }

  def flatMap[
                Env2 <: HList,
                Inj2 <: HList,
                CombinedEnv <: HList,
                CombinedEnvMinusInjected <: HList,
                B]
            (f: A => Dep2.Aux[Env2, Inj2, B])
            (implicit union: Union.Aux[Env, Env2, CombinedEnv],
             injUnique: NotContainsConstraint[self.Inj, B],

            ): Dep2.Aux[Env2, B :: Inj, CombinedEnvMinusInjected] = new Dep2[B] {
    override type Env = CombinedEnv
    override type Inj = B :: self.Inj
    override type EnvWoInj = this.type

    override protected def runWithInjected(environment: Env, injected: Inj): B = ???
  }
}

object Dep2 {
  type Aux[E, I, A1] = Dep2[A1]{
    type Env = E
    type Inj = I
  }
}

object Test {

}
