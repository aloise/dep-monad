package name.aloise.core

import name.aloise.core.utils.hset.{:+:, Exclude, HEmpty, HSet, Union}
import shapeless.{::, Generic, HList, HNil}
import shapeless.{=:!=, DepFn1, DepFn2, Generic, HList, HNil}

trait Dep3[A] { self =>

  type Env <: HList // environment minus internally provided types
  type Inj <: HSet // all injected and captured params - ie all output "A" types

  protected def runWithEnv(environment: Env): A

  def run(implicit envNil: HNil =:= Env): A =
    runWithEnv(envNil(HNil))

  def run(env: Env): A =
    runWithEnv(env)

  def map[B](f: A => B): Dep3.Aux[Env, A :+: Inj, B] = new Dep3[B] {
    override type Env = self.Env
    override type Inj = A :+: self.Inj

    override protected def runWithEnv(environment: Env): B = {

      val parentRun = self.runWithEnv(environment)

      val mapped = f(parentRun)
      mapped
    }
  }

  def flatMap[
  Env2 <: HList,
  Inj2 <: HSet,
  CombinedInjWithoutA <: HSet,
  CombinedInj <: HSet,
  EwoI <: HSet,
  CombinedEnv <: HSet,
  CombinedEnvMinusInjected <: HSet,
  FinalEnvHList <: HNil,
  B]
  (f: A => Dep3.Aux[Env2, Inj2, B])
  (
    implicit injectedCombinedWithoutA: Union.Aux[A :+: Inj, Inj2, CombinedInjWithoutA],
    injectedCombinedWithA: Union.Aux[A :+: HEmpty, CombinedInjWithoutA, CombinedInj],
    // combinedEnv: Union.Aux[Env, Env2, CombinedEnv],
    combinedEnvMinusInjected: Exclude.Aux[CombinedEnv, CombinedInj, CombinedEnvMinusInjected],
    toFinalEnv: Generic.Aux[CombinedEnvMinusInjected, FinalEnvHList],
  ): Dep3.Aux[FinalEnvHList, CombinedInj, B] = new Dep3[B] {
    override type Env = FinalEnvHList
    override type Inj = CombinedInj

    override protected def runWithEnv(environment: FinalEnvHList): B = {
      ???
    }
  }
}

object Dep3 {
  type Aux[E, I, A1] = Dep3[A1]{
    type Env = E
    type Inj = I
  }

  def pure[A](f: => A): Aux[HNil, HEmpty, A] = new Dep3[A] {
    override type Env = HNil
    override type Inj = HEmpty

    override protected def runWithEnv(environment: Env): A = f
  }

  def reader[A,B](f: A => B): Aux[A :: HNil, B :+: HEmpty, B] = new Dep3[B] {
    override type Env = A :: HNil
    override type Inj = B :+: HEmpty

    override protected def runWithEnv(environment: Env): B = f(environment.head)
  }
}

object Test2 {

  import Dep3._

//  val dep = for {
//    a <- pure(5)
//    b <- pure("str")
//    c <- reader{ i: String => true }
//    _ <- reader{ i: String => A(5) }
//    _ <- reader{ i: String => B(5) }
//  } yield a + b.length + (if(c)1 else 0)

  case class A(i: Int)
  case class B(i: Int)
  case class C(i: Int)
  case class D(i: Int)

  def main(args: Array[String]): Unit = {
    // println(dep.run)

    val a = A(12) :+: true :+: "hello world" :+: 1 :+: HEmpty

    val b = C(15) :+: A(15) :+: "str" :+: HEmpty

    // println((a join b join c).replace[String]("replaced2"))

    // println(("str" :+: HEmpty) union (HEmpty))
    // println(Union.hsetEmptyUnion.apply(HEmpty, 1 :+: "str" :+: HEmpty))

    // println(a union b)
    // println(a.remove[A].remove[Boolean].remove[B])

    // println(dep.run)

  }


}