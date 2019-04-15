package name.aloise.core

import name.aloise.core.utils.hset.{:+:, Exclude, HEmpty, HSet, NotContains, ToHList, Union}
import shapeless.{::, Generic, HList, HNil}
import shapeless.{=:!=, DepFn1, DepFn2, Generic, HList, HNil}

trait Dep3[A] { self =>

  import HSet._

  type Env <: HList // environment minus internally provided types
  type Inj <: HSet // all injected and captured params - ie all output "A" types

  protected def runWithEnv(environment: Env): (A, Inj)

  def run(implicit envNil: HNil =:= Env): A =
    runWithEnv(envNil(HNil))._1

  def run(env: Env): A =
    runWithEnv(env)._1

  def map[B](f: A => B)(implicit nc: NotContains[Inj, B]): Dep3.Aux[Env, B :+: Inj, B] = new Dep3[B] {
    override type Env = self.Env
    override type Inj = B :+: self.Inj

    override protected def runWithEnv(environment: Env): (B, Inj) = {

      val (parentRun, inj) = self.runWithEnv(environment)

      val mapped = f(parentRun)
      (mapped, mapped :+: inj)
    }
  }

  def flatMap[
  EnvHset <: HSet,
  Env2 <: HList,
  Env2Hset <: HSet,
  Inj2 <: HSet,
  CombinedInj <: HSet,
  CombinedEnv <: HSet,
  CombinedEnvMinusInjected <: HSet,
  FinalEnvHList <: HNil,
  B]
  (f: A => Dep3.Aux[Env2, Inj2, B])
  (
    implicit injectedCombined: Union.Aux[Inj, Inj2, CombinedInj],
    envToHSet: Generic.Aux[Env, EnvHset],
    env2ToHSet: Generic.Aux[Env2, Env2Hset],
    combinedEnv: Union.Aux[EnvHset, Env2Hset, CombinedEnv],
    combinedEnvMinusInjected: Exclude.Aux[CombinedEnv, CombinedInj, CombinedEnvMinusInjected],
    toFinalEnv: Generic.Aux[CombinedEnvMinusInjected, FinalEnvHList],
  ): Dep3.Aux[FinalEnvHList, CombinedInj, B] = new Dep3[B] {
    override type Env = FinalEnvHList
    override type Inj = CombinedInj

    override protected def runWithEnv(environment: FinalEnvHList): (B, Inj) = {
      ???
    }
  }
}

object Dep3 {
  type Aux[E, I, A1] = Dep3[A1]{
    type Env = E
    type Inj = I
  }

  def pure[B](f: => B): Aux[HNil, B :+: HEmpty, B] = new Dep3[B] {
    override type Env = HNil
    override type Inj = B :+: HEmpty

    override protected def runWithEnv(environment: Env): (B, Inj) = {
      val result = f
      (result, result :+: HEmpty)
    }
  }

  def reader[A,B](f: A => B): Aux[A :: HNil, B :+: HEmpty, B] = new Dep3[B] {
    override type Env = A :: HNil
    override type Inj = B :+: HEmpty

    override protected def runWithEnv(environment: Env): (B, Inj) = {
      val result = f(environment.head)
      (result, result :+: HEmpty)
    }
  }
}

object Test2 {

  import Dep3._

//  val dep = for {
//    a <- pure(5)
////    b <- pure("str")
////    _ <- reader{ str: String => str.isEmpty }
////    _ <- reader{ i: String => A(5) }
////    _ <- reader{ i: String => B(5) }
//  } yield 1

  case class A(i: Int)
  case class B(i: Int)
  case class C(i: Int)
  case class D(i: Int)

  def main(args: Array[String]): Unit = {

    import HSet._
    // println(dep.run)

    val a = A(12) :+: true :+: "hello world" :+: 1 :+: HEmpty

    val b = C(15) :+: A(15) :+: "str" :+: HEmpty


    def toHSet[S <: HSet, L <: HList](value: S)(implicit gen: ToHList.Aux[S, L]): L = gen(value)

    // println((a join b join c).replace[String]("replaced2"))

    // println(("str" :+: HEmpty) union (HEmpty))
    // println(Union.hsetEmptyUnion.apply(HEmpty, 1 :+: "str" :+: HEmpty))

    // println(a union b)


    val he: HEmpty.type = HEmpty
    val set: HSet = toHSet(he)



    // println(dep.run)

  }


}