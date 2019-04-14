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

  protected def runWithInjected(environment: Env, injected: Inj): A

  def run(implicit envNil: HNil =:= Env, injNil: HNil =:= Inj): A =
    runWithInjected(envNil(HNil), injNil(HNil))

  def run(env: Env)(implicit eq: HNil =:= Inj): A =
    runWithInjected(env, eq(HNil))

  def map[B, EnvWoInjOut <: HList](f: A => B)
                                                    (implicit filterNot: Remove.Aux[Env, A, EnvWoInjOut],
                                                     injUnique: NotContainsConstraint[self.Inj, A],
                                                    ): Dep2.Aux[EnvWoInjOut, A :: Inj, B] = new Dep2[B] {
    override type Env = EnvWoInjOut
    override type Inj = A :: self.Inj

    override protected def runWithInjected(environment: Env, injected: Inj): B = {
      val parentEnv = filterNot.reinsert(environment)

      val parentRun = self.runWithInjected(parentEnv, injected.tail)

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
            (f: A => Dep2.Aux[Env2, Inj2, B])
            (implicit union: Union.Aux[Env, Env2, CombinedEnv],
             injUnique: NotContainsConstraint[self.Inj, B],
             diff: Diff.Aux[CombinedEnv, B :: self.Inj, CombinedEnvMinusInjected]
            ): Dep2.Aux[CombinedEnv, A :: Inj, B] = new Dep2[B] {
    override type Env = CombinedEnv
    override type Inj = A :: self.Inj

    override protected def runWithInjected(environment: Env, injected: Inj): B = ???
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

    override protected def runWithInjected(environment: Env, injected: Inj): A = f
  }

  def reader[A,B](f: A => B): Aux[A :: HNil, A :: HNil, B] = new Dep2[B] {
    override type Env = A :: HNil
    override type Inj = A :: HNil

    override protected def runWithInjected(environment: Env, injected: Inj): B = f(environment.head)
  }
}

object Test {

  import Dep2._

  val ad = pure(1)
  val bd = pure("str")


  val a = 1 :: "test" :: "test2" :: 1L :: "test3" :: HNil
  val b = 1 :: "x" :: true :: HNil

  def uni[A <: HList, B <: HList, C <: HList](a: A, b: B)(implicit un: Intersection.Aux[A, B, C]):C =
    un(a)


  val c = uni(a, b)



//  val dep = for {
//    a <- ad
//    b <- bd
//  } yield a + b.length
//
  def main(args: Array[String]): Unit = {
    println(c)
  }


}
