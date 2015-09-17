/**
 * @author bogdanski
 * @since 22.07.15
 */

/** Functor Trait */
trait Functor[A, F[_]] {
  def map[B](f: A => B): F[B]
  def flatMap[B](f: A => F[B]): F[B]
}

/** Higher Functor Trait */
trait HigherFunctor[A, M[+_], F[_[_]]] {
  def map[B](f: A => B): F[M]
  def flatMap[B](f: A => M[B]): F[M]
}

/** Multi-Functor Trait */
trait MultiFunctor[A, State, F[_, _]] extends Functor[A, ({type λ[α] = F[α, State]})#λ] {
  def map[B](f: A => B): F[B, State]
  def flatMap[B](f: A => F[B, State]): F[B, State]
}

/** Higher Multi-Functor Trait */
trait HigherMultiFunctor[K, A[+_], State] extends HigherFunctor[K, A, ({type λ[α[+_]] = HigherMultiFunctor[_, α, State]})#λ] {
  def map[B](f: K => B): HigherMultiFunctor[B, A, State]
  def flatMap[B](f: K => A[B]): HigherMultiFunctor[B, A, State]
}

/**
 * State Monad to pass state during monadic chaining of function calls.
 * Examples: Counting duration, continual discrete world state, ...
 */
case class StateMonad[A, State](value: A, state: State, stateTrans: State => State) extends MultiFunctor[A, State, StateMonad] {
  def map[B](f: A => B): StateMonad[B, State] = StateMonad(f(value), stateTrans(state), stateTrans)
  def flatMap[B](f: A => StateMonad[B, State]): StateMonad[B, State] = f(value)
  def zip: StateMonad[(A, State), State] = StateMonad((value, state), stateTrans(state), stateTrans)
  def zipMap[B](f: A => B): StateMonad[(B, State), State] = StateMonad((f(value), state), stateTrans(state), stateTrans)
}


case class ListStateMonad[A, State](list: List[A], state: State, stateTrans: State => State) extends HigherMultiFunctor[A, List, State] {
  def map[B](f: A => B): ListStateMonad[B, State] = ListStateMonad(list map f, stateTrans(state), stateTrans)
  def flatMap[B](f: A => List[B]): ListStateMonad[B, State] = ListStateMonad(list flatMap f, stateTrans(state), stateTrans)
}