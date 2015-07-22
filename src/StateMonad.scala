/**
 * State Monad to pass state during monadic chaining of function calls.
 * Examples: Counting duration, continual discrete world state, ...
 *
 * @author bogdanski
 * @since 22.07.15
 */

trait Functor[A, F[_]] {
  def map[B](f: A => B): F[B]
}

trait MultiFunctor[A, State, F[_, _]] extends Functor[A, ({type λ[α] = F[α, State]})#λ] {
  def map[B](f: (A) => B): F[B, State]
}

case class StateMonad[A, State](value: A, state: State, stateTrans: State => State) extends MultiFunctor[A, State, StateMonad] {
  def map[B](f: A => B): StateMonad[B, State] = StateMonad(f(value), stateTrans(state), stateTrans)
}