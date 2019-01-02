## Type classes
A type class is an interface that represents some functionality we want to implement.

### Bracket

### Sync
Monad that can suspect execution of side effects in the `F[_]` context

### LiftIO
Monad that can convert any `IO[A]` to `F[A]`
- Useful for defining parametric signatures
- Composing monad transformer stacks

### Async
Monad that can describe synchronous or asynchronous computations that produce exactly 1 result.
Model data types that can
- Start asynchronous processes (that are not cancellable)
- Emit 1 result on completion
- End in error

#### Asynchrony
An asynchronous task represents logic that executes independently of the main program flow. It can be a task whose result gets computed on another thread, or on the network. 
These tasks are usually represented with the type signature
```scala
(A => Unit) => Unit
```

And in the case where we allow for signalling an error condition
```scala
(Either[Throwable, A] => Unit) => Unit
```
This is the type of `Async.async` builder

```scala
import cats.effect.{IO, Async}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}

val apiCall: Future[String] = Future.successful("I come from the Future!")

val ioa: IO[String] =
  Async[IO].async { cb =>
  	val y: Either[Throwable, String] => Unit = cb //just to see the type signature
    import scala.util.{Failure, Success}
    val x: Unit = apiCall.onComplete {
      case Success(value) => cb(Right(value)) // What does this actually do?
      case Failure(error) => cb(Left(error))
    }
    x
  }

ioa.unsafeRunSync()
```

`cb` is of type `Either[Throwable, String] => Unit`. i.e. it is a function. We call this function on the result of `apiCall.onComplete` (after converting to `Either`), so that we can yield a result of type `Unit`. 
But how does it actually transform the `Either` to `Unit` ?

### Concurrent 
Type class for Async, but cancellable.


### Effect

### ConcurrentEffect