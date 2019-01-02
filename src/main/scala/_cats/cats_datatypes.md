# Cats Data types

## EitherT 
`EitherT[F[_], A, B]` is equivalent to `F[Either[A, B]]`
If `F[_]` is a monad, then `EitherT` will also form a monad.
