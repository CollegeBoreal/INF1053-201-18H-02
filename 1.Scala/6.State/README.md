# State


https://github.com/scalaz/scalaz/blob/series/7.3.x/CONTRIBUTING.md#methods

Use currying sparingly. Less indirection helps performance. Curried versions of methods may be offered as alternatives.

```
def fold[A, B](fa: F[A])(f: (A, B) => B): F[B] // good
def fold[A, B](fa: F[A])(f: A => B => B): F[B] // bad
def fold[A, B](fa: F[A]): A => B => B => F[B] // bad
final def foldCurried[A, B](fa: F[A]): (A => B => B) => F[B] = f => fold(fa)((a, b) => f(a)(b)) // okay
```
