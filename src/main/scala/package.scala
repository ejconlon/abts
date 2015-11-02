package object abts {
  type ABT[F[_]] = Fix[Node, F]
}