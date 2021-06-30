# headlens

An exploration of the idea of using a lens for setting a head of a list.
This lens, called `_head`, would only change the head of a list and add an
element on empty lists. The exploration is done in the form of creating
candidate lenses and testing them.

My conclusion is that I can create a [Setter](https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Setter.html#t:Setter)
of type `Setter [a] (NonEmpty [a]) (Maybe a) a` that behaves as desired and is
type-safe.
The object would even have the type of a Traversal, but it wouldn't satisfy
the laws stipulated in the documentation (they require `Traversal'` type).
A comment remarks that a Traversal shouldn't increase the number of elements.
My head lens would contradict that property as it would add a head to an empty list.

`Setter' [a] (Maybe a)` that removes the head when provided with nothing, e.g.,
with `set l Nothing` doesn't satisfy setter laws. It doesn't satisfy `over l f
. over l g ≡ over l (f . g)` as the left side can remove the head twice while
the right side can do it at most once.
There's a commented test-case for this in unit-tests.

It's impossible to create a Lens for this, because Lens encodes a has-a
relationship but empty lists have no heads. Types won't work out (
`view` is impossible to implement).

I don't think it's possible to have a `Prism` type for this either. The `Prism`
type enforces an is-a relationship, i.e., `Prism [a] (NonEmpty [a]) (Maybe a) a`
would mean that `set prism x xs == [x]` (any tail of `xs` gets discarded).
