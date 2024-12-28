# UNITS-POC

This repo contains a rough-and-ready proof of concept meant to answer the
challenge posed [in this
article](https://yoric.github.io/post/rust-refinement-types/), which
concerns

> a static analyzer for Rust that checked that the code was using
> units of measures correctly, e.g. a distance in meters is not a
> distance in centimeters, dividing meters by seconds gave you a value
> in `m / s` (aka `m * s^-1`).

Specifically, the author evinces skepticism that Rust's unadorned type
system alone can allow one to compute with units such that different
routes to what should be the same unit are recognized by the system as
equivalent, subject to a set of reasonable desiderata. For instance,
given a value `a` representing a number of meters, and `b`
representing a number of seconds, then `a / (b * b)` and `(a / b) / b`
should both represent _meters per second squared_. The desiderata are
that there be no set of units defined in advance once and for all (ie
it should be extensible by clients), and there be no need to manually
specify equivalences between units (eg between meters per second, per
second, and meters per second-squared).

This crate provides values tagged with units. Units are implemented as
a zero-sized type parameterized by two type-level lists containing
types implementing the `BaseUnit` trait. The two lists represent the
numerator and denominator of a fraction, and they are maintained in
sorted order by unit name. (Because comparing `TypeId`s is not yet
possible in a const context, this means that the `BaseUnit` trait has
an associated constant which provides the name of the unit. This is in
fact the only thing the trait does; it is purely a named tag.) Given the
following:

```rust
defunit!(Meter);
defunit!(Second);
let a = Meter::new(1,0);
let b = Second::new(2.0);
```

Then both `a / (b * b)` and `(a / b) / b` will have the inferred type
`Value<f64, Units<Cons<Meter, Nil>, Cons<Second, Cons<Second, Nil>>>>`
(a macro is provided to enable writing explicit types more easily:
`vtype!(f64; Meter / Second, Second)`). We canonicalize the
representation whenever a multiplication or division is performed;
multiplying by `c = Second::new(3.0)` will remove the first `Second`
type from the second list. Sortability gives us commutativity of
multiplication by default (`a * b` and `b * a` will both give the type
`Value<f64, Units<Cons<Second, Cons<Meter, Nil>>>, Nil>>`) and also
means that merging and simplifying can be implemented fairly easily
and not extremely inefficiently.

Since units are just an essentially meaningless tag, they are
perfectly well definable in other crates and in fact this crate only
defines units for testing, and since equivalence is simply having the
same canonical form, we have the desiderata from the blog post. On the
other hand, for specifically *units of measure*, this is not
necessarily what you really want; there is no way, for instance, to
express that yards and meters are both units of length and should
perhaps be interconvertible (albeit not implicitly), the way you can
do in [Frink](https://frinklang.org/). On the final hand, this saves
me from having to have an opinion on whether or not [the candela is a
scam](https://frinklang.org/frinkdata/units.txt), and being able to
answer the question "how big of a cube is a gallon" is not really the
problem at issue.

# FAQ

## Do you have any regrets?

Yes. Before I realized that it was possible to use the `Units` struct
for everything by making it impossible to construct a `Value` tagged
with a `BaseUnit`, there was a much more involved set of
implementations of `Mul` and `Div`, which used some neat type-level
tricks including a [type-level
Maybe](https://github.com/bwo/units-poc/commit/cc23afa808e456ae32f26baf1515c5e856dbd836#diff-06060802aa63cf0eea53aba4bb5b2c88a0b96d2dc21bbe7a7736d21e31b127b5L90-L131)
(aka `Option`, but I used `Maybe`/`Nothing`/`Just` to avoid stepping
on the existing type). However, since it was actually unnecessary, I
took it out. Alas.
