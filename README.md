# Introduction

If you're trying to do scientific computing in Rust, and you can't get used
to mathematical functions like sin() or cos() being postfix methods, this
crate may be for you!

It provides free function versions of the trait methods of the `num` traits,
so that you can easily do things like `sin(x) + 3*ln(y)`.

Each trait's methods are exposed as an module of free functions, named after
a snake_case version of the trait's name, and it only takes a couple of use
clauses to go from there to using the above syntax in your math expressions.

Only a one-line summary of each method's documentations is provided, please
refer to the corresponding trait method's documentation in `num_traits` for
the full details of each function's API contract.

Note that one advantage of the num_traits' trait-based approach over free
functions is that trait methods gracefully handle namespace collisions.
Whereas with this crate, you will be the one responsible for `use`-ing the
right methods for a given task. For what it's worth, this is why programming
languages with prefix numerical methods usually also support method
overloading, which Rust could not support as it would break the kind of
advanced type inference that all Rustaceans are used to enjoy.

# What's included

This crate generally opts for maximal coverage of the num traits, except in
the following circumstances:

- The trait represents an operator whose standard notation is closer to a
  postfix method than to a prefix function, as is the case for most binary
  operators. For this reason, `AsPrimitive`, `CheckedAdd`, `CheckedDiv`,
  `CheckedMul`, `CheckedRem`, `CheckedShl`, `CheckedShr`, `CheckedSub`,
  `MulAdd`, `MulAddAssign`, `Saturating`, `WrappingAdd`, `WrappingMul`,
  `WrappingShl`, `WrappingShr` and `WrappingSub` are not covered.
- The num_trait crate already provides a set of free functions that cover
  90% of a trait's functionality, and we re-export them. Thus, `One`,
  `Signed` and `Zero` are not covered.
- A specific trait or its methods would require very significant supporting
  infrastructure to be exposed as a free function by this crate, and the
  extent of its real-world usage does not seem to justify the effort.
  To be more specific...
    * `FloatConst::TAU()` would require adding Self trait bound support to
      the underlying macro infrastructure, while `TAU` is arguably a math
      expert joke that most normal persons would spell out as `2.0 * PI`.
    * `MulAdd` and `MulAddAssign` would require adding generic trait support
      to the underlying macro infrastructure, while it is debatable whether
      a multiply-add should be considered a prefix or postfix operator.
    * `NumCast` would require adding generic trait method support, when it
      is dubious whether `from::<T, _>(n)` is actually a readability
      improvement over the `T::from(n)` that it replaces.
    * `i128`-based casts would require extending this crate's conditional
      compilation setup quite a bit through use of the `autocfg` crate,
      which seems to be a bit much considering how obscure that type is.

If you find a num trait functionality which is neither exposed by this
crate nor covered by the above list, this is likely an oversight from my
part, please ping me about it.

I am also willing to reconsider any point of the above policy if someone
manages to make a good argument against it. Issues are welcome!
