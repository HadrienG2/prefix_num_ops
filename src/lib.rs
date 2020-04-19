//! # Introduction
//!
//! If you're trying to do scientific computing in Rust, and you can't get used
//! to mathematical functions like `sin()` or `cos()` being postfix methods,
//! this crate may be for you!
//!
//! It provides free function versions of the trait methods of the `num` traits,
//! so that you can easily do things like `sin(x) + 3*ln(y)`.
//!
//! Each trait's methods are exposed as an module of free functions, named after
//! a snake_case version of the trait's name, and it only takes a couple of use
//! clauses to go from there to using the above syntax in your math expressions.
//!
//!
//! # API coverage
//!
//! This crate generally opts for maximal coverage of the num traits, except in
//! the following circumstances:
//!
//! - The trait represents an operator whose standard notation is closer to a
//!   postfix method than to a prefix function, as is the case for most binary
//!   operators. For this reason, `AsPrimitive`, `CheckedAdd`, `CheckedDiv`,
//!   `CheckedMul`, `CheckedRem`, `CheckedShl`, `CheckedShr`, `CheckedSub`,
//!   `MulAdd`, `MulAddAssign`, `Saturating`, `WrappingAdd`, `WrappingMul`,
//!   `WrappingShl`, `WrappingShr` and `WrappingSub` are not covered.
//! - The num_trait crate already provides a set of free functions that cover
//!   90% of a trait's functionality, and we re-export them. Thus, `One`,
//!   `Signed` and `Zero` are not covered.
//! - A specific trait or its methods would require very significant supporting
//!   infrastructure to be exposed as a free function by this crate, and the
//!   extent of its real-world usage does not seem to justify the effort.
//!   To be more specific...
//!     * `FloatConst::TAU()` would require adding Self trait bound support to
//!       the underlying macro infrastructure, while `TAU` is arguably a math
//!       expert joke that most normal persons would spell out as `2.0 * PI`.
//!     * `MulAdd` and `MulAddAssign` would require adding generic trait support
//!       to the underlying macro infrastructure, while it is debatable whether
//!       a multiply-add should be considered a prefix or postfix operator.
//!     * `NumCast` would require adding generic trait method support, when it
//!       is dubious whether `from::<T, _>(n)` is actually a readability
//!       improvement over the `T::from(n)` that it replaces.
//!     * `i128`-based casts would require extending this crate's conditional
//!       compilation setup quite a bit through use of the `autocfg` crate,
//!       which seems to be a bit much considering how obscure that type is.
//!
//! If you find a num trait functionality which is neither exposed by this
//! crate nor covered by the above list, this is likely an oversight from my
//! part, please ping me about it.
//!
//! I am also willing to reconsider any point of the above policy if someone
//! manages to make a good argument against it. Issues are welcome!
//!
//!
//! # Limitations
//!
//! ## Documentation
//!
//! Only a one-line summary of each method's documentation is provided. Please
//! refer to the corresponding trait method's documentation in `num_traits` for
//! the full details of each function's API contract.
//!
//! ## Namespace collisions
//!
//! One advantage of using a trait-based approach like `num_traits` instead of
//! free functions like this crate is that trait methods gracefully handle
//! namespace collisions.
//!
//! With this crate, you will instead be the one responsible for only `use`-ing
//! one function with a given name at a time.
//!
//! For what it's worth, this is why programming languages with prefix numerical
//! methods usually also support method overloading. But Rust could not have
//! that language feature, as it would break the kind of advanced type inference
//! that all Rustaceans are used to enjoy today...

#![deny(missing_docs)]
#![no_std]

#[cfg(feature = "std")]
extern crate std;

// Only `no_std` builds actually use `libm`.
#[cfg(all(not(feature = "std"), feature = "libm"))]
extern crate libm;

use core::num::FpCategory;

// Re-export the underlying num_traits API.
pub use num_traits;

/// Mechanism to expose methods from num's traits as modules of free functions
macro_rules! define_prefix_methods {
    (
        $(
            $(#[$module_attrs:meta])*
            $trait_path:path => $module_name:ident {
            $(
                $(#[$method_attrs:meta])*
                $method_name:ident $method_args:tt -> $method_result:tt
            )*
        } )*
    ) => (
        $(
            $(#[$module_attrs])*
            #[allow(unused_imports)]
            pub mod $module_name {
                use super::*;

                $(
                    define_method! {
                        $(#[$method_attrs])*
                        $trait_path : $method_name $method_args -> $method_result
                    }
                )*
            }
        )*
    )
}
//
// TODO: Try to nerd-snipe a macro expert like dtolnay into either
//       generalizing/deduplicating this, or proving that it cannot be done
//       while operating within macro_rules' limitations.
//       I don't want to go for full proc macros because the compile time hit is
//       too high while declarative macros do the job, although in a clunky way.
//
// NOTE: Some function return types must be parenthesized because no macro
//       matcher seems fully adequate for matching a function return type above.
//       `ty` and `path` do not match `Self`, while `tt` does not match paths
//       and generic instantiations.
//
// NOTE: I would like this macro not to care about method attributes, but
//       it seems that expanding the attributes above the `define_method!` macro
//       invocation does not have the intended effect, at least as far as
//       rustdoc documentation is concerned.
//
macro_rules! define_method {
    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident() -> Self
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>() -> T {
            <T as $trait_path>::$method_name()
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident(x: Self) -> Self
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>(x: T) -> T {
            <T as $trait_path>::$method_name(x)
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident($($arg:ident: $arg_ty:ty),*) -> (Option<Self>)
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>($($arg: $arg_ty),*) -> Option<T> {
            <T as $trait_path>::$method_name($($arg),*)
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident($($arg:ident: $arg_ty:ty),*) -> (Result<Self, Self::$assoc_type:ident>)
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>($($arg: $arg_ty),*) -> Result<T, T::$assoc_type> {
            <T as $trait_path>::$method_name($($arg),*)
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident(self) -> Self
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>(self_: T) -> T {
            self_.$method_name()
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident(self) -> (Self::$assoc_type:ident)
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>(self_: T) -> T::$assoc_type {
            self_.$method_name()
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident(self) -> (Self, Self)
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>(self_: T) -> (T, T) {
            self_.$method_name()
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident(self) -> $return_type:ty
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>(self_: T) -> $return_type {
            self_.$method_name()
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident(self, $($self_arg:ident: Self),*) -> Self
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>(self_: T, $($self_arg: T),*) -> T {
            self_.$method_name($($self_arg),*)
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident(self, $($arg:ident: $arg_ty:ty),*) -> Self
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>(self_: T, $($arg: $arg_ty),*) -> T {
            self_.$method_name($($arg),*)
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident(&self) -> (Option<Self>)
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>(self_: &T) -> Option<T> {
            self_.$method_name()
        }
    };

    (
        $(#[$method_attrs:meta])*
        $trait_path:path : $method_name:ident(&self) -> ($return_type:ty)
    ) => {
        $(#[$method_attrs])*
        pub fn $method_name<T: $trait_path>(self_: &T) -> $return_type {
            self_.$method_name()
        }
    };
}
//
define_prefix_methods! {
    /// Methods from the Bounded trait, exposed as free functions.
    num_traits::bounds::Bounded => bounded {
        /// Returns the smallest finite number this type can represent.
        min_value() -> Self

        /// Returns the largest finite number this type can represent.
        max_value() -> Self
    }

    /// Methodes from the FromPrimitive trait, exposed as free functions.
    num_traits::cast::FromPrimitive => from_primitive {
        /// Converts an `i64` to return an optional value of this type.
        from_i64(n: i64) -> (Option<Self>)

        /// Converts an `u64` to return an optional value of this type.
        from_u64(n: u64) -> (Option<Self>)

        /// Converts an `isize` to return an optional value of this type.
        from_isize(n: isize) -> (Option<Self>)

        /// Converts an `i8` to return an optional value of this type.
        from_i8(n: i8) -> (Option<Self>)

        /// Converts an `i16` to return an optional value of this type.
        from_i16(n: i16) -> (Option<Self>)

        /// Converts an `i32` to return an optional value of this type.
        from_i32(n: i32) -> (Option<Self>)

        // NOTE: from_i128 is not supported, see crate docs to understand why.

        /// Converts an `usize` to return an optional value of this type.
        from_usize(n: usize) -> (Option<Self>)

        /// Converts an `u8` to return an optional value of this type.
        from_u8(n: u8) -> (Option<Self>)

        /// Converts an `u16` to return an optional value of this type.
        from_u16(n: u16) -> (Option<Self>)

        /// Converts an `u32` to return an optional value of this type.
        from_u32(n: u32) -> (Option<Self>)

        // NOTE: from_u128 not supported, see crate docs to understand why.

        /// Converts an `f32` to return an optional value of this type.
        from_f32(n: f32) -> (Option<Self>)

        /// Converts an `f64` to return an optional value of this type.
        from_f64(n: f64) -> (Option<Self>)
    }

    /// Methodes from the ToPrimitive trait, exposed as free functions.
    num_traits::cast::ToPrimitive => to_primitive {
        /// Converts the input to an `i64`.
        to_i64(&self) -> (Option<i64>)

        /// Converts the input to an `u64`.
        to_u64(&self) -> (Option<u64>)

        /// Converts the input to an `isize`.
        to_isize(&self) -> (Option<isize>)

        /// Converts the input to an `i8`.
        to_i8(&self) -> (Option<i8>)

        /// Converts the input to an `i16`.
        to_i16(&self) -> (Option<i16>)

        /// Converts the input to an `i32`.
        to_i32(&self) -> (Option<i32>)

        // NOTE: to_i128 is not supported, see crate docs to know why.

        /// Converts the input to an `usize`.
        to_usize(&self) -> (Option<usize>)

        /// Converts the input to an `u8`.
        to_u8(&self) -> (Option<u8>)

        /// Converts the input to an `u16`.
        to_u16(&self) -> (Option<u16>)

        /// Converts the input to an `u32`.
        to_u32(&self) -> (Option<u32>)

        // NOTE: to_u128 not supported, see crate docs to know why.

        /// Converts the input to an `f32`.
        to_f32(&self) -> (Option<f32>)

        /// Converts the input to an `f64`.
        to_f64(&self) -> (Option<f64>)
    }

    /// Methods from the Float trait, exposed as free functions.
    #[cfg(any(feature = "std", feature = "libm"))]
    num_traits::float::Float => float {
        /// Returns the `NaN` value.
        nan() -> Self

        /// Returns the infinite value.
        infinity() -> Self

        /// Returns the negative infinite value.
        neg_infinity() -> Self

        /// Returns `-0.0`.
        neg_zero() -> Self

        /// Returns the smallest finite value that this type can represent.
        min_value() -> Self

        /// Returns the smallest positive, normalized value that this type can represent.
        min_positive_value() -> Self

        /// Returns the largest finite value that this type can represent.
        max_value() -> Self

        /// Returns `true` if this value is `NaN` and false otherwise.
        is_nan(self) -> bool

        /// Returns `true` if this value is positive or negative infinity
        /// and `false` otherwise.
        is_infinite(self) -> bool

        /// Returns `true` if this number is neither infinite nor `NaN`.
        is_finite(self) -> bool

        /// Returns `true` if the number is neither zero, infinite, subnormal, or `NaN`.
        is_normal(self) -> bool

        /// Returns the floating point category of the number.
        classify(self) -> FpCategory

        /// Returns the largest integer less than or equal to a number.
        floor(self) -> Self

        /// Returns the smallest integer greater than or equal to a number.
        ceil(self) -> Self

        /// Returns the nearest integer to a number.
        /// Rounds half-way cases away from `0.0`.
        round(self) -> Self

        /// Returns the integer part of a number.
        trunc(self) -> Self

        /// Returns the fractional part of a number.
        fract(self) -> Self

        /// Computes the absolute value of a number.
        abs(self) -> Self

        /// Returns a number that represents the sign of the input.
        signum(self) -> Self

        /// Returns true if the input is positive.
        is_sign_positive(self) -> bool

        /// Returns true if the input is negative.
        is_sign_negative(self) -> bool

        /// Fused multiply-add.
        mul_add(self, a: Self, b: Self) -> Self

        /// Takes the reciprocal (inverse) of a number, `1/x`.
        recip(self) -> Self

        /// Raises a number to an integer power.
        powi(self, n: i32) -> Self

        /// Raises a number to a floating point power.
        powf(self, n: Self) -> Self

        /// Takes the square root of a number.
        sqrt(self) -> Self

        /// Returns `e^x`, (the exponential function).
        exp(self) -> Self

        /// Returns `2^x`.
        exp2(self) -> Self

        /// Returns the natural logarithm of the number.
        ln(self) -> Self

        /// Returns the logarithm of the number with respect to an arbitrary base.
        log(self, base: Self) -> Self

        /// Returns the base 2 logarithm of the number.
        log2(self) -> Self

        /// Returns the base 10 logarithm of the number.
        log10(self) -> Self

        /// Returns the maximum of the two numbers.
        max(self, other: Self) -> Self

        /// Returns the minimum of the two numbers.
        min(self, other: Self) -> Self

        /// The positive difference of two numbers.
        abs_sub(self, other: Self) -> Self

        /// Takes the cubic root of a number.
        cbrt(self) -> Self

        /// Computes the length of the hypotenuse of a right-angle triangle
        /// given its legs' lengths.
        hypot(self, other: Self) -> Self

        /// Computes the sine of a number (in radians).
        sin(self) -> Self

        /// Computes the cosine of a number (in radians).
        cos(self) -> Self

        /// Computes the tangent of a number (in radians).
        tan(self) -> Self

        /// Computes the arcsine of a number.
        asin(self) -> Self

        /// Computes the arccosine of a number.
        acos(self) -> Self

        /// Computes the arctangent of a number.
        atan(self) -> Self

        /// Computes the four quadrant arctangent of two numbers.
        atan2(self, other: Self) -> Self

        /// Simultaneously computes the sine and cosine of a number `x`.
        /// Returns `(sin(x), cos(x))`.
        sin_cos(self) -> (Self, Self)

        /// Returns `e^(self) - 1` in a way that is accurate even
        /// if the number is close to zero.
        exp_m1(self) -> Self

        /// Returns `ln(1+n)` (natural logarithm) more accurately than
        /// if the operations were performed separately.
        ln_1p(self) -> Self

        /// Hyperbolic sine function.
        sinh(self) -> Self

        /// Hyperbolic cosine function.
        cosh(self) -> Self

        /// Hyperbolic tangent function.
        tanh(self) -> Self

        /// Inverse hyperbolic sine function.
        asinh(self) -> Self

        /// Inverse hyperbolic cosine function.
        acosh(self) -> Self

        /// Inverse hyperbolic tangent function.
        atanh(self) -> Self

        /// Returns the mantissa, base 2 exponent, and sign as integers, respectively.
        integer_decode(self) -> (u64, i16, i8)

        /// Returns epsilon, a small positive value.
        epsilon() -> Self

        /// Converts radians to degrees.
        to_degrees(self) -> Self

        /// Converts degrees to radians.
        to_radians(self) -> Self
    }

    /// Methods from the FloatConst trait, exposed as free functions.
    #[allow(non_snake_case)]
    num_traits::float::FloatConst => float_const {
        /// Returns Euler’s number.
        E() -> Self

        /// Returns `1.0 / π`.
        FRAC_1_PI() -> Self

        /// Returns `1.0 / sqrt(2.0)`.
        FRAC_1_SQRT_2() -> Self

        /// Returns `2.0 / π`.
        FRAC_2_PI() -> Self

        /// Returns `2.0 / sqrt(π)`.
        FRAC_2_SQRT_PI() -> Self

        /// Returns `π / 2.0`.
        FRAC_PI_2() -> Self

        /// Returns `π / 3.0`.
        FRAC_PI_3() -> Self

        /// Returns `π / 4.0`.
        FRAC_PI_4() -> Self

        /// Returns `π / 6.0`.
        FRAC_PI_6() -> Self

        /// Returns `π / 8.0`.
        FRAC_PI_8() -> Self

        /// Returns `ln(10.0)`.
        LN_10() -> Self

        /// Returns `ln(2.0)`.
        LN_2() -> Self

        /// Returns `log10(e)`.
        LOG10_E() -> Self

        /// Returns `log2(e)`.
        LOG2_E() -> Self

        /// Returns Archimedes’ constant `π`.
        PI() -> Self

        /// Returns `sqrt(2.0)`.
        SQRT_2() -> Self
    }

    /// Methods from the FloatCore trait, exposed as free functions.
    num_traits::float::FloatCore => float_core {
        /// Returns positive infinity.
        infinity() -> Self

        /// Returns negative infinity.
        neg_infinity() -> Self

        /// Returns `NaN`.
        nan() -> Self

        /// Returns `-0.0`.
        neg_zero() -> Self

        /// Returns the smallest finite value that this type can represent.
        min_value() -> Self

        /// Returns the smallest positive, normalized value that this type can represent.
        min_positive_value() -> Self

        /// Returns epsilon, a small positive value.
        epsilon() -> Self

        /// Returns the largest finite value that this type can represent.
        max_value() -> Self

        /// Returns the floating point category of the number.
        classify(self) -> FpCategory

        /// Converts to degrees, assuming the number is in radians.
        to_degrees(self) -> Self

        /// Converts to radians, assuming the number is in degrees.
        to_radians(self) -> Self

        /// Returns the mantissa, base 2 exponent, and sign as integers, respectively.
        integer_decode(self) -> (u64, i16, i8)

        /// Returns true if the number is `NaN`.
        is_nan(self) -> bool

        /// Returns true if the number is infinite.
        is_infinite(self) -> bool

        /// Returns true if the number is neither infinite or `NaN`.
        is_finite(self) -> bool

        /// Returns true if the number is neither zero, infinite, subnormal or `NaN`.
        is_normal(self) -> bool

        /// Returns the largest integer less than or equal to a number.
        floor(self) -> Self

        /// Returns the smallest integer greater than or equal to a number.
        ceil(self) -> Self

        /// Returns the nearest integer to a number. Round half-way cases away from `0.0`.
        round(self) -> Self

        /// Returns the integer part of a number.
        trunc(self) -> Self

        /// Returns the fractional part of a number.
        fract(self) -> Self

        /// Computes the absolute value of a number.
        abs(self) -> Self

        /// Returns a number that represents the sign of.
        signum(self) -> Self

        /// Returns true if the input is positive.
        is_sign_positive(self) -> bool

        /// Returns true if the input is negative.
        is_sign_negative(self) -> bool

        /// Returns the minimum of the two numbers.
        min(self, other: Self) -> Self

        /// Returns the maximum of the two numbers.
        max(self, other: Self) -> Self

        /// Returns the reciprocal (multiplicative inverse) of the number.
        recip(self) -> Self

        /// Raise a number to an integer power.
        powi(self, exp: i32) -> Self
    }

    /// Methods from the PrimInt trait, exposed as free functions.
    num_traits::int::PrimInt => prim_int {
        /// Returns the number of ones in the binary representation of the input.
        count_ones(self) -> u32

        /// Returns the number of zeros in the binary representation of the input.
        count_zeros(self) -> u32

        /// Returns the number of leading zeros in the binary representation of the input.
        leading_zeros(self) -> u32

        /// Returns the number of trailing zeros in the binary representation of the input.
        trailing_zeros(self) -> u32

        /// Shifts the bits to the left by a specified amount amount, `n`,
        /// wrapping the truncated bits to the end of the resulting integer.
        rotate_left(self, n: u32) -> Self

        /// Shifts the bits to the right by a specified amount amount, `n`,
        /// wrapping the truncated bits to the beginning of the resulting integer.
        rotate_right(self, n: u32) -> Self

        /// Shifts the bits to the left by a specified amount amount, `n`,
        /// filling zeros in the least significant bits.
        signed_shl(self, n: u32) -> Self

        /// Shifts the bits to the right by a specified amount amount, `n`,
        /// copying the "sign bit" in the most significant bits even for unsigned types.
        signed_shr(self, n: u32) -> Self

        /// Shifts the bits to the left by a specified amount amount, `n`,
        /// filling zeros in the least significant bits.
        unsigned_shl(self, n: u32) -> Self

        /// Shifts the bits to the right by a specified amount amount, `n`,
        /// filling zeros in the most significant bits.
        unsigned_shr(self, n: u32) -> Self

        /// Reverses the byte order of the integer.
        swap_bytes(self) -> Self

        /// Convert an integer from big endian to the target's endianness.
        from_be(x: Self) -> Self

        /// Convert an integer from little endian to the target's endianness.
        from_le(x: Self) -> Self

        /// Convert the input to big endian from the target's endianness.
        to_be(self) -> Self

        /// Convert the input to little endian from the target's endianness.
        to_le(self) -> Self

        /// Raises a number to the power of exp, using exponentiation by squaring.
        pow(self, exp: u32) -> Self
    }

    /// Methods from the Num trait, exposed as free functions
    num_traits::Num => num {
        /// Convert from a string and radix <= 36.
        from_str_radix(str: &str, radix: u32) -> (Result<Self, Self::FromStrRadixErr>)
    }

    /// Methods from the CheckedNeg trait, exposed as free functions
    num_traits::ops::checked::CheckedNeg => checked_neg {
        /// Negates a number, returning None for results that can't be represented.
        checked_neg(&self) -> (Option<Self>)
    }

    /// Methods from the Inv trait, exposed as free functions
    num_traits::ops::inv::Inv => inv {
        /// Returns the multiplicative inverse of a number.
        inv(self) -> (Self::Output)
    }

    /// Methods from the Real trait, exposed as free functions.
    #[cfg(any(feature = "std", feature = "libm"))]
    num_traits::real::Real => real {
        /// Returns the smallest finite value that this type can represent.
        min_value() -> Self

        /// Returns the smallest positive, normalized value that this type can represent.
        min_positive_value() -> Self

        /// Returns epsilon, a small positive value.
        epsilon() -> Self

        /// Returns the largest finite value that this type can represent.
        max_value() -> Self

        /// Returns the largest integer less than or equal to a number.
        floor(self) -> Self

        /// Returns the smallest integer greater than or equal to a number.
        ceil(self) -> Self

        /// Returns the nearest integer to a number.
        round(self) -> Self

        /// Returns the integer part of a number.
        trunc(self) -> Self

        /// Returns the fractional part of a number.
        fract(self) -> Self

        /// Computes the absolute value of a number.
        abs(self) -> Self

        /// Returns a number that represents the sign of the input.
        signum(self) -> Self

        /// Returns true if the input is positive.
        is_sign_positive(self) -> bool

        /// Returns true if the input is negative.
        is_sign_negative(self) -> bool

        /// Fused multiply-add.
        mul_add(self, a: Self, b: Self) -> Self

        /// Take the reciprocal (inverse) of a number, `1/x`.
        recip(self) -> Self

        /// Raise a number to an integer power.
        powi(self, n: i32) -> Self

        /// Raise a number to a real number power.
        powf(self, n: Self) -> Self

        /// Take the square root of a number.
        sqrt(self) -> Self

        /// Returns `e^x`, (the exponential function).
        exp(self) -> Self

        /// Returns `2^x`.
        exp2(self) -> Self

        /// Returns the natural logarithm of the number.
        ln(self) -> Self

        /// Returns the logarithm of the number with respect to an arbitrary base.
        log(self, base: Self) -> Self

        /// Returns the base 2 logarithm of the number.
        log2(self) -> Self

        /// Returns the base 10 logarithm of the number.
        log10(self) -> Self

        /// Converts radians to degrees.
        to_degrees(self) -> Self

        /// Converts degrees to radians.
        to_radians(self) -> Self

        /// Returns the maximum of two numbers.
        max(self, other: Self) -> Self

        /// Returns the minimum of two numbers.
        min(self, other: Self) -> Self

        /// Returns the positive difference of two numbers.
        abs_sub(self, other: Self) -> Self

        /// Takes the cubic root of a number.
        cbrt(self) -> Self

        /// Computes the length of the hypotenuse of a right-angle triangle
        /// given its legs' lengths.
        hypot(self, other: Self) -> Self

        /// Computes the sine of a number (in radians).
        sin(self) -> Self

        /// Computes the cosine of a number (in radians).
        cos(self) -> Self

        /// Computes the tangent of a number (in radians).
        tan(self) -> Self

        /// Computes the arcsine of a number.
        asin(self) -> Self

        /// Computes the arccosine of a number.
        acos(self) -> Self

        /// Computes the arctangent of a number.
        atan(self) -> Self

        /// Computes the four quadrant arctangent of two numbers.
        atan2(self, other: Self) -> Self

        /// Simultaneously computes the sine and cosine of a number `x`.
        /// Returns `(sin(x), cos(x))`.
        sin_cos(self) -> (Self, Self)

        /// Returns `e^(self) - 1` in a way that is accurate even if
        /// the number is close to zero.
        exp_m1(self) -> Self

        /// Returns `ln(1+n)` (natural logarithm) more accurately than
        /// if the operations were performed separately.
        ln_1p(self) -> Self

        /// Hyperbolic sine function.
        sinh(self) -> Self

        /// Hyperbolic cosine function.
        cosh(self) -> Self

        /// Hyperbolic tangent function.
        tanh(self) -> Self

        /// Inverse hyperbolic sine function.
        asinh(self) -> Self

        /// Inverse hyperbolic cosine function.
        acosh(self) -> Self

        /// Inverse hyperbolic tangent function.
        atanh(self) -> Self
    }
}
