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
//!   operators. For this reason, `AsPrimitive`,
//!   `Checked(Add|Div|Mul|Rem|Shl|Shr|Sub)`, `MulAdd`, `MulAddAssign`,
//!   `Saturating(Add|Mul|Sub)?`, `Overflowing(Add|Mul|Sub)` and
//!   `Wrapping(Add|Mul|Shl|Shr|Sub)` are not covered.
//! - The num_trait crate already provides a set of free functions that cover
//!   90% of a trait's functionality, and we re-export them. Thus, `One`,
//!   `Signed` and `Zero` are not covered.
//! - A specific trait or its methods would require very significant supporting
//!   infrastructure to be exposed as a free function by this crate, and the
//!   extent of its real-world usage does not seem to justify the effort.
//!   To be more specific...
//!     * `FloatConst::TAU()` would require adding T trait bound support to
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

/// Parse method-like definitions for a given trait and module name and define a prefix method
/// for each one.
macro_rules! define_prefix_methods {
    (
        $(
            $( #[$module_attrs:meta] )*
            $trait_path:path => $module_name:ident {
                $(
                    $( #[$method_attrs:meta] )*
                    $method_name:ident $method_args:tt -> $method_result:ty
                )*
            }
        )*
    ) => {
        $(
            $( #[$module_attrs] )*
            #[allow(unused_imports)]
            pub mod $module_name {
                use super::*;

                $(
                    define_method! {
                        $( #[$method_attrs] )*
                        $trait_path : $method_name $method_args -> $method_result
                    }
                )*
            }
        )*
    }
}

/// Define a prefix method
macro_rules! define_method {
    (
        $( #[$method_attrs:meta] )*
        $trait_path:path : $method_name:ident ( $( $arg:ident : $arg_ty:ty ),* ) -> $return_type:ty
    ) => {
        $( #[$method_attrs] )*
        pub fn $method_name<T: $trait_path>( $( $arg : $arg_ty ),* ) -> $return_type {
            <T as $trait_path>::$method_name( $( $arg ),* )
        }
    };
}

define_prefix_methods! {
    /// Methods from the Bounded trait, exposed as free functions.
    num_traits::bounds::Bounded => bounded {
        /// Returns the smallest finite number this type can represent.
        min_value() -> T

        /// Returns the largest finite number this type can represent.
        max_value() -> T
    }

    /// Methodes from the FromPrimitive trait, exposed as free functions.
    num_traits::cast::FromPrimitive => from_primitive {
        /// Converts an `i64` to return an optional value of this type.
        from_i64(n: i64) -> Option<T>

        /// Converts an `u64` to return an optional value of this type.
        from_u64(n: u64) -> Option<T>

        /// Converts an `isize` to return an optional value of this type.
        from_isize(n: isize) -> Option<T>

        /// Converts an `i8` to return an optional value of this type.
        from_i8(n: i8) -> Option<T>

        /// Converts an `i16` to return an optional value of this type.
        from_i16(n: i16) -> Option<T>

        /// Converts an `i32` to return an optional value of this type.
        from_i32(n: i32) -> Option<T>

        // NOTE: from_i128 is not supported, see crate docs to understand why.

        /// Converts an `usize` to return an optional value of this type.
        from_usize(n: usize) -> Option<T>

        /// Converts an `u8` to return an optional value of this type.
        from_u8(n: u8) -> Option<T>

        /// Converts an `u16` to return an optional value of this type.
        from_u16(n: u16) -> Option<T>

        /// Converts an `u32` to return an optional value of this type.
        from_u32(n: u32) -> Option<T>

        // NOTE: from_u128 not supported, see crate docs to understand why.

        /// Converts an `f32` to return an optional value of this type.
        from_f32(n: f32) -> Option<T>

        /// Converts an `f64` to return an optional value of this type.
        from_f64(n: f64) -> Option<T>
    }

    /// Methodes from the ToPrimitive trait, exposed as free functions.
    num_traits::cast::ToPrimitive => to_primitive {
        /// Converts the input to an `i64`.
        to_i64(x: &T) -> Option<i64>

        /// Converts the input to an `u64`.
        to_u64(x: &T) -> Option<u64>

        /// Converts the input to an `isize`.
        to_isize(x: &T) -> Option<isize>

        /// Converts the input to an `i8`.
        to_i8(x: &T) -> Option<i8>

        /// Converts the input to an `i16`.
        to_i16(x: &T) -> Option<i16>

        /// Converts the input to an `i32`.
        to_i32(x: &T) -> Option<i32>

        // NOTE: to_i128 is not supported, see crate docs to know why.

        /// Converts the input to an `usize`.
        to_usize(x: &T) -> Option<usize>

        /// Converts the input to an `u8`.
        to_u8(x: &T) -> Option<u8>

        /// Converts the input to an `u16`.
        to_u16(x: &T) -> Option<u16>

        /// Converts the input to an `u32`.
        to_u32(x: &T) -> Option<u32>

        // NOTE: to_u128 not supported, see crate docs to know why.

        /// Converts the input to an `f32`.
        to_f32(x: &T) -> Option<f32>

        /// Converts the input to an `f64`.
        to_f64(x: &T) -> Option<f64>
    }

    /// Methods from the Float trait, exposed as free functions.
    #[cfg(any(feature = "std", feature = "libm"))]
    num_traits::float::Float => float {
        /// Returns the `NaN` value.
        nan() -> T

        /// Returns the infinite value.
        infinity() -> T

        /// Returns the negative infinite value.
        neg_infinity() -> T

        /// Returns `-0.0`.
        neg_zero() -> T

        /// Returns the smallest finite value that this type can represent.
        min_value() -> T

        /// Returns the smallest positive, normalized value that this type can represent.
        min_positive_value() -> T

        /// Returns the largest finite value that this type can represent.
        max_value() -> T

        /// Returns `true` if this value is `NaN` and false otherwise.
        is_nan(x: T) -> bool

        /// Returns `true` if this value is positive or negative infinity
        /// and `false` otherwise.
        is_infinite(x: T) -> bool

        /// Returns `true` if this number is neither infinite nor `NaN`.
        is_finite(x: T) -> bool

        /// Returns `true` if the number is neither zero, infinite, subnormal, or `NaN`.
        is_normal(x: T) -> bool

        /// Returns the floating point category of the number.
        classify(x: T) -> FpCategory

        /// Returns the largest integer less than or equal to a number.
        floor(x: T) -> T

        /// Returns the smallest integer greater than or equal to a number.
        ceil(x: T) -> T

        /// Returns the nearest integer to a number.
        /// Rounds half-way cases away from `0.0`.
        round(x: T) -> T

        /// Returns the integer part of a number.
        trunc(x: T) -> T

        /// Returns the fractional part of a number.
        fract(x: T) -> T

        /// Computes the absolute value of a number.
        abs(x: T) -> T

        /// Returns a number that represents the sign of the input.
        signum(x: T) -> T

        /// Returns true if the input is positive.
        is_sign_positive(x: T) -> bool

        /// Returns true if the input is negative.
        is_sign_negative(x: T) -> bool

        /// Fused multiply-add.
        mul_add(x: T, a: T, b: T) -> T

        /// Takes the reciprocal (inverse) of a number, `1/x`.
        recip(x: T) -> T

        /// Raises a number to an integer power.
        powi(x: T, n: i32) -> T

        /// Raises a number to a floating point power.
        powf(x: T, n: T) -> T

        /// Takes the square root of a number.
        sqrt(x: T) -> T

        /// Returns `e^x`, (x: The exponential function).
        exp(x: T) -> T

        /// Returns `2^x`.
        exp2(x: T) -> T

        /// Returns the natural logarithm of the number.
        ln(x: T) -> T

        /// Returns the logarithm of the number with respect to an arbitrary base.
        log(x: T, base: T) -> T

        /// Returns the base 2 logarithm of the number.
        log2(x: T) -> T

        /// Returns the base 10 logarithm of the number.
        log10(x: T) -> T

        /// Returns the maximum of the two numbers.
        max(x: T, other: T) -> T

        /// Returns the minimum of the two numbers.
        min(x: T, other: T) -> T

        /// The positive difference of two numbers.
        abs_sub(x: T, other: T) -> T

        /// Takes the cubic root of a number.
        cbrt(x: T) -> T

        /// Computes the length of the hypotenuse of a right-angle triangle
        /// given its legs' lengths.
        hypot(x: T, other: T) -> T

        /// Computes the sine of a number (in radians).
        sin(x: T) -> T

        /// Computes the cosine of a number (in radians).
        cos(x: T) -> T

        /// Computes the tangent of a number (in radians).
        tan(x: T) -> T

        /// Computes the arcsine of a number.
        asin(x: T) -> T

        /// Computes the arccosine of a number.
        acos(x: T) -> T

        /// Computes the arctangent of a number.
        atan(x: T) -> T

        /// Computes the four quadrant arctangent of two numbers.
        atan2(x: T, other: T) -> T

        /// Simultaneously computes the sine and cosine of a number `x`.
        /// Returns `(sin(x), cos(x))`.
        sin_cos(x: T) -> (T, T)

        /// Returns `e^(x: T) - 1` in a way that is accurate even
        /// if the number is close to zero.
        exp_m1(x: T) -> T

        /// Returns `ln(1+n)` (natural logarithm) more accurately than
        /// if the operations were performed separately.
        ln_1p(x: T) -> T

        /// Hyperbolic sine function.
        sinh(x: T) -> T

        /// Hyperbolic cosine function.
        cosh(x: T) -> T

        /// Hyperbolic tangent function.
        tanh(x: T) -> T

        /// Inverse hyperbolic sine function.
        asinh(x: T) -> T

        /// Inverse hyperbolic cosine function.
        acosh(x: T) -> T

        /// Inverse hyperbolic tangent function.
        atanh(x: T) -> T

        /// Returns the mantissa, base 2 exponent, and sign as integers, respectively.
        integer_decode(x: T) -> (u64, i16, i8)

        /// Returns epsilon, a small positive value.
        epsilon() -> T

        /// Converts radians to degrees.
        to_degrees(x: T) -> T

        /// Converts degrees to radians.
        to_radians(x: T) -> T
    }

    /// Methods from the FloatConst trait, exposed as free functions.
    #[allow(non_snake_case)]
    num_traits::float::FloatConst => float_const {
        /// Returns Euler’s number.
        E() -> T

        /// Returns `1.0 / π`.
        FRAC_1_PI() -> T

        /// Returns `1.0 / sqrt(2.0)`.
        FRAC_1_SQRT_2() -> T

        /// Returns `2.0 / π`.
        FRAC_2_PI() -> T

        /// Returns `2.0 / sqrt(π)`.
        FRAC_2_SQRT_PI() -> T

        /// Returns `π / 2.0`.
        FRAC_PI_2() -> T

        /// Returns `π / 3.0`.
        FRAC_PI_3() -> T

        /// Returns `π / 4.0`.
        FRAC_PI_4() -> T

        /// Returns `π / 6.0`.
        FRAC_PI_6() -> T

        /// Returns `π / 8.0`.
        FRAC_PI_8() -> T

        /// Returns `ln(10.0)`.
        LN_10() -> T

        /// Returns `ln(2.0)`.
        LN_2() -> T

        /// Returns `log10(e)`.
        LOG10_E() -> T

        /// Returns `log2(e)`.
        LOG2_E() -> T

        /// Returns Archimedes’ constant `π`.
        PI() -> T

        /// Returns `sqrt(2.0)`.
        SQRT_2() -> T
    }

    /// Methods from the FloatCore trait, exposed as free functions.
    num_traits::float::FloatCore => float_core {
        /// Returns positive infinity.
        infinity() -> T

        /// Returns negative infinity.
        neg_infinity() -> T

        /// Returns `NaN`.
        nan() -> T

        /// Returns `-0.0`.
        neg_zero() -> T

        /// Returns the smallest finite value that this type can represent.
        min_value() -> T

        /// Returns the smallest positive, normalized value that this type can represent.
        min_positive_value() -> T

        /// Returns epsilon, a small positive value.
        epsilon() -> T

        /// Returns the largest finite value that this type can represent.
        max_value() -> T

        /// Returns the floating point category of the number.
        classify(x: T) -> FpCategory

        /// Converts to degrees, assuming the number is in radians.
        to_degrees(x: T) -> T

        /// Converts to radians, assuming the number is in degrees.
        to_radians(x: T) -> T

        /// Returns the mantissa, base 2 exponent, and sign as integers, respectively.
        integer_decode(x: T) -> (u64, i16, i8)

        /// Returns true if the number is `NaN`.
        is_nan(x: T) -> bool

        /// Returns true if the number is infinite.
        is_infinite(x: T) -> bool

        /// Returns true if the number is neither infinite or `NaN`.
        is_finite(x: T) -> bool

        /// Returns true if the number is neither zero, infinite, subnormal or `NaN`.
        is_normal(x: T) -> bool

        /// Returns the largest integer less than or equal to a number.
        floor(x: T) -> T

        /// Returns the smallest integer greater than or equal to a number.
        ceil(x: T) -> T

        /// Returns the nearest integer to a number. Round half-way cases away from `0.0`.
        round(x: T) -> T

        /// Returns the integer part of a number.
        trunc(x: T) -> T

        /// Returns the fractional part of a number.
        fract(x: T) -> T

        /// Computes the absolute value of a number.
        abs(x: T) -> T

        /// Returns a number that represents the sign of.
        signum(x: T) -> T

        /// Returns true if the input is positive.
        is_sign_positive(x: T) -> bool

        /// Returns true if the input is negative.
        is_sign_negative(x: T) -> bool

        /// Returns the minimum of the two numbers.
        min(x: T, other: T) -> T

        /// Returns the maximum of the two numbers.
        max(x: T, other: T) -> T

        /// Returns the reciprocal (multiplicative inverse) of the number.
        recip(x: T) -> T

        /// Raise a number to an integer power.
        powi(x: T, exp: i32) -> T
    }

    /// Methods from the PrimInt trait, exposed as free functions.
    num_traits::int::PrimInt => prim_int {
        /// Returns the number of ones in the binary representation of the input.
        count_ones(x: T) -> u32

        /// Returns the number of zeros in the binary representation of the input.
        count_zeros(x: T) -> u32

        /// Returns the number of leading zeros in the binary representation of the input.
        leading_zeros(x: T) -> u32

        /// Returns the number of trailing zeros in the binary representation of the input.
        trailing_zeros(x: T) -> u32

        /// Shifts the bits to the left by a specified amount amount, `n`,
        /// wrapping the truncated bits to the end of the resulting integer.
        rotate_left(x: T, n: u32) -> T

        /// Shifts the bits to the right by a specified amount amount, `n`,
        /// wrapping the truncated bits to the beginning of the resulting integer.
        rotate_right(x: T, n: u32) -> T

        /// Shifts the bits to the left by a specified amount amount, `n`,
        /// filling zeros in the least significant bits.
        signed_shl(x: T, n: u32) -> T

        /// Shifts the bits to the right by a specified amount amount, `n`,
        /// copying the "sign bit" in the most significant bits even for unsigned types.
        signed_shr(x: T, n: u32) -> T

        /// Shifts the bits to the left by a specified amount amount, `n`,
        /// filling zeros in the least significant bits.
        unsigned_shl(x: T, n: u32) -> T

        /// Shifts the bits to the right by a specified amount amount, `n`,
        /// filling zeros in the most significant bits.
        unsigned_shr(x: T, n: u32) -> T

        /// Reverses the byte order of the integer.
        swap_bytes(x: T) -> T

        /// Convert an integer from big endian to the target's endianness.
        from_be(x: T) -> T

        /// Convert an integer from little endian to the target's endianness.
        from_le(x: T) -> T

        /// Convert the input to big endian from the target's endianness.
        to_be(x: T) -> T

        /// Convert the input to little endian from the target's endianness.
        to_le(x: T) -> T

        /// Raises a number to the power of exp, using exponentiation by squaring.
        pow(x: T, exp: u32) -> T
    }

    /// Methods from the Num trait, exposed as free functions
    num_traits::Num => num {
        /// Convert from a string and radix <= 36.
        from_str_radix(s: &str, radix: u32) -> Result<T, T::FromStrRadixErr>
    }

    /// Methods from the CheckedNeg trait, exposed as free functions
    num_traits::ops::checked::CheckedNeg => checked_neg {
        /// Negates a number, returning None for results that can't be represented.
        checked_neg(x: &T) -> Option<T>
    }

    /// Methods from the Inv trait, exposed as free functions
    num_traits::ops::inv::Inv => inv {
        /// Returns the multiplicative inverse of a number.
        inv(x: T) -> T::Output
    }

    /// Methods from the WrappingNeg trait, exposed as free functions
    num_traits::ops::wrapping::WrappingNeg => wrapping_neg {
        /// Wrapping (modular) negation.
        wrapping_neg(&self) -> Self
    }

    /// Methods from the Real trait, exposed as free functions.
    #[cfg(any(feature = "std", feature = "libm"))]
    num_traits::real::Real => real {
        /// Returns the smallest finite value that this type can represent.
        min_value() -> T

        /// Returns the smallest positive, normalized value that this type can represent.
        min_positive_value() -> T

        /// Returns epsilon, a small positive value.
        epsilon() -> T

        /// Returns the largest finite value that this type can represent.
        max_value() -> T

        /// Returns the largest integer less than or equal to a number.
        floor(x: T) -> T

        /// Returns the smallest integer greater than or equal to a number.
        ceil(x: T) -> T

        /// Returns the nearest integer to a number.
        round(x: T) -> T

        /// Returns the integer part of a number.
        trunc(x: T) -> T

        /// Returns the fractional part of a number.
        fract(x: T) -> T

        /// Computes the absolute value of a number.
        abs(x: T) -> T

        /// Returns a number that represents the sign of the input.
        signum(x: T) -> T

        /// Returns true if the input is positive.
        is_sign_positive(x: T) -> bool

        /// Returns true if the input is negative.
        is_sign_negative(x: T) -> bool

        /// Fused multiply-add.
        mul_add(x: T, a: T, b: T) -> T

        /// Take the reciprocal (inverse) of a number, `1/x`.
        recip(x: T) -> T

        /// Raise a number to an integer power.
        powi(x: T, n: i32) -> T

        /// Raise a number to a real number power.
        powf(x: T, n: T) -> T

        /// Take the square root of a number.
        sqrt(x: T) -> T

        /// Returns `e^x`, (x: The exponential function).
        exp(x: T) -> T

        /// Returns `2^x`.
        exp2(x: T) -> T

        /// Returns the natural logarithm of the number.
        ln(x: T) -> T

        /// Returns the logarithm of the number with respect to an arbitrary base.
        log(x: T, base: T) -> T

        /// Returns the base 2 logarithm of the number.
        log2(x: T) -> T

        /// Returns the base 10 logarithm of the number.
        log10(x: T) -> T

        /// Converts radians to degrees.
        to_degrees(x: T) -> T

        /// Converts degrees to radians.
        to_radians(x: T) -> T

        /// Returns the maximum of two numbers.
        max(x: T, other: T) -> T

        /// Returns the minimum of two numbers.
        min(x: T, other: T) -> T

        /// Returns the positive difference of two numbers.
        abs_sub(x: T, other: T) -> T

        /// Takes the cubic root of a number.
        cbrt(x: T) -> T

        /// Computes the length of the hypotenuse of a right-angle triangle
        /// given its legs' lengths.
        hypot(x: T, other: T) -> T

        /// Computes the sine of a number (in radians).
        sin(x: T) -> T

        /// Computes the cosine of a number (in radians).
        cos(x: T) -> T

        /// Computes the tangent of a number (in radians).
        tan(x: T) -> T

        /// Computes the arcsine of a number.
        asin(x: T) -> T

        /// Computes the arccosine of a number.
        acos(x: T) -> T

        /// Computes the arctangent of a number.
        atan(x: T) -> T

        /// Computes the four quadrant arctangent of two numbers.
        atan2(x: T, other: T) -> T

        /// Simultaneously computes the sine and cosine of a number `x`.
        /// Returns `(sin(x), cos(x))`.
        sin_cos(x: T) -> (T, T)

        /// Returns `e^(x: T) - 1` in a way that is accurate even if
        /// the number is close to zero.
        exp_m1(x: T) -> T

        /// Returns `ln(1+n)` (natural logarithm) more accurately than
        /// if the operations were performed separately.
        ln_1p(x: T) -> T

        /// Hyperbolic sine function.
        sinh(x: T) -> T

        /// Hyperbolic cosine function.
        cosh(x: T) -> T

        /// Hyperbolic tangent function.
        tanh(x: T) -> T

        /// Inverse hyperbolic sine function.
        asinh(x: T) -> T

        /// Inverse hyperbolic cosine function.
        acosh(x: T) -> T

        /// Inverse hyperbolic tangent function.
        atanh(x: T) -> T
    }
}
