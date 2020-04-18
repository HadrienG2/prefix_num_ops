//! The num_traits API, reimplemented using free functions.
//!
//! So, you're trying to do scientific computing in Rust, and you can't get
//! used to mathematical functions like sin() or cos() being postfix methods?
//! This crate may be for you!
//!
//! It re-export all useful free functions from `num_traits` in a single place
//! and provides free function versions of the trait methods of the num traits,
//! so that you can easily do things like `sin(x) + 3*ln(y)`.
//!
//! Each trait's methods are exposed as an identically names module of free
//! functions in this crate, and it only takes one use clause to go from there
//! to using the above syntax in your math expressions.
//!
//! Only a one-line summary of each method's documentations is provided, please
//! refer to the corresponding trait method's documentation for details.

use core::num::FpCategory;

// Re-export useful prefix functions from num_traits
pub use num_traits::{
    cast::cast,
    clamp, clamp_max, clamp_min,
    identities::{one, zero},
    pow::{checked_pow, pow},
    sign::{abs, abs_sub, signum},
};

/// Expose methods from num's traits as a module of free functions
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
                    $(#[$method_attrs])*
                    #[inline]
                    define_method! {
                        $trait_path : $method_name $method_args -> $method_result
                    }
                )*
            }
        )*
    )
}
//
// TODO: Try to nerd-snipe a macro expert like dtolnay into generalizing this,
//       or proving that it cannot be done within macro_rules' limitations.
macro_rules! define_method {
    ($trait_path:path : $method_name:ident() -> Self) => {
        pub fn $method_name<T: $trait_path>() -> T {
            <T as $trait_path>::$method_name()
        }
    };

    ($trait_path:path : $method_name:ident(self) -> Self) => {
        pub fn $method_name<T: $trait_path>(self_: T) -> T {
            self_.$method_name()
        }
    };

    ($trait_path:path : $method_name:ident(self) -> (Self, Self)) => {
        pub fn $method_name<T: $trait_path>(self_: T) -> (T, T) {
            self_.$method_name()
        }
    };

    ($trait_path:path : $method_name:ident(self) -> $return_type:ty) => {
        pub fn $method_name<T: $trait_path>(self_: T) -> $return_type {
            self_.$method_name()
        }
    };

    ($trait_path:path : $method_name:ident(self, $($self_arg:ident: Self),*) -> Self) => {
        pub fn $method_name<T: $trait_path>(self_: T, $($self_arg: T),*) -> T {
            self_.$method_name($($self_arg),*)
        }
    };

    ($trait_path:path : $method_name:ident(self, $($arg:ident: $arg_ty:ty),*) -> Self) => {
        pub fn $method_name<T: $trait_path>(self_: T, $($arg: $arg_ty),*) -> T {
            self_.$method_name($($arg),*)
        }
    };
}
//
define_prefix_methods! {
    /// Methods from the Real trait, exposed as free functions.
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

        /// Return the integer part of a number.
        trunc(self) -> Self

        /// Returns the fractional part of a number.
        fract(self) -> Self

        /// Computes the absolute value of self.
        abs(self) -> Self

        /// Returns a number that represents the sign of self.
        signum(self) -> Self

        /// Returns true if self is positive.
        is_sign_positive(self) -> bool

        /// Returns true if self is negative.
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

        /// Returns the maximum of the two numbers.
        max(self, other: Self) -> Self

        /// Returns the minimum of the two numbers.
        min(self, other: Self) -> Self

        /// The positive difference of two numbers.
        abs_sub(self, other: Self) -> Self

        /// Take the cubic root of a number.
        cbrt(self) -> Self

        /// Calculate the length of the hypotenuse of a right-angle triangle given leg lengths.
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

        /// Computes the four quadrant arctangent of self (y) and other (x).
        atan2(self, other: Self) -> Self

        /// Simultaneously computes the sine and cosine of a number.
        sin_cos(self) -> (Self, Self)

        /// Returns `e^(self) - 1` in a way that is accurate even if the number is close to zero.
        exp_m1(self) -> Self

        /// Returns `ln(1+n)` (natural logarithm) more accurately than if the operations were performed separately.
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

    /// Methods from the FloatCore trait, exposed as free functions.
    num_traits::float::FloatCore => float_core {
        /// Returns positive infinity.
        infinity() -> Self

        /// Returns negative infinity.
        neg_infinity() -> Self

        /// Returns NaN.
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

        /// Returns true if the number is NaN.
        is_nan(self) -> bool

        /// Returns true if the number is infinite.
        is_infinite(self) -> bool

        /// Returns true if the number is neither infinite or NaN.
        is_finite(self) -> bool

        /// Returns true if the number is neither zero, infinite, subnormal or NaN.
        is_normal(self) -> bool

        /// Returns the largest integer less than or equal to a number.
        floor(self) -> Self

        /// Returns the smallest integer greater than or equal to a number.
        ceil(self) -> Self

        /// Returns the nearest integer to a number. Round half-way cases away from 0.0.
        round(self) -> Self

        /// Return the integer part of a number.
        trunc(self) -> Self

        /// Returns the fractional part of a number.
        fract(self) -> Self

        /// Computes the absolute value of self.
        abs(self) -> Self

        /// Returns a number that represents the sign of self.
        signum(self) -> Self

        /// Returns true if self is positive.
        is_sign_positive(self) -> bool

        /// Returns true if self is negative.
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

    // TODO: Do the same thing for other traits
}
