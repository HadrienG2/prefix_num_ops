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
                $method_name:ident $method_args:tt -> $method_result:ident
            )*
        } )*
    ) => (
        $(
            $(#[$module_attrs])*
            pub mod $module_name {
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
// FIXME: Support all method signatures, not just self->Self.
macro_rules! define_method {
    ($trait_path:path : $method_name:ident(self) -> Self) => {
        pub fn $method_name<T: $trait_path>(self_: T) -> T {
            self_.$method_name()
        }
    }
}
//
define_prefix_methods! {
    /// Methods from the Real trait, as free functions.
    num_traits::real::Real => real {
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

        /// Take the reciprocal (inverse) of a number, `1/x`.
        recip(self) -> Self

        /// Take the square root of a number.
        sqrt(self) -> Self

        /// Returns `e^x`, (the exponential function).
        exp(self) -> Self

        /// Returns `2^x`.
        exp2(self) -> Self

        /// Returns the natural logarithm of the number.
        ln(self) -> Self

        /// Returns the base 2 logarithm of the number.
        log2(self) -> Self

        /// Returns the base 10 logarithm of the number.
        log10(self) -> Self

        /// Converts radians to degrees.
        to_degrees(self) -> Self

        /// Converts degrees to radians.
        to_radians(self) -> Self

        /// Take the cubic root of a number.
        cbrt(self) -> Self

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

        /// Returns e^(self) - 1 in a way that is accurate even if the number is close to zero.
        exp_m1(self) -> Self

        /// Returns ln(1+n) (natural logarithm) more accurately than if the operations were performed separately.
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

    // TODO: Do the same thing for other traits
}
