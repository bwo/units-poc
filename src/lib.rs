#![feature(generic_const_exprs)]

pub use list::{Cons, Inserted, Nil};
pub use units::{BaseUnit, Units, Value};

pub mod list;
pub mod units;

#[macro_export]
macro_rules! defunit {
    ($name:ident) => {
        #[derive(Copy, Clone, Eq, Ord, PartialOrd, PartialEq, Hash)]
        pub struct $name;

        impl $crate::units::BaseUnit for $name {
            const NAME: &'static [u8] = stringify!($name).as_bytes();
        }

        impl $name {
            pub const fn new<T>(t: T) -> vtype!(T ; $name / ) {
                $crate::Value::new(t)
            }
        }
    };
}

#[macro_export]
macro_rules! tlist {
    () => { $crate::list::Nil };
    ($name:ident) => { $crate::list::Cons<$name, $crate::list::Nil> };
    ($name:ident, $($ns:ident),+) => {
	$crate::list::Inserted<$name, tlist!($($ns),+)>
    }
}

#[macro_export]
macro_rules! units {
    () => { $crate::Units<$crate::list::Nil, $crate::list::Nil> };
    ($name:ident / ) => { $crate::Units<tlist!($name), $crate::list::Nil> } ;
    (/ $name:ident ) => { $crate::Units<$crate::list::Nil, tlist!($name)> } ;
    ($name:ident / $($tail:tt)*) => { $crate::Units<tlist!($name), tlist!($($tail)*)> } ;
    ($name:ident, $($ns:ident),+ /   $($tail:tt)* ) => { $crate::Units<tlist!($name, $($ns),+), tlist!($($tail)*)> } ;
}

#[macro_export]
macro_rules! vtype {
    ($t:ty; $($units:tt)*) => { Value<$t, units!($($units)*)> }
}

/// ```compile_faile
///
/// #[macro_use] extern crate units_poc;
/// use units_poc::{defunit, Value};
/// defunit!(A);
/// defunit!(B);
/// let _ = A::new(1.0) + B::new(1.0);
/// ```
///
/// ```compile_fail
///
/// #[macro_use] extern crate units_poc;
/// use units_poc::{defunit, Value};
/// defunit!(A);
/// defunit!(B);
/// let _ = A::new(1.0) + (B::new(1.0) * A::new(2.0));
/// ```

#[cfg(doctest)]
pub struct T;

/// ```compile_fail
///
/// #[macro_use] extern crate units_poc;
/// use units_poc::{defunit, Value};
/// defunit!(Meter);
/// defunit!(Second);
/// let a = Meter::new(1.0);
/// let b = Second::new(2.0);
/// let _ = (a / (b * b)) + (a / b);
/// ```
#[cfg(doctest)]
pub struct Yoric;



#[cfg(test)]
mod tests {

    use super::Value;
    defunit!(A);
    defunit!(B);
    defunit!(C);
    defunit!(D);
    defunit!(E);

    #[test]
    fn test_basics() {
        let a = A::new(1.0);
        let b = B::new(1.0);
        let c = C::new(1.0);
        let d = D::new(1.0);
        let e = E::new(1.0);
        let _: vtype!(f64; A /) = a * 5.0;
        let _: vtype!(f64; A /) = 5.0 * a;
        let _: vtype!(f64; C /) = c + (d * (c / d));
        let y = c * d;
        let _: vtype!(f64; C, D / A, B) = y / (a * b);
        let _: vtype!(f64; / A) = 2.0 / a;
        let _: vtype!(f64; A / ) = 2.0 / (2.0 / a);
        let _: vtype!(f64; B / A) = b * 2.0 / a;
        let x: vtype!(f64; D, C, B / E, E, A) = (b * 2.0 * c * d) / (e * a * e);
        let _: vtype!(f64; E, E, A / D, B) = (2.0 * c) / x;
    }

    #[test]
    fn test_yoric() {
	defunit!(Meter);
	defunit!(Second);

	let a = Meter::new(1.0);
	let b = Second::new(2.0);
	let _ : vtype!(f64; Meter / Second, Second) = (a / (b * b)) - ((a / b) / b);
    }
}
