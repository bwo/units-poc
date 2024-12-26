#![feature(generic_const_exprs)]

pub use units::{BaseUnit, Unit, Units, Value};

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
    (/ $name:ident ) => { $crate::Units<Nil, tlist!($name)> } ;
    ($name:ident / $($tail:tt)*) => { $crate::Units<tlist!($name), tlist!($($tail)*)> } ;
    ($name:ident, $($ns:ident),+ /   $($tail:tt)* ) => { $crate::Units<tlist!($name, $($ns),+), tlist!($($tail)*)> } ;
}

#[macro_export]
macro_rules! vtype {
    ($t:ty; $($units:tt)*) => { Value<$t, units!($($units)*)> }
}

#[cfg(test)]
mod tests {

    use super::Value;
    defunit!(A);
    defunit!(B);
    defunit!(C);
    defunit!(D);

    #[test]
    fn test_basics() {
        let a = A::new(1.0);
        let b = B::new(1.0);
        let c = C::new(1.0);
        let d = D::new(1.0);
        let _ = a * 5.0;
        let _ = 5.0 * a;
        let y = c * d;
        let _: vtype!(f64; C, D / A, B) = y / (a * b);
    }
}
