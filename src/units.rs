use std::cmp::Ordering;
use std::marker::PhantomData;
use std::ops::{Add, Div, Mul, Sub};

use crate::list::{compare_type, sealed as listtraits, Cons, HasName, Merged, Nil, TypeComparison};

mod sealed {
    use crate::list::sealed::*;
    pub trait DivideList<Denom: List> {
        type N: List;
        type D: List;
    }

    pub trait DivideListH<Denom: List, const CMP: u8> {
        type N: List;
        type D: List;
    }
    pub trait Unit {}
}

impl sealed::DivideList<Nil> for Nil {
    type N = Nil;
    type D = Nil;
}

impl<H: HasName, T: listtraits::List> sealed::DivideList<Nil> for Cons<H, T> {
    type N = Cons<H, T>;
    type D = Nil;
}

impl<H: HasName, T: listtraits::List> sealed::DivideList<Cons<H, T>> for Nil {
    type N = Nil;
    type D = Cons<H, T>;
}

impl<H1: HasName, H2: HasName + TypeComparison<H1>, T1: listtraits::List, T2: listtraits::List>
    sealed::DivideList<Cons<H1, T1>> for Cons<H2, T2>
where
    Cons<H2, T2>: sealed::DivideListH<Cons<H1, T1>, { compare_type::<H2, H1>() }>,
{
    type N = <Cons<H2, T2> as sealed::DivideListH<Cons<H1, T1>, { compare_type::<H2, H1>() }>>::N;
    type D = <Cons<H2, T2> as sealed::DivideListH<Cons<H1, T1>, { compare_type::<H2, H1>() }>>::D;
}

impl<
        H1: HasName,
        H2: HasName + TypeComparison<H1>,
        T1: listtraits::List,
        T2: listtraits::List + sealed::DivideList<T1>,
    > sealed::DivideListH<Cons<H1, T1>, { Ordering::Equal as u8 }> for Cons<H2, T2>
{
    type N = Numerator<T2, T1>;
    type D = Denominator<T2, T1>;
}

impl<H1: HasName, H2: HasName + TypeComparison<H1>, T1: listtraits::List, T2: listtraits::List>
    sealed::DivideListH<Cons<H1, T1>, { Ordering::Less as u8 }> for Cons<H2, T2>
where
    Cons<H2, T2>: sealed::DivideList<T1>,
{
    type N = Numerator<Cons<H2, T2>, T1>;
    type D = Cons<H1, Denominator<Cons<H2, T2>, T1>>;
}

impl<H1: HasName, H2: HasName + TypeComparison<H1>, T1: listtraits::List, T2: listtraits::List>
    sealed::DivideListH<Cons<H1, T1>, { Ordering::Greater as u8 }> for Cons<H2, T2>
where
    T2: sealed::DivideList<Cons<H1, T1>>,
{
    type N = Cons<H2, Numerator<T2, Cons<H1, T1>>>;
    type D = Denominator<T2, Cons<H1, T1>>;
}

pub trait BaseUnit {
    const NAME: &'static [u8];
}

impl<U: BaseUnit> HasName for U {
    const NAME: &'static [u8] = <U as BaseUnit>::NAME;
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct Units<M: listtraits::List, D: listtraits::List> {
    _multipliers: PhantomData<M>,
    _divisors: PhantomData<D>,
}

impl<M: listtraits::List, D: listtraits::List> Units<M, D> {
    pub const fn new<T>(t: T) -> Value<T, Self> {
        Value::new(t)
    }
}

impl<M: listtraits::List, D: listtraits::List> Clone for Units<M, D> {
    fn clone(&self) -> Self {
        Units {
            _divisors: PhantomData,
            _multipliers: PhantomData,
        }
    }
}
impl<M: listtraits::List, D: listtraits::List> Copy for Units<M, D> {}

impl<M: listtraits::List, D: listtraits::List> sealed::Unit for Units<M, D> {}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Value<T, U: sealed::Unit> {
    t: T,
    _u: PhantomData<U>,
}

impl<T, U: sealed::Unit> Value<T, U> {
    pub const fn new(t: T) -> Value<T, U> {
        Value { t, _u: PhantomData }
    }
}

impl<
        T: Mul<Output = T> + Copy,
        M1: listtraits::List,
        D1: listtraits::List,
        M2: listtraits::List,
        D2: listtraits::List,
    > Mul<Value<T, Units<M1, D1>>> for Value<T, Units<M2, D2>>
where
    D1: listtraits::Merge<D2>,
    M1: listtraits::Merge<M2>,
    Merged<M1, M2>: sealed::DivideList<Merged<D1, D2>>,
{
    type Output = Value<
        T,
        Units<
            Numerator<Merged<M1, M2>, Merged<D1, D2>>,
            Denominator<Merged<M1, M2>, Merged<D1, D2>>,
        >,
    >;
    fn mul(self, rhs: Value<T, Units<M1, D1>>) -> Self::Output {
        Value::new(self.t * rhs.t)
    }
}

impl<
        T: Div<Output = T> + Copy,
        M1: listtraits::List,
        D1: listtraits::List,
        M2: listtraits::List,
        D2: listtraits::List,
    > Div<Value<T, Units<M1, D1>>> for Value<T, Units<M2, D2>>
where
    M2: listtraits::Merge<D1>,
    D2: listtraits::Merge<M1>,
    Merged<M2, D1>: sealed::DivideList<Merged<D2, M1>>,
{
    type Output = Value<
        T,
        Units<
            Numerator<Merged<M2, D1>, Merged<D2, M1>>,
            Denominator<Merged<M2, D1>, Merged<D2, M1>>,
        >,
    >;
    fn div(self, rhs: Value<T, Units<M1, D1>>) -> Self::Output {
        Value::new(self.t / rhs.t)
    }
}

impl<T: Mul<Output = T>, U: sealed::Unit> Mul<T> for Value<T, U> {
    type Output = Value<T, U>;
    fn mul(self, rhs: T) -> Self::Output {
        Value::new(self.t * rhs)
    }
}

impl<T: Div<Output = T>, U: sealed::Unit> Div<T> for Value<T, U> {
    type Output = Value<T, U>;
    fn div(self, rhs: T) -> Self::Output {
        Value::new(self.t / rhs)
    }
}

macro_rules! scalar_div_on_lhs {
    ($t:ty) => {
        impl<M: $crate::list::sealed::List, D: $crate::list::sealed::List, T, S>
            Div<Value<T, Units<M, D>>> for $t
        where
            $t: Div<T, Output = S>,
        {
            type Output = Value<S, Units<D, M>>;
            fn div(self, rhs: Value<T, Units<M, D>>) -> Self::Output {
                Value::new(self / rhs.t)
            }
        }
    };
}

macro_rules! scalar_mul_on_lhs {
    ($t:ty) => {
        impl<U: sealed::Unit, T, S> Mul<Value<T, U>> for $t
        where
            $t: Mul<T, Output = S>,
        {
            type Output = Value<S, U>;
            fn mul(self, rhs: Value<T, U>) -> Self::Output {
                Value::new(self * rhs.t)
            }
        }
    };
}

macro_rules! scalar_muls_on_lhs {
    ($t:ty) => {scalar_mul_on_lhs!($t);};
    ($t:ty, $($ts:ty),*) => {
	scalar_mul_on_lhs!($t);
	scalar_muls_on_lhs!($($ts),*);
    }
}

macro_rules! scalar_divs_on_lhs {
    ($t:ty) => {scalar_div_on_lhs!($t);};
    ($t:ty, $($ts:ty),*) => {
	scalar_div_on_lhs!($t);
	scalar_divs_on_lhs!($($ts),*);
    }
}

scalar_muls_on_lhs!(f32, f64, i8, u8, i16, u16, usize, i32, u32, i64, u64, i128, u128);
scalar_divs_on_lhs!(f32, f64, i8, u8, i16, u16, usize, i32, u32, i64, u64, i128, u128);

impl<T: Add<Output = T>, U: sealed::Unit> Add<Value<T, U>> for Value<T, U> {
    type Output = Value<T, U>;
    fn add(self, rhs: Value<T, U>) -> Self::Output {
        Value::new(self.t + rhs.t)
    }
}

impl<T: Sub<Output = T>, U: sealed::Unit> Sub<Value<T, U>> for Value<T, U> {
    type Output = Value<T, U>;
    fn sub(self, rhs: Value<T, U>) -> Self::Output {
        Value::new(self.t - rhs.t)
    }
}

pub(crate) type Numerator<Num, Denom> = <Num as sealed::DivideList<Denom>>::N;
pub(crate) type Denominator<Num, Denom> = <Num as sealed::DivideList<Denom>>::D;
