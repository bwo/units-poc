//use crate::traits::private::*;
use std::{cmp::Ordering, marker::PhantomData};

pub struct Nil;

pub struct Cons<H: HasName, T> {
    _h: PhantomData<H>,
    _t: PhantomData<T>,
}

pub(crate) mod sealed {

    pub trait List {}
    pub trait Maybe {}

    pub trait Remove<L: List> {
        type Output: Maybe;
    }

    pub trait RemoveH<L: List, const CMP: u8> {
        type Output: Maybe;
    }

    pub trait RemoveR<H: super::HasName, Removed: Maybe> {
        type Output: Maybe;
    }

    pub trait Insort<L: List> {
        type Output: List;
    }

    pub trait InsortH<L: List, const CMP: u8> {
        type Output: List;
    }

    pub trait Merge<L: List> {
        type Output: List;
    }

    pub trait MergeH<L: List, const CMP: u8> {
        type Output: List;
    }
}

pub trait HasName {
    const NAME: &'static [u8];
}

pub trait TypeComparison<O: HasName>: HasName {
    const OUT: u8;
}

pub const fn compare_type<A: HasName, B: HasName>() -> u8 {
    compare_u8a(<A as HasName>::NAME, &<B as HasName>::NAME) as u8
}

pub const fn compare_u8a(a: &'static [u8], b: &'static [u8]) -> Ordering {
    if a.len() < b.len() {
        Ordering::Less
    } else if a.len() > b.len() {
        Ordering::Greater
    } else {
        let mut i = 0;
        while i < a.len() {
            if a[i] < b[i] {
                return Ordering::Less;
            } else if a[i] > b[i] {
                return Ordering::Greater;
            };
            i += 1;
        }
        Ordering::Equal
    }
}

impl sealed::List for Nil {}
impl<H: HasName, T: sealed::List> sealed::List for Cons<H, T> {}

pub struct Nothing;
impl sealed::Maybe for Nothing {}
pub struct Just<T> {
    _t: PhantomData<T>,
}
impl<T> sealed::Maybe for Just<T> {}

impl<U: HasName, V: HasName> TypeComparison<U> for V {
    const OUT: u8 = { compare_u8a(<V as HasName>::NAME, <U as HasName>::NAME) as u8 };
}

impl<T> sealed::Remove<Nil> for T {
    type Output = Nothing;
}

impl<
        Needle: HasName + sealed::RemoveH<Cons<H, T>, { compare_type::<Needle, H>() }>,
        H: HasName,
        T: sealed::List,
    > sealed::Remove<Cons<H, T>> for Needle
{
    type Output = <Needle as sealed::RemoveH<Cons<H, T>, { compare_type::<Needle, H>() }>>::Output;
}

impl<Needle: HasName + sealed::Remove<T>, H: HasName, T: sealed::List>
    sealed::RemoveH<Cons<H, T>, { Ordering::Equal as u8 }> for Needle
{
    type Output = Just<T>;
}

impl<Needle: HasName + sealed::Remove<T>, H: HasName, T: sealed::List>
    sealed::RemoveH<Cons<H, T>, { Ordering::Greater as u8 }> for Needle
{
    type Output = Nothing;
}

impl<
        Needle: HasName + sealed::RemoveR<H, Removed<Needle, T>> + sealed::Remove<T>,
        H: HasName,
        T: sealed::List,
    > sealed::RemoveH<Cons<H, T>, { Ordering::Less as u8 }> for Needle
{
    type Output = <Needle as sealed::RemoveR<H, Removed<Needle, T>>>::Output;
}

impl<Needle: HasName, H: HasName> sealed::RemoveR<H, Nothing> for Needle {
    type Output = Nothing;
}

impl<Needle: HasName, H: HasName, T: sealed::List> sealed::RemoveR<H, Just<T>> for Needle {
    type Output = Just<Cons<H, T>>;
}

impl<H: HasName> sealed::Insort<Nil> for H {
    type Output = Cons<H, Nil>;
}

impl<
        H: HasName,
        T: sealed::List,
        N: HasName + sealed::InsortH<Cons<H, T>, { compare_type::<N, H>() }>,
    > sealed::Insort<Cons<H, T>> for N
{
    type Output = <N as sealed::InsortH<Cons<H, T>, { compare_type::<N, H>() }>>::Output;
}

impl<H: HasName, T: sealed::List, N: HasName> sealed::InsortH<Cons<H, T>, { Ordering::Equal as u8 }>
    for N
{
    type Output = Cons<N, Cons<H, T>>;
}

impl<H: HasName, T: sealed::List, N: HasName>
    sealed::InsortH<Cons<H, T>, { Ordering::Greater as u8 }> for N
{
    type Output = Cons<N, Cons<H, T>>;
}

impl<H: HasName, T: sealed::List, N: HasName + sealed::Insort<T>>
    sealed::InsortH<Cons<H, T>, { Ordering::Less as u8 }> for N
{
    type Output = Cons<H, <N as sealed::Insort<T>>::Output>;
}

impl sealed::Merge<Nil> for Nil {
    type Output = Nil;
}

impl<H: HasName, T: sealed::List> sealed::Merge<Nil> for Cons<H, T> {
    type Output = Cons<H, T>;
}

impl<H: HasName, T: sealed::List> sealed::Merge<Cons<H, T>> for Nil {
    type Output = Cons<H, T>;
}

impl<H1: HasName + TypeComparison<H1>, T1: sealed::List, H2: HasName, T2: sealed::List>
    sealed::Merge<Cons<H1, T1>> for Cons<H2, T2>
where
    Cons<H2, T2>: sealed::MergeH<Cons<H1, T1>, { compare_type::<H2, H1>() }>,
{
    type Output =
        <Cons<H2, T2> as sealed::MergeH<Cons<H1, T1>, { compare_type::<H2, H1>() }>>::Output;
}

impl<H1: HasName, T1: sealed::List + sealed::Merge<T2>, H2: HasName, T2: sealed::List>
    sealed::MergeH<Cons<H1, T1>, { Ordering::Equal as u8 }> for Cons<H2, T2>
{
    type Output = Cons<H1, Cons<H2, Merged<T1, T2>>>;
}

impl<H1: HasName, T1: sealed::List, H2: HasName, T2: sealed::List>
    sealed::MergeH<Cons<H1, T1>, { Ordering::Greater as u8 }> for Cons<H2, T2>
where
    Cons<H1, T1>: sealed::Merge<T2>,
{
    type Output = Cons<H2, Merged<Cons<H1, T1>, T2>>;
}

impl<H1: HasName, T1: sealed::List, H2: HasName, T2: sealed::List>
    sealed::MergeH<Cons<H1, T1>, { Ordering::Less as u8 }> for Cons<H2, T2>
where
    Cons<H2, T2>: sealed::Merge<T1>,
{
    type Output = Cons<H1, Merged<Cons<H2, T2>, T1>>;
}

pub(crate) type Merged<L1, L2> = <L1 as sealed::Merge<L2>>::Output;
pub type Inserted<N, L> = <N as sealed::Insort<L>>::Output;
pub(crate) type Removed<Needle, List> = <Needle as sealed::Remove<List>>::Output;
pub(crate) type Singleton<T> = Cons<T, Nil>;
