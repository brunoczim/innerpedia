//! This module exports list components.

use crate::component::{BlockComponent, Component, Context};
use std::fmt;

/// A list of elements, with enumeration on each item.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrderedList<T>(pub Vec<T>)
where
    T: Component;

impl<T> Default for OrderedList<T>
where
    T: Component,
{
    fn default() -> Self {
        Self(Vec::default())
    }
}

impl<T> Component for OrderedList<T>
where
    T: Component,
{
    type Kind = BlockComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<ol class=\"ordered-list\">")?;
        for item in &self.0 {
            write!(fmt, "<li>{}</li>", ctx.renderer(item))?;
        }
        write!(fmt, "</ol>")?;

        Ok(())
    }
}

/// A list of elements, with no enumeration, but still with a bullet before each
/// item.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnorderedList<T>(pub Vec<T>)
where
    T: Component;

impl<T> Default for UnorderedList<T>
where
    T: Component,
{
    fn default() -> Self {
        Self(Vec::default())
    }
}

impl<T> Component for UnorderedList<T>
where
    T: Component,
{
    type Kind = BlockComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<ul class=\"unordered-list\">")?;
        for item in &self.0 {
            write!(fmt, "<li>{}</li>", ctx.renderer(item))?;
        }
        write!(fmt, "</ul>")?;

        Ok(())
    }
}

/// A list of elements, with no enumeration and no bullet on the items.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnmarkedList<T>(pub Vec<T>)
where
    T: Component;

impl<T> Default for UnmarkedList<T>
where
    T: Component,
{
    fn default() -> Self {
        Self(Vec::default())
    }
}

impl<T> Component for UnmarkedList<T>
where
    T: Component,
{
    type Kind = BlockComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<ul class=\"unmarked-list\">")?;
        for item in &self.0 {
            write!(fmt, "<li>{}</li>", ctx.renderer(item))?;
        }
        write!(fmt, "</ul>")?;

        Ok(())
    }
}
