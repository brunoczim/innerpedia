//! This module exports components more related to text.

use crate::{
    component::{BlockComponent, Component, Context, InlineComponent},
    location::Location,
};
use std::fmt;

/// Bold text. The parameter is wrapped to make its text bold.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Bold<T, K>(pub T)
where
    T: Component<Kind = K>;

impl<T> Component for Bold<T, InlineComponent>
where
    T: Component<Kind = InlineComponent>,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<b class=\"bold\">{}</b>", ctx.renderer(&self.0))?;
        Ok(())
    }
}

impl<T> Component for Bold<T, BlockComponent>
where
    T: Component<Kind = BlockComponent>,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<div class=\"bold\">{}</div>", ctx.renderer(&self.0))?;
        Ok(())
    }
}

/// Italic text. The parameter is wrapped to make its text italic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Italic<T, K>(pub T)
where
    T: Component<Kind = K>;

impl<T> Component for Italic<T, InlineComponent>
where
    T: Component<Kind = InlineComponent>,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<i class=\"italic\">{}</i>", ctx.renderer(&self.0))?;
        Ok(())
    }
}

impl<T> Component for Italic<T, BlockComponent>
where
    T: Component<Kind = BlockComponent>,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<div class=\"italic\">{}</div>", ctx.renderer(&self.0))?;
        Ok(())
    }
}

/// Preformatted text. The parameter is wrapped to make its text monospaced
/// and/or treated like code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Preformatted<T, K>(pub T)
where
    T: Component<Kind = K>;

impl<T> Component for Preformatted<T, InlineComponent>
where
    T: Component<Kind = InlineComponent>,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<pre class=\"pre\">{}</pre>", ctx.renderer(&self.0))?;
        Ok(())
    }
}

impl<T> Component for Preformatted<T, BlockComponent>
where
    T: Component<Kind = BlockComponent>,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<div class=\"pre\">{}</div>", ctx.renderer(&self.0))?;
        Ok(())
    }
}

/// Wraps the given component into a paragraph.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Paragraph<T>(pub T)
where
    T: Component<Kind = InlineComponent>;

impl<T> Component for Paragraph<T>
where
    T: Component<Kind = InlineComponent>,
{
    type Kind = BlockComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(fmt, "<p class=\"paragraph\">{}</p>", ctx.renderer(&self.0))?;
        Ok(())
    }
}

/// A link to some location.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Link<T>
where
    T: Component<Kind = InlineComponent>,
{
    /// The text displayed for the link.
    pub text: T,
    /// The location to which this link points to.
    pub location: Location,
}

impl<T> Component for Link<T>
where
    T: Component<Kind = InlineComponent>,
{
    type Kind = InlineComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(
            fmt,
            "<a href=\"{}\" class=\"link\">{}</a>",
            ctx.renderer(&self.location),
            ctx.renderer(&self.text),
        )?;
        Ok(())
    }
}
