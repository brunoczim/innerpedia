//! This module specify the inner components of a page.

pub mod text;
pub mod img;
pub mod table;
pub mod list;
pub mod audio;

use crate::{location::InternalPath, site::Site};
use std::{
    borrow::Cow,
    fmt::{self, Write},
    rc::Rc,
    sync::Arc,
};

fn html_escape(ch: char) -> Option<&'static str> {
    match ch {
        '&' => Some("&amp;"),
        '<' => Some("&lt;"),
        '>' => Some("&gt;"),
        '"' => Some("&quot;"),
        '\'' => Some("&#39;"),
        '\\' => Some("&#92;"),
        _ => None,
    }
}

/// The type to tag a block component (via `Component<Kind = BlockComponent>`).
/// Block components cannot be inserted into the middle of the text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockComponent;

/// The type to tag an inline component (via `Component<Kind =
/// InlineComponent>`). Inline components can be inserted into the middle of the
/// text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InlineComponent;

/// The context of a component rendering. Cannot be created manually. Used to
/// identify things such as the current page's location.
#[derive(Debug, Clone, Copy)]
pub struct Context<'loc, 'site> {
    location: &'loc InternalPath,
    site: &'site Site,
}

impl<'loc, 'site> Context<'loc, 'site> {
    /// Creates the context from the current page's location and the given site.
    pub(crate) fn new(location: &'loc InternalPath, site: &'site Site) -> Self {
        Self { location, site }
    }

    /// The location of the current page.
    pub fn location(self) -> &'loc InternalPath {
        self.location
    }

    /// Creates a renderer over a component from this context. The `Display`
    /// trait can be used on the renderer.
    pub fn renderer<T>(self, component: T) -> Renderer<'loc, 'site, T>
    where
        T: Component,
    {
        Renderer { component, context: self }
    }
}

/// A renderer over a component. The `Display` trait can be used on the
/// renderer.
#[derive(Debug, Clone, Copy)]
pub struct Renderer<'loc, 'site, T>
where
    T: Component,
{
    /// The component being rendered.
    pub component: T,
    /// The context at which the component will be rendered.
    pub context: Context<'loc, 'site>,
}

impl<'loc, 'site, T> fmt::Display for Renderer<'loc, 'site, T>
where
    T: Component,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.component.to_html(fmt, self.context)
    }
}

/// A helper type to specify a dynamic dispatch component.
pub type DynComponent<Kind = BlockComponent> =
    Arc<dyn Component<Kind = Kind> + Send + Sync>;

/// The component trait. It specifies a rendering method for a component, as
/// well some helper methods.
pub trait Component: fmt::Debug {
    /// The kind of the component. Either [`BlockComponent`] or
    /// [`InlineComponent`].
    type Kind;

    /// Renders this component given the context and the output formatter.
    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result;

    /// Ensures this component can be used as a blocking component.
    fn blocking(self) -> Blocking<Self>
    where
        Self: Sized,
    {
        Blocking(self)
    }

    /// Transforms this component into a dynamic dispatch component.
    fn to_dyn(self) -> DynComponent<Self::Kind>
    where
        Self: Sized + Send + Sync + 'static,
    {
        Arc::new(self)
    }
}

impl<'this, T> Component for &'this T
where
    T: Component + ?Sized,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        (**self).to_html(fmt, ctx)
    }
}

impl<T> Component for Box<T>
where
    T: Component + ?Sized,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        (**self).to_html(fmt, ctx)
    }
}

impl<T> Component for Rc<T>
where
    T: Component + ?Sized,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        (**self).to_html(fmt, ctx)
    }
}

impl<T> Component for Arc<T>
where
    T: Component + ?Sized,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        (**self).to_html(fmt, ctx)
    }
}

impl<'cow, T> Component for Cow<'cow, T>
where
    T: Component + ToOwned + ?Sized,
    T::Owned: fmt::Debug,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        (**self).to_html(fmt, ctx)
    }
}

impl<T> Component for Vec<T>
where
    T: Component,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        for elem in self {
            elem.to_html(fmt, ctx)?;
        }
        Ok(())
    }
}

impl<T> Component for Option<T>
where
    T: Component,
{
    type Kind = T::Kind;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        if let Some(component) = self {
            component.to_html(fmt, ctx)?;
        }
        Ok(())
    }
}

impl Component for str {
    type Kind = InlineComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, _ctx: Context) -> fmt::Result {
        FmtAdapter { buf: fmt }.write_str(self)
    }
}

impl Component for String {
    type Kind = InlineComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        (**self).to_html(fmt, ctx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct FmtAdapter<W>
where
    W: fmt::Write,
{
    buf: W,
}

impl<W> fmt::Write for FmtAdapter<W>
where
    W: fmt::Write,
{
    fn write_str(&mut self, string: &str) -> fmt::Result {
        let mut start = 0;
        let iter = string
            .char_indices()
            .filter_map(|(i, ch)| html_escape(ch).map(|s| (i, s)));

        for (end, escape) in iter {
            self.buf.write_str(&string[start .. end])?;
            self.buf.write_str(escape)?;
            start = end + 1;
        }

        self.buf.write_str(&string[start ..])?;
        Ok(())
    }
}

/// Wrapper over any component in order to force it to be blocking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Blocking<T>(pub T)
where
    T: Component;

impl<T> Component for Blocking<T>
where
    T: Component,
{
    type Kind = BlockComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        self.0.to_html(fmt, ctx)
    }
}

/// Uses given data to make a component if data implements [`fmt::Display`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Display<T>(pub T)
where
    T: fmt::Debug + fmt::Display;

impl<T> Component for Display<T>
where
    T: fmt::Debug + fmt::Display,
{
    type Kind = InlineComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, _ctx: Context) -> fmt::Result {
        write!(FmtAdapter { buf: fmt }, "{}", self.0)
    }
}
