//! This module exports image-related components.

use crate::{
    component::{BlockComponent, Component, Context, InlineComponent},
    location::Location,
};
use std::fmt;

/// Just a plain image.
#[derive(Debug, Clone)]
pub struct Image {
    /// Location of the image file.
    pub src: Location,
    /// Alt text, i.e. short text associated with the image, mostly for
    /// accessibilty purposes or when the image could not be loaded.
    pub alt: String,
}

impl Component for Image {
    type Kind = BlockComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(
            fmt,
            "<img src=\"{}\" alt=\"{}\" class=\"image\">",
            ctx.renderer(&self.src),
            ctx.renderer(&self.alt),
        )?;
        Ok(())
    }
}

/// A figure: an image with a legend.
#[derive(Debug, Clone)]
pub struct Figure<L>
where
    L: Component<Kind = InlineComponent>,
{
    /// Image data.
    pub img: Image,
    /// The legend component.
    pub legend: L,
}

impl<L> Component for Figure<L>
where
    L: Component<Kind = InlineComponent>,
{
    type Kind = BlockComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(
            fmt,
            "<div class=\"fig-wrapper\">{}<div \
             class=\"fig-legend\">{}</div></div>",
            ctx.renderer(&self.img),
            ctx.renderer(&self.legend),
        )?;
        Ok(())
    }
}
