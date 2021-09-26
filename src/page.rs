//! This module covers the more exterior pieces of a page, other than the more
//! inner components.

use crate::{
    component::{Context, DynComponent, InlineComponent},
    location::{Id, InternalLoc, InternalPath},
    site::Site,
};
use std::fmt;

/// A section of the page, on either top-level, nested in one level, nested in
/// two leves, etc.
#[derive(Debug, Clone)]
pub struct Section {
    /// Title of the section. Displayed above.
    pub title: DynComponent<InlineComponent>,
    /// Body of the section. Main display.
    pub body: DynComponent,
    /// ID that references the section.
    pub id: Id,
    /// Children sections. Possibly empty.
    pub children: Vec<Section>,
}

/// Internal (private) section renderer.
#[derive(Debug, Clone, Copy)]
struct RenderSection<'section, 'loc, 'site> {
    section: &'section Section,
    level: u32,
    ctx: Context<'loc, 'site>,
}

impl<'section, 'loc, 'site> fmt::Display
    for RenderSection<'section, 'loc, 'site>
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut path = self.ctx.location().clone();
        if path
            .fragments
            .last()
            .map_or(false, |last| last.as_str() == "index.html")
        {
            path.fragments.pop();
        }

        write!(
            fmt,
            "<div id={id} class=\"section section-{level}\"><{title_tag} \
             class=\"header\"><a class=\"header-link\" \
             href=\"{link}\">{title}</a></{title_tag}><div \
             class=\"section-body\">{body}",
            level = self.level,
            title_tag = heading_level(self.level),
            title = self.ctx.renderer(&self.section.title),
            body = self.ctx.renderer(&self.section.body),
            id = self.ctx.renderer(&self.section.id),
            link = self.ctx.renderer(InternalLoc {
                path,
                id: Some(self.section.id.clone())
            })
        )?;

        for section in &self.section.children {
            write!(
                fmt,
                "{}",
                RenderSection { level: self.level + 1, section, ..*self }
            )?;
        }

        write!(fmt, "</div></div>")?;

        Ok(())
    }
}

/// A page of the encyclopedia. Contains everything in a page.
#[derive(Debug, Clone)]
pub struct Page {
    /// The external-most title of the page.
    pub title: String,
    /// The external-most body of the page.
    pub body: DynComponent,
    /// Child sections of the page.
    pub sections: Vec<Section>,
}

impl AsRef<Page> for Page {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl AsMut<Page> for Page {
    fn as_mut(&mut self) -> &mut Self {
        self
    }
}

/// The renderer of a page. Publicly usable and creatable.
#[derive(Debug, Clone, Copy)]
pub struct RenderPage<'page, 'loc, 'site> {
    /// The page being rendered.
    pub page: &'page Page,
    /// Path to this page.
    pub location: &'loc InternalPath,
    /// The target site (not modified).
    pub site: &'site Site,
}

impl<'page, 'loc, 'site> fmt::Display for RenderPage<'page, 'loc, 'site> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let ctx = Context::new(self.location, self.site);
        write!(
            fmt,
            "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta \
             name=\"viewport\" content=\"width=device-width, \
             initial-scale=1.0\"><link rel=\"stylesheet\" type=\"text/css\" \
             href=\"{css}\"><title>{title}</title><body><div \
             id=\"page-top\"><div id=\"banner\"><a href=\"{home}\">The World \
             From The Stars</a></div><h1>{title}</h1><div \
             id=\"body-wrapper\">{body}",
            css = ctx.renderer(InternalPath::parse("css/main.css").unwrap()),
            title = ctx.renderer(&self.page.title),
            home = ctx.renderer(InternalPath::parse("").unwrap()),
            body = ctx.renderer(&self.page.body),
        )?;

        for section in &self.page.sections {
            write!(fmt, "{}", RenderSection { level: 1, ctx, section })?;
        }

        write!(fmt, "</div></div></body></html>")?;
        Ok(())
    }
}

fn heading_level(section_level: u32) -> &'static str {
    match section_level {
        0 => "h1",
        1 => "h2",
        2 => "h3",
        3 => "h4",
        4 => "h5",
        _ => "h6",
    }
}
