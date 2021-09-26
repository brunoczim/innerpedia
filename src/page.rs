//! This module covers the more exterior pieces of a page, other than the more
//! inner components.

use crate::{
    component::{Context, DynComponent, InlineComponent},
    location::{Id, InternalLoc, InternalPath, Location},
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

impl Section {
    fn renderer<'this, 'loc, 'site>(
        &'this self,
        level: u32,
        ctx: Context<'loc, 'site>,
    ) -> SectionRenderer<'this, 'loc, 'site> {
        SectionRenderer { section: self, level, ctx }
    }
}

/// Internal (private) section renderer.
#[derive(Debug, Clone, Copy)]
struct SectionRenderer<'section, 'loc, 'site> {
    section: &'section Section,
    level: u32,
    ctx: Context<'loc, 'site>,
}

impl<'section, 'loc, 'site> fmt::Display
    for SectionRenderer<'section, 'loc, 'site>
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
            write!(fmt, "{}", section.renderer(self.level + 1, self.ctx))?;
        }

        write!(fmt, "</div></div>")?;

        Ok(())
    }
}

/// Configuration over a page's head.
#[derive(Debug, Clone)]
pub struct HeadConfig {
    /// Stylesheets (of CSS) used in this page.
    pub stylesheets: Vec<Location>,
    /// Scripts (of JavaScript) used in this page.
    pub scripts: Vec<Location>,
}

impl HeadConfig {
    fn renderer<'this, 'loc, 'site>(
        &'this self,
        ctx: Context<'loc, 'site>,
    ) -> HeadCfgRenderer<'this, 'loc, 'site> {
        HeadCfgRenderer { config: self, ctx }
    }
}

/// Internal (private) section renderer.
#[derive(Debug, Clone, Copy)]
struct HeadCfgRenderer<'cfg, 'loc, 'site> {
    config: &'cfg HeadConfig,
    ctx: Context<'loc, 'site>,
}

impl<'cfg, 'loc, 'site> fmt::Display for HeadCfgRenderer<'cfg, 'loc, 'site> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for stylesheet in &self.config.stylesheets {
            write!(
                fmt,
                "<link rel=\"stylesheet\" type=\"text/css\" href=\"{}\">",
                self.ctx.renderer(stylesheet)
            )?;
        }

        for script in &self.config.scripts {
            write!(
                fmt,
                "<script type=\"text/javascript\" src=\"{}\">",
                self.ctx.renderer(script)
            )?;
        }

        Ok(())
    }
}

/// Configuration for a page, usually fixed.
#[derive(Debug, Clone)]
pub struct Config {
    /// Head configuration used for this page.
    pub head: HeadConfig,
    /// Element used for this page's banner.
    pub banner: DynComponent,
}

/// A page of the encyclopedia. Contains everything in a page.
#[derive(Debug, Clone)]
pub struct Page {
    /// Configuration for this page, usually fixed for some set of pages.
    pub config: Config,
    /// The external-most title of the page.
    pub title: String,
    /// The external-most body of the page.
    pub body: DynComponent,
    /// Child sections of the page.
    pub sections: Vec<Section>,
}

impl Page {
    pub fn renderer<'this, 'loc, 'site>(
        &'this self,
        location: &'loc InternalPath,
        site: &'site Site,
    ) -> PageRenderer<'this, 'loc, 'site> {
        PageRenderer { page: self, location, site }
    }
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
pub struct PageRenderer<'page, 'loc, 'site> {
    /// The page being rendered.
    pub page: &'page Page,
    /// Path to this page.
    pub location: &'loc InternalPath,
    /// The target site (not modified).
    pub site: &'site Site,
}

impl<'page, 'loc, 'site> fmt::Display for PageRenderer<'page, 'loc, 'site> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let ctx = Context::new(self.location, self.site);
        write!(
            fmt,
            "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta \
             name=\"viewport\" content=\"width=device-width, \
             initial-scale=1.0\"><title>{title}</title>{headcfg}<body><div \
             id=\"page-top\"><div \
             id=\"banner\">{banner}</div><h1>{title}</h1><div \
             id=\"body-wrapper\">{body}",
            title = ctx.renderer(&self.page.title),
            headcfg = self.page.config.head.renderer(ctx),
            banner = ctx.renderer(&self.page.config.banner),
            body = ctx.renderer(&self.page.body),
        )?;

        for section in &self.page.sections {
            write!(fmt, "{}", section.renderer(1, ctx))?;
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
