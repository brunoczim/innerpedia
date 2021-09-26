//! This module provides location, paths, URLs.

use crate::component::{Component, Context, InlineComponent};
use percent_encoding::{percent_encode, CONTROLS};
use std::{fmt, path::PathBuf, str};
use thiserror::Error;
use url::Url;

/// A location of a page, either internal or external.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Location {
    /// An external page (or an internal page encoded as an external URL).
    URL(Url),
    /// An internal location.
    Internal(InternalLoc),
}

impl From<InternalPath> for Location {
    fn from(path: InternalPath) -> Self {
        Location::Internal(InternalLoc::from(path))
    }
}

impl From<InternalLoc> for Location {
    fn from(loc: InternalLoc) -> Self {
        Location::Internal(loc)
    }
}

impl From<Url> for Location {
    fn from(url: Url) -> Self {
        Location::URL(url)
    }
}

impl Location {
    /// Parses an internal location but returns a generic location.
    pub fn internal<S>(contents: S) -> Self
    where
        S: AsRef<str>,
    {
        Location::Internal(InternalLoc::parse(contents).unwrap())
    }
}

impl Component for Location {
    type Kind = InlineComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        match self {
            Location::URL(url) => write!(fmt, "{}", url),
            Location::Internal(int) => int.to_html(fmt, ctx),
        }
    }
}

/// An internal path, without any ID. Always absolute (with the root pointing to
/// the root of the encyclopedia).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternalPath {
    /// Fragments of the path (i.e. each piece, each element).
    pub fragments: Vec<Fragment>,
}

impl InternalPath {
    /// Parser the internal path. Fragments separated by "/".
    pub fn parse<S>(string: S) -> anyhow::Result<Self>
    where
        S: AsRef<str>,
    {
        let string = string.as_ref();
        let mut this = Self { fragments: Vec::new() };

        if string.len() > 0 {
            for fragment in string.split('/') {
                this.fragments.push(Fragment::new(fragment)?);
            }
        }

        Ok(this)
    }

    /// Path to the root of the encyclopedia.
    pub fn root() -> Self {
        Self { fragments: Vec::new() }
    }

    /// Tests if this path leads to the root.
    pub fn is_root(&self) -> bool {
        self.fragments.len() == 0
    }

    /// Counts the directory depth.
    pub fn dir_depth(&self) -> usize {
        self.fragments.len().saturating_sub(1)
    }

    /// Creates an OS path buffer.
    pub fn to_fs_path(&self) -> PathBuf {
        PathBuf::from(format!("{}", self))
    }

    /// Appends a fragment (a piece) to the end of this path. Returns the
    /// modified path.
    pub fn append(mut self, fragment: Fragment) -> Self {
        self.fragments.push(fragment);
        self
    }

    /// Compares two locations taking "index" into account and ignoring its
    /// presence.
    pub fn eq_index(&self, other: &Self) -> bool {
        if self.fragments.len() == other.fragments.len() + 1 {
            let last_index = self.fragments.len() - 1;
            self.fragments[last_index].as_str() == "index.html"
                && self.fragments[.. last_index] == other.fragments
        } else if other.fragments.len() == self.fragments.len() + 1 {
            let last_index = other.fragments.len() - 1;
            other.fragments[last_index].as_str() == "index.html"
                && other.fragments[.. last_index] == self.fragments
        } else {
            self == other
        }
    }
}

impl Default for InternalPath {
    fn default() -> Self {
        Self::root()
    }
}

impl fmt::Display for InternalPath {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;

        for fragment in &self.fragments {
            if first {
                first = false;
            } else {
                fmt.write_str("/")?;
            }
            write!(fmt, "{}", fragment)?;
        }

        Ok(())
    }
}

impl Component for InternalPath {
    type Kind = InlineComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        if !self.eq_index(ctx.location()) {
            for _ in 0 .. ctx.location().dir_depth() {
                fmt.write_str("../")?;
            }
            let encoded = percent_encode(self.to_string().as_bytes(), CONTROLS)
                .collect::<String>();
            write!(fmt, "{}", ctx.renderer(&encoded))?;
        }
        Ok(())
    }
}

/// A location to an internal page, with optional ID. Always absolute (with the
/// root pointing to the root of the encyclopedia).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct InternalLoc {
    /// Path to the document.
    pub path: InternalPath,
    /// ID of the section or specific object inside of the document.
    pub id: Option<Id>,
}

impl From<InternalPath> for InternalLoc {
    fn from(path: InternalPath) -> Self {
        Self { path, id: None }
    }
}

impl InternalLoc {
    /// Parses an internal location. Path fragments separated by "/", ID
    /// appended to the end with "#" between the path and the ID, if any ID
    /// at all.
    pub fn parse<S>(string: S) -> anyhow::Result<Self>
    where
        S: AsRef<str>,
    {
        let string = string.as_ref();
        let hash = string
            .as_bytes()
            .iter()
            .rposition(|&ch| ch == b'#')
            .unwrap_or(string.len());

        Ok(Self {
            path: InternalPath::parse(&string[.. hash])?,
            id: if hash == string.len() {
                None
            } else {
                Some(Id::new(&string[hash + 1 ..])?)
            },
        })
    }
}

impl fmt::Display for InternalLoc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", &self.path)?;

        if let Some(id) = &self.id {
            write!(fmt, "#{}", id)?;
        }

        Ok(())
    }
}

impl Component for InternalLoc {
    type Kind = InlineComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        self.path.to_html(fmt, ctx)?;
        if let Some(id) = &self.id {
            fmt.write_str("#")?;
            id.to_html(fmt, ctx)?;
        }
        Ok(())
    }
}

/// Error when an invalid ID string is given to be parsed.
#[derive(Debug, Clone, Error)]
#[error("Invalid ID string")]
pub struct InvalidId;

/// An ID of a location.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id {
    contents: Box<str>,
}

impl Id {
    /// Creates an ID from the desired string contents. The string can only
    /// contain alphanumeric characters or '_' or '-'.
    pub fn new<S>(contents: S) -> anyhow::Result<Self>
    where
        S: AsRef<str> + Into<Box<str>>,
    {
        let mut iter = contents.as_ref().as_bytes().iter();

        iter.next().filter(|ch| ch.is_ascii_alphabetic()).ok_or(InvalidId)?;

        for &ch in iter {
            if !ch.is_ascii_alphanumeric() && ch != b'_' && ch != b'-' {
                Err(InvalidId)?;
            }
        }

        Ok(Self { contents: contents.into() })
    }

    /// The string contents of this ID.
    pub fn as_str(&self) -> &str {
        &self.contents
    }
}

impl fmt::Display for Id {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(&self.as_str())
    }
}

impl Component for Id {
    type Kind = InlineComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, _ctx: Context) -> fmt::Result {
        write!(fmt, "{}", self)
    }
}

/// Error when an invalid fragment (piece of a path) string is given to be
/// parsed.
#[derive(Debug, Clone, Error)]
#[error("Invalid location fragment string")]
pub struct InvalidFragment;

/// A fragment of a path, that is, a piece, an element of it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fragment {
    contents: Box<str>,
}

impl Fragment {
    /// Creates a fragment from the desired string contents. The string cannot
    /// contain '/' or '#', it cannot be empty or composed of only "." or
    /// ".." as well.
    pub fn new<S>(contents: S) -> anyhow::Result<Self>
    where
        S: AsRef<str> + Into<Box<str>>,
    {
        if let "" | "." | ".." = contents.as_ref() {
            Err(InvalidFragment)?;
        }

        for ch in contents.as_ref().bytes() {
            if let b'/' | b'#' = ch {
                Err(InvalidFragment)?;
            }
        }

        Ok(Self { contents: contents.into() })
    }

    /// The string contents of this fragment.
    pub fn as_str(&self) -> &str {
        &self.contents
    }
}

impl fmt::Display for Fragment {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(&self.as_str())
    }
}

#[cfg(test)]
mod test {
    use super::InternalPath;

    #[test]
    fn eq_index() {
        let left = InternalPath::parse("langs/div-prt/phonology").unwrap();
        let right =
            InternalPath::parse("langs/div-prt/phonology/index.html").unwrap();
        assert!(left.eq_index(&right));
        assert!(right.eq_index(&left));
    }
}
