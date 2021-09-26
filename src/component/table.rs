//! This module exports items related to table components.

use crate::component::{
    BlockComponent,
    Component,
    Context,
    DynComponent,
    InlineComponent,
};
use std::fmt;

/// The type of the entries of a table. For regular tables, this will act like a
/// Matrix, but for irregular ones, it won't be a proper matrix.
pub type Entries<E> = Vec<Vec<Entry<E>>>;

/// A table with title and flexible columns.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Table<T, E>
where
    T: Component<Kind = InlineComponent>,
    E: Component,
{
    /// The title of the table.
    pub title: T,
    /// The nested-vector of table entries.
    pub entries: Entries<E>,
}

impl<T, E> Component for Table<T, E>
where
    T: Component<Kind = InlineComponent>,
    E: Component,
{
    type Kind = BlockComponent;

    fn to_html(&self, fmt: &mut fmt::Formatter, ctx: Context) -> fmt::Result {
        write!(
            fmt,
            "<div class=\"table-wrapper\"><span \
             class=\"table-title\">{title}</span><table class=\"table\">",
            title = ctx.renderer(&self.title),
        )?;

        for row in &self.entries {
            write!(fmt, "<tr>")?;
            for entry in row {
                write!(fmt, "<{}", entry.tag())?;
                if entry.rowspan != 1 {
                    write!(fmt, " rowspan=\"{}\"", entry.rowspan)?;
                }
                if entry.colspan != 1 {
                    write!(fmt, " colspan=\"{}\"", entry.colspan)?;
                }
                write!(
                    fmt,
                    ">{}</{}>",
                    ctx.renderer(&entry.data),
                    entry.tag()
                )?;
            }
            write!(fmt, "</tr>")?;
        }

        write!(fmt, "</table></div>")?;

        Ok(())
    }
}

/// An entry in the table.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Entry<T>
where
    T: Component,
{
    /// How many rows this entry occupies.
    pub rowspan: u32,
    /// How many columns this entry occupies.
    pub colspan: u32,
    /// Whether this is a header entry or not.
    pub header: bool,
    /// The displayed data component.
    pub data: T,
}

impl<K> Entry<DynComponent<K>> {
    pub fn dynamic<T>(data: T) -> Self
    where
        T: Component<Kind = K> + Send + Sync + 'static,
    {
        Self::new(data.to_dyn())
    }
}

impl<T> Entry<T>
where
    T: Component,
{
    /// Creates an entry with the default settings and the data component.
    pub fn new(data: T) -> Self {
        Self { data, rowspan: 1, colspan: 1, header: false }
    }

    fn tag(&self) -> &'static str {
        match self.header {
            true => "th",
            false => "td",
        }
    }
}

impl<T> Default for Entry<T>
where
    T: Component + Default,
{
    fn default() -> Self {
        Self::new(T::default())
    }
}
