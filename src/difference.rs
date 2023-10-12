use crate::style::{Coloring, FormatFlags};

use super::Style;

/// When printing out one colored string followed by another, use one of
/// these rules to figure out which *extra* control codes need to be sent.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum StyleDelta {
    /// Print out the control codes specified by this style to end up looking
    /// like the second string's styles.
    ExtraStyles(Style),
    /// The before style is exactly the same as the after style, so no further
    /// control codes need to be printed.
    #[default]
    Empty,
}

/// Tracks which styling properties exist.
#[derive(Clone, Copy, Debug)]
pub struct BoolStyle {
    /// Whether this style will be prefixed with [`RESET`](crate::ansi::RESET).
    pub reset_before_style: bool,
    /// Flags representing whether particular formatting properties are set or not.
    pub formats: FormatFlags,
    /// Data regarding the foreground/background color applied by this style.
    pub coloring: BoolColoring,
}

/// Tracks which colors are set.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct BoolColoring {
    pub foreground: bool,
    pub background: bool,
}

impl BoolColoring {
    /// Check if there are no colors set.
    pub fn is_empty(&self) -> bool {
        !(self.background || self.foreground)
    }
}

impl From<Coloring> for BoolColoring {
    fn from(coloring: Coloring) -> Self {
        BoolColoring {
            foreground: coloring.fg.is_some(),
            background: coloring.bg.is_some(),
        }
    }
}

/// Trait for types which can compute how they changed.
pub trait Difference: Clone + Copy {
    /// Take the complement (for boolean types, this would be the `!` operator).
    fn not(self) -> Self;

    /// Take the conjunction (for boolean types, this would be the `&&` operator).
    fn conjunction(self, other: Self) -> Self;

    /// Compute what turned on in `after` relative to `before`.
    fn turned_on(before: Self, after: Self) -> Self {
        before.not().conjunction(after)
    }

    /// Compute what turned off in `after` relative to `before`.
    fn turned_off(before: Self, after: Self) -> Self {
        before.conjunction(after.not())
    }
}

impl Difference for bool {
    fn not(self) -> Self {
        !self
    }

    fn conjunction(self, other: Self) -> Self {
        self && other
    }
}

impl Difference for BoolColoring {
    fn not(self) -> Self {
        Self {
            foreground: self.foreground.not(),
            background: self.background.not(),
        }
    }

    fn conjunction(self, other: Self) -> Self {
        Self {
            foreground: self.foreground.conjunction(other.foreground),
            background: self.background.conjunction(other.background),
        }
    }
}

impl Difference for FormatFlags {
    fn not(self) -> Self {
        !self
    }

    fn conjunction(self, other: Self) -> Self {
        self & other
    }
}

impl Difference for BoolStyle {
    fn not(self) -> Self {
        Self {
            reset_before_style: self.reset_before_style.not(),
            formats: self.formats.complement(),
            coloring: self.coloring.not(),
        }
    }

    fn conjunction(self, other: Self) -> Self {
        Self {
            reset_before_style: self
                .reset_before_style
                .conjunction(other.reset_before_style),
            formats: self.formats.conjunction(other.formats),
            coloring: self.coloring.conjunction(other.coloring),
        }
    }
}

impl From<Style> for BoolStyle {
    fn from(style: Style) -> Self {
        let Style {
            prefix_before_reset: reset_before_style,
            formats,
            coloring,
        } = style;
        Self {
            reset_before_style,
            formats,
            coloring: coloring.into(),
        }
    }
}

impl Style {
    /// Computes the differences between two consecutive styles, returning a
    /// result specifying the minimum `Style` required to change from the first
    /// (`self`) style to the `next` style.
    pub fn compute_delta(self, next: Style) -> StyleDelta {
        println!("computing delta");
        dbg!(self, next);
        if self == next {
            // If self is the same as next, no changes are required.
            StyleDelta::Empty
        } else if (next.is_empty() && !self.is_empty()) || next.is_reset_before_style() {
            // If either the next style is empty, and this is not, OR if the
            // next style requires/explicitly requests a reset before style,
            // emit the next style with a reset.
            StyleDelta::ExtraStyles(next.reset_before_style())
        } else {
            // No colors or formatting options were turned off. But there might
            // have been things turned on, or colors that have changed. This
            // case handles that.
            let turned_off_in_next = BoolStyle::turned_off(self.into(), next.into());
            if turned_off_in_next.formats.is_empty() && turned_off_in_next.coloring.is_empty() {
                let turned_on_from_self = BoolStyle::turned_on(self.into(), next.into());
                let mut r = Style::default().insert_formats(turned_on_from_self.formats);
                if self.is_fg() != next.is_fg() {
                    r = r.set_fg(next.coloring.fg);
                }
                if self.is_bg() != next.is_bg() {
                    r = r.set_bg(next.coloring.bg);
                }
                StyleDelta::ExtraStyles(r)
            } else {
                // If colors were turned off, we need to reset.
                StyleDelta::ExtraStyles(next.reset_before_style())
            }
        }
    }
}

impl StyleDelta {
    pub fn delta_next(self, next: Style) -> StyleDelta {
        match self {
            StyleDelta::ExtraStyles(current) => current.compute_delta(next),
            StyleDelta::Empty => StyleDelta::ExtraStyles(next),
        }
    }
}

#[cfg(test)]
mod test {
    use super::StyleDelta::*;
    use crate::style::Color::*;
    use crate::style::Style;

    fn style() -> Style {
        Style::new()
    }

    macro_rules! test {
        ($name: ident: $first: expr; $next: expr => $result: expr) => {
            #[test]
            fn $name() {
                let outcome = $first.compute_delta($next);
                let expected = $result;
                if outcome != expected {
                    use crate::debug::DebugDiff;
                    let diff = outcome.debug_diff(&expected);
                    println!("difference!\n{diff}");
                }
                assert_eq!(outcome, expected);
            }
        };
    }

    test!(nothing:    Green.normal(); Green.normal()  => Empty);
    test!(bold:  Green.normal(); Green.bold()    => ExtraStyles(style().bold()));
    test!(unbold:  Green.bold();   Green.normal()  => ExtraStyles(style().fg(Green).reset_before_style()));
    test!(nothing2:   Green.bold();   Green.bold()    => Empty);

    test!(color_change: Red.normal(); Blue.normal() => ExtraStyles(style().fg(Blue)));

    test!(addition_of_blink:          style(); style().blink()          => ExtraStyles(style().blink()));
    test!(addition_of_dimmed:         style(); style().dimmed()         => ExtraStyles(style().dimmed()));
    test!(addition_of_hidden:         style(); style().hidden()         => ExtraStyles(style().hidden()));
    test!(addition_of_reverse:        style(); style().reverse()        => ExtraStyles(style().reverse()));
    test!(addition_of_strikethrough:  style(); style().strikethrough()  => ExtraStyles(style().strikethrough()));

    test!(removal_of_strikethrough:   style().strikethrough(); style()  => ExtraStyles(style().reset_before_style()));
    test!(removal_of_reverse:         style().reverse();       style()  => ExtraStyles(style().reset_before_style()));
    test!(removal_of_hidden:          style().hidden();        style()  => ExtraStyles(style().reset_before_style()));
    test!(removal_of_dimmed:          style().dimmed();        style()  => ExtraStyles(style().reset_before_style()));
    test!(removal_of_blink:           style().blink();         style()  => ExtraStyles(style().reset_before_style()));
}
