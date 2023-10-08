use super::Style;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum StyleDelta {
    PrefixUsing(Style),
    #[default]
    Empty,
}

impl Style {
    pub fn compute_delta(self, next: Style) -> StyleDelta {
        println!("computing delta");
        dbg!(self, next);
        if self == next {
            println!("self == next, returning Empty");
            StyleDelta::Empty
        } else if next.is_empty() && !self.is_empty() {
            println!("self is not plain, next requires reset, returning:");
            dbg!(next.prefix_with_reset());
            StyleDelta::PrefixUsing(next.prefix_with_reset())
        } else {
            let turned_off_in_next = Style::turned_off(self, next);
            dbg!(turned_off_in_next);
            if turned_off_in_next.has_formatting() || turned_off_in_next.has_color() {
                println!(
                    "formatting, or a color was turned off; returning next with reset prefix:"
                );
                dbg!(next.prefix_with_reset());
                StyleDelta::PrefixUsing(next.prefix_with_reset())
            } else {
                let turned_on_from_self = Self::turned_on(self, next);
                dbg!(turned_on_from_self);
                let mut r = Style::default().set_flags(turned_on_from_self);
                if self.is_foreground() != next.is_foreground() {
                    r = r.set_foreground(next.coloring.foreground);
                }
                if self.is_background() != next.is_background() {
                    r = r.set_background(next.coloring.background);
                }
                println!("returning: ");
                dbg!(r);
                StyleDelta::PrefixUsing(r)
            }
        }
    }
}

impl StyleDelta {
    pub fn delta_next(self, next: Style) -> StyleDelta {
        match self {
            StyleDelta::PrefixUsing(current) => current.compute_delta(next),
            StyleDelta::Empty => StyleDelta::PrefixUsing(next),
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

    test!(nothing:    Green.foreground(); Green.foreground()  => Empty);
    test!(bold:  Green.foreground(); Green.bold()    => PrefixUsing(style().bold()));
    test!(unbold:  Green.bold();   Green.foreground()  => PrefixUsing(style().foreground(Green).prefix_with_reset()));
    test!(nothing2:   Green.bold();   Green.bold()    => Empty);

    test!(color_change: Red.foreground(); Blue.foreground() => PrefixUsing(style().foreground(Blue)));

    test!(addition_of_blink:          style(); style().blink()          => PrefixUsing(style().blink()));
    test!(addition_of_dimmed:         style(); style().dimmed()         => PrefixUsing(style().dimmed()));
    test!(addition_of_hidden:         style(); style().hidden()         => PrefixUsing(style().hidden()));
    test!(addition_of_reverse:        style(); style().reverse()        => PrefixUsing(style().reverse()));
    test!(addition_of_strikethrough:  style(); style().strikethrough()  => PrefixUsing(style().strikethrough()));

    test!(removal_of_strikethrough:   style().strikethrough(); style()  => PrefixUsing(style().prefix_with_reset()));
    test!(removal_of_reverse:         style().reverse();       style()  => PrefixUsing(style().prefix_with_reset()));
    test!(removal_of_hidden:          style().hidden();        style()  => PrefixUsing(style().prefix_with_reset()));
    test!(removal_of_dimmed:          style().dimmed();        style()  => PrefixUsing(style().prefix_with_reset()));
    test!(removal_of_blink:           style().blink();         style()  => PrefixUsing(style().prefix_with_reset()));
}
