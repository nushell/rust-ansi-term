use super::Style;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum StyleDelta {
    PrefixUsing(Style),
    #[default]
    Empty,
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
                assert_eq!($result, $first.compute_delta($next));
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
