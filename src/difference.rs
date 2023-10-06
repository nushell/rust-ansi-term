use super::Style;
use crate::Color;

#[inline]
pub fn turned_off(current: bool, next: bool) -> bool {
    current && !next
}

macro_rules! requires_reset {
    ($self:ident, $next:ident | $($bool_field:ident),*) => {
        $(turned_off($self.$bool_field, $next.$bool_field))||* || turned_off($self.foreground.is_some(), $next.foreground.is_some()) ||
        turned_off($self.background.is_some(), $next.background.is_some())
    };
}

macro_rules! update_fields {
    ($result:ident, $self:ident, $next:ident | $($field:ident),*) => {
        {
            pub trait UpdateStyleValue: Default + Eq + PartialEq {
                #[inline]
                fn update(&self, next: Self, no_change: bool) -> (Self, bool) {
                    if *self == next {
                        (Self::default(), no_change)
                    } else {
                        (next, false)
                    }
                }
            }

            impl UpdateStyleValue for bool {}
            impl UpdateStyleValue for Option<Color> {}

            let mut no_change = true;
            $(($result.$field, no_change) = $self.$field.update($next.$field, no_change);)*

            if !no_change {
                UpdateCommand::Prefix(if $result.is_plain() {
                    $result.prefix_with_reset()
                } else {
                    $result
                })
            } else {
                UpdateCommand::DoNothing
            }
        }
    };
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum UpdateCommand {
    Prefix(Style),
    #[default]
    DoNothing,
}

impl UpdateCommand {
    pub fn compare_styles(first: Style, mut next: Style) -> UpdateCommand {
        let mut result = if requires_reset!(
            first,
            next | is_bold,
            is_dimmed,
            is_italic,
            is_underline,
            is_blink,
            is_reverse,
            is_hidden,
            is_strikethrough
        ) {
            return UpdateCommand::Prefix(next.prefix_with_reset());
        } else {
            next.prefix_with_reset = false;
            next
        };

        update_fields!(
            result,
            first,
            next | foreground,
            background,
            is_bold,
            is_dimmed,
            is_italic,
            is_underline,
            is_blink,
            is_reverse,
            is_hidden,
            is_strikethrough,
            prefix_with_reset
        )
    }
    pub fn update_relative(self, next: Style) -> UpdateCommand {
        match self {
            UpdateCommand::Prefix(first) => Self::compare_styles(first, next),
            UpdateCommand::DoNothing => Self::compare_styles(Style::default(), next),
        }
    }
}

#[cfg(test)]
mod test {
    use super::UpdateCommand;
    use super::UpdateCommand::*;
    use crate::style::Color::*;
    use crate::style::Style;

    fn style() -> Style {
        Style::new()
    }

    macro_rules! test {
        ($name: ident: $first: expr; $next: expr => $result: expr) => {
            #[test]
            fn $name() {
                assert_eq!($result, UpdateCommand::compare_styles($first, $next));
            }
        };
    }

    test!(nothing:    Green.normal(); Green.normal()  => DoNothing);
    test!(bold:  Green.normal(); Green.bold()    => Prefix(style().bold()));
    test!(unbold:  Green.bold();   Green.normal()  => Prefix(style().fg(Green).prefix_with_reset()));
    test!(nothing2:   Green.bold();   Green.bold()    => DoNothing);

    test!(color_change: Red.normal(); Blue.normal() => Prefix(style().fg(Blue)));

    test!(addition_of_blink:          style(); style().blink()          => Prefix(style().blink()));
    test!(addition_of_dimmed:         style(); style().dimmed()         => Prefix(style().dimmed()));
    test!(addition_of_hidden:         style(); style().hidden()         => Prefix(style().hidden()));
    test!(addition_of_reverse:        style(); style().reverse()        => Prefix(style().reverse()));
    test!(addition_of_strikethrough:  style(); style().strikethrough()  => Prefix(style().strikethrough()));

    test!(removal_of_strikethrough:   style().strikethrough(); style()  => Prefix(style().prefix_with_reset()));
    test!(removal_of_reverse:         style().reverse();       style()  => Prefix(style().prefix_with_reset()));
    test!(removal_of_hidden:          style().hidden();        style()  => Prefix(style().prefix_with_reset()));
    test!(removal_of_dimmed:          style().dimmed();        style()  => Prefix(style().prefix_with_reset()));
    test!(removal_of_blink:           style().blink();         style()  => Prefix(style().prefix_with_reset()));
}
