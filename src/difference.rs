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
                fn update(&self, next: Self, mut no_change: bool) -> (Self, bool) {
                    if self == next {
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
                Some($result)
            } else {
                None
            }
        }
    };
}

impl Style {
    fn update_with(self, next: Self) -> Self {
        let mut result = if requires_reset!(
            self,
            next | is_bold,
            is_dimmed,
            is_italic,
            is_underline,
            is_blink,
            is_reverse,
            is_strikethrough
        ) {
            return Some(next.prefix_with_reset());
        } else {
            let mut result = next;
            result.prefix_with_reset = false;
            result
        };

        update_fields!(
            result,
            self,
            next | foreground,
            background,
            is_bold,
            is_dimmed,
            is_italic,
            is_underline,
            is_blink,
            is_reverse,
            is_hidden,
            is_strikethrough
        )
    }
}

#[cfg(test)]
mod test {
    use crate::style::Color::*;
    use crate::style::Style;

    fn style() -> Style {
        Style::new()
    }

    macro_rules! test {
        ($name: ident: $first: expr; $next: expr => $result: expr) => {
            #[test]
            fn $name() {
                assert_eq!($result, $first.incremental_update($next));
            }
        };
    }

    test!(nothing:    Green.normal(); Green.normal()  => None);
    test!(bold:  Green.normal(); Green.bold()    => Some(style().bold()));
    test!(unbold:  Green.bold();   Green.normal()  => Some(style().fg(Green.normal()).prefix_with_reset()));
    test!(nothing2:   Green.bold();   Green.bold()    => None);

    test!(color_change: Red.normal(); Blue.normal() => Some(style().fg(Blue.normal())));

    test!(addition_of_blink:          style(); style().blink()          => Some(style().blink()));
    test!(addition_of_dimmed:         style(); style().dimmed()         => Some(style().dimmed()));
    test!(addition_of_hidden:         style(); style().hidden()         => Some(style().hidden()));
    test!(addition_of_reverse:        style(); style().reverse()        => Some(style().reverse()));
    test!(addition_of_strikethrough:  style(); style().strikethrough()  => Some(style().strikethrough()));

    test!(removal_of_strikethrough:   style().strikethrough(); style()  => Some(style().prefix_with_reset()));
    test!(removal_of_reverse:         style().reverse();       style()  => Some(style().prefix_with_reset()));
    test!(removal_of_hidden:          style().hidden();        style()  => Some(style().prefix_with_reset()));
    test!(removal_of_dimmed:          style().dimmed();        style()  => Some(style().prefix_with_reset()));
    test!(removal_of_blink:           style().blink();         style()  => Some(style().prefix_with_reset()));
}
