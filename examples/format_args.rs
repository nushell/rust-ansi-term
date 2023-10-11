use nu_ansi_term::{ansi_generics, Color, Style};

fn main() {
    // However, notice that nested format_args occurrences don't quite behave as
    // one might expect, because fmt::Arguments is "opaque" regarding its
    // contents, styles between layers do not interact. Instead, see the
    // `nested_strings` example of to see how one can do such a thing using `AnsiGenericStrings`
    println!(
        "{}",
        Style::new().blink().paint(ansi_generics!(
            "{}{}{}",
            "format ",
            Color::Blue.paint(format_args!(" args ")),
            Style::new().bold().paint(format_args!(" can be styled!"))
        ))
    )
}
