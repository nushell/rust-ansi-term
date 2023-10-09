use nu_ansi_term::{Color, Style};

fn main() {
    // However, notice that nested format_args occurrences don't quite behave as
    // one might expect, because fmt::Arguments is "opaque" regarding its
    // contents, styles between layers do not interact.
    println!(
        "{}",
        Style::new().blink().paint(format_args!(
            "{}{}{}",
            "format ",
            Color::Blue.paint(format_args!(" args ")),
            Style::new().bold().paint(format_args!(" can be styled!"))
        ))
    )
}
