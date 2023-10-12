use nu_ansi_term::{ansi_generics, Color, Style};

fn main() {
    let yes = Color::Yellow.as_foreground().bold().paint("yes!");
    let exclamation = Color::Yellow
        .as_background()
        .foreground(Color::Black)
        .italic()
        .paint("true!");
    println!(
        "{} {} {}",
        Style::new().italic().underline().paint("hello"),
        Color::Cyan.paint("world!"),
        format_args!("{yes} it's {exclamation}")
    );

    println!(
        "{}",
        Style::new().blink().paint(ansi_generics!(
            "{}{}{}",
            "format ",
            Color::Blue.paint(format_args!(" args ")),
            Style::new().bold().paint(format_args!(" can be styled!"))
        ))
    );
}
