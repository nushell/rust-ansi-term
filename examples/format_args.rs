use nu_ansi_term::{Color, Style};

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
}
