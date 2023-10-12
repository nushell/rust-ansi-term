use nu_ansi_term::{Color, Style};

fn main() {
    let yes = Color::Yellow.normal().bold().paint("yes!");
    let exclamation = Color::Yellow.bg().fg(Color::Black).italic().paint("true!");
    println!(
        "{} {} {}",
        Style::new().italic().underline().paint("hello"),
        Color::Cyan.paint("world!"),
        format_args!("{yes} it's {exclamation}")
    );
}
