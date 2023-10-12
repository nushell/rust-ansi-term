use cellopt::CellOpt;
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
    );

    // println!("{}", {
    //     use nu_ansi_term::{AnsiGenericString, FmtArgRenderer};
    //     use std::cell::RefCell;
    //     use std::fmt;

    //     #[derive(Default)]
    //     struct GenericFmtArgImplementor15549459557283006301<'a> {
    //         render_inputs: Vec<AnsiGenericString<'a, str>>,
    //         opt: RefCell<Option<fmt::Arguments<'a>>>,
    //     }
    //     impl<'a> GenericFmtArgImplementor15549459557283006301<'a> {
    //         fn new_boxed(
    //             render_inputs: Vec<AnsiGenericString<'a, str>>,
    //         ) -> Box<dyn FmtArgRenderer<str>> {
    //             Box::new(Self {
    //                 render_inputs,
    //                 opt: RefCell::new(None),
    //             })
    //         }
    //     }
    //     impl<'a> FmtArgRenderer<'a, str> for GenericFmtArgImplementor15549459557283006301<'a>
    //     where
    //         Self: 'a + Clone,
    //     {
    //         fn render_inputs_ref(&self) -> &[AnsiGenericString<'a, str>] {
    //             &self.render_inputs
    //         }
    //         fn render_inputs_mut(&mut self) -> &mut [AnsiGenericString<'a, str>] {
    //             &mut self.render_inputs
    //         }
    //         fn clone_renderer(&self) -> Box<dyn FmtArgRenderer<'a, str>> {
    //             Box::new(self.clone())
    //         }
    //         fn render(&self) -> String {
    //             let render_inputs = self.render_inputs_ref();
    //             format!(
    //                 "{}{}{}",
    //                 render_inputs[0usize].clone(),
    //                 render_inputs[1usize].clone(),
    //                 render_inputs[2usize].clone()
    //             )
    //         }
    //     }
    //     impl<'a> Clone for GenericFmtArgImplementor15549459557283006301<'a> {
    //         fn clone(&self) -> Self {
    //             Self {
    //                 render_inputs: self.render_inputs.clone(),
    //                 opt: RefCell::new(None),
    //             }
    //         }
    //     }
    //     impl<'a> fmt::Debug for GenericFmtArgImplementor15549459557283006301<'a> {
    //         fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    //             f.debug_struct("GenericFmtArgImplementor15549459557283006301").field("render_inputs", &self.render_inputs).field("fmt_args", &"Some(format_args ! (\"{}{}{}\" , render_inputs [0usize] , render_inputs [1usize] , render_inputs [2usize]))").finish()
    //         }
    //     }
    //     GenericFmtArgImplementor15549459557283006301::new_boxed(
    //         (<[_]>::into_vec(std::boxed::Box::new([
    //             (AnsiGenericString::from("format ")),
    //             (AnsiGenericString::from(Color::Blue.paint(format_args!(" args ")))),
    //             (AnsiGenericString::from(
    //                 Style::new().bold().paint(format_args!(" can be styled!")),
    //             )),
    //         ]))),
    //     )
    // });
}
