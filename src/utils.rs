use crate::display::AnsiStrings;

/// Return a substring of the given AnsiStrings sequence, while keeping the formatting.
pub fn sub_string<'a>(start: usize, len: usize, strs: &AnsiStrings) -> AnsiStrings<'a> {
    let mut vec = Vec::new();
    let mut pos = start;
    let mut len_rem = len;

    for i in strs.iter() {
        let content = i.content.to_string();
        let frag_len = content.len();
        if pos >= frag_len {
            pos -= frag_len;
            continue;
        }
        if len_rem == 0 {
            break;
        }

        let end = pos + len_rem;
        let pos_end = if end >= frag_len { frag_len } else { end };

        vec.push(i.style_ref().paint(String::from(&content[pos..pos_end])));

        if end <= frag_len {
            break;
        }

        len_rem -= pos_end - pos;
        pos = 0;
    }

    AnsiStrings(vec)
}

/// Return a concatenated copy of `strs` without the formatting, as an allocated `String`.
pub fn unstyle(strs: &AnsiStrings) -> String {
    let mut s = String::new();

    for i in strs.iter() {
        s += &i.content.to_string();
    }

    s
}

/// Return the unstyled length of AnsiStrings. This is equaivalent to `unstyle(strs).len()`.
pub fn unstyled_len(strs: &AnsiStrings) -> usize {
    let mut l = 0;
    for i in strs.iter() {
        l += i.content.to_string().len();
    }
    l
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Color::*;

    #[test]
    fn test_unstyling() {
        let l = AnsiStrings([
            Black.paint("first"),
            Red.paint("-second"),
            White.paint("-third"),
        ]);
        let a = l.clone();
        assert_eq!(unstyle(&a), "first-second-third");
        assert_eq!(unstyled_len(&a), 18);
    }

    #[test]
    fn test_substring() {
        let l = AnsiStrings([
            Black.paint("first"),
            Red.paint("-second"),
            White.paint("-third"),
        ]);

        let l2 = AnsiStrings([Black.paint("st"), Red.paint("-second"), White.paint("-t")]);
        assert_eq!(&sub_string(3, 11, &l).to_string(), &l2.to_string());
    }
}
