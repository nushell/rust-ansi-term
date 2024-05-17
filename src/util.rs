use crate::display::{AnsiString, AnsiStrings};

/// Return a substring of the given `AnsiStrings` sequence, while keeping the formatting.
#[must_use]
pub fn sub_string(start: usize, len: usize, strs: &AnsiStrings) -> Vec<AnsiString<'static>> {
    let mut vec = Vec::new();
    let mut pos = start;
    let mut len_rem = len;

    for i in strs.0 {
        let frag_len = i.string.len();
        if pos >= frag_len {
            pos -= frag_len;
            continue;
        }
        if len_rem == 0 {
            break;
        }

        let end = pos + len_rem;
        let pos_end = if end >= frag_len { frag_len } else { end };

        vec.push(i.style_ref().paint(String::from(&i.string[pos..pos_end])));

        if end <= frag_len {
            break;
        }

        len_rem -= pos_end - pos;
        pos = 0;
    }

    vec
}

/// Return a concatenated copy of `strs` without the formatting, as an allocated `String`.
#[must_use]
pub fn unstyle(strs: &AnsiStrings) -> String {
    let mut s = String::new();

    for i in strs.0 {
        s += &*i.string;
    }

    s
}

/// Return the unstyled length of `AnsiStrings`. This is equivalent to `unstyle(strs).len()`.
#[must_use]
pub fn unstyled_len(strs: &AnsiStrings) -> usize {
    let mut l = 0;
    for i in strs.0 {
        l += i.string.len();
    }
    l
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Color::*;

    #[test]
    fn test() {
        let l = [
            Black.paint("first"),
            Red.paint("-second"),
            White.paint("-third"),
        ];
        let a = AnsiStrings(&l);
        assert_eq!(unstyle(&a), "first-second-third");
        assert_eq!(unstyled_len(&a), 18);

        let l2 = [Black.paint("st"), Red.paint("-second"), White.paint("-t")];
        assert_eq!(sub_string(3, 11, &a), l2);
    }
}
