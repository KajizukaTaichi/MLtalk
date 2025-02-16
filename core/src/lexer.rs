use crate::*;

pub fn tokenize(input: &str, delimiter: &[&str], is_expr: bool) -> Result<Vec<String>, Fault> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;
    let mut is_escape = false;

    fn include_letter(query: &str, chars: &Vec<String>, idx: usize) -> bool {
        chars
            .clone()
            .get(idx..idx + query.chars().count())
            .map(|i| query == i.concat())
            .unwrap_or(false)
    }
    fn is_space_splited(chars: &Vec<String>, index: usize) -> bool {
        if let Some(index) = index.checked_sub(1) {
            if let Some(val) = chars.get(index) {
                SPACE.contains(&val.as_str()) || [";", ","].contains(&val.as_str())
            } else {
                true
            }
        } else {
            true
        }
    }

    let chars: Vec<String> = input.chars().map(String::from).collect();
    let mut index = 0;

    while index < chars.len() {
        let c = ok!(chars.get(index))?.to_owned();
        if is_escape {
            current_token.push_str(match c.as_str() {
                "n" => "\n",
                "t" => "\t",
                "r" => "\r",
                _ => &c,
            });
            is_escape = false;
            index += 1;
        } else if include_letter(BEGIN, &chars, index)
            && !in_quote
            && is_space_splited(&chars, index)
            && is_space_splited(&chars, index + BEGIN.chars().count() + 1)
        {
            current_token.push_str(BEGIN);
            index += BEGIN.chars().count();
            in_parentheses += 1;
        } else if include_letter(END, &chars, index)
            && !in_quote
            && is_space_splited(&chars, index)
            && is_space_splited(&chars, index + END.chars().count() + 1)
        {
            current_token.push_str(END);
            index += END.chars().count();
            if let Some(i) = in_parentheses.checked_sub(1) {
                in_parentheses = i;
            } else {
                return Err(Fault::Syntax);
            }
        } else if ["(", "[", "{"].contains(&c.as_str()) {
            current_token.push_str(c.as_str());
            in_parentheses += 1;
            index += 1;
        } else if [")", "]", "}"].contains(&c.as_str()) {
            current_token.push_str(c.as_str());
            if let Some(i) = in_parentheses.checked_sub(1) {
                in_parentheses = i;
            } else {
                return Err(Fault::Syntax);
            }
            index += 1;
        } else if ["\"", "'", "`"].contains(&c.as_str()) {
            in_quote = !in_quote;
            current_token.push_str(c.as_str());
            index += 1;
        } else if c == "\\" {
            current_token.push_str(&c);
            is_escape = true;
            index += 1;
        } else {
            let mut is_opr = false;
            if is_expr {
                'a: for op in OPERATOR {
                    if include_letter(op, &chars, index) && in_parentheses == 0 && !in_quote {
                        if current_token.is_empty() {
                            index += op.chars().count();
                            tokens.push(op.to_string());
                        } else {
                            tokens.push(current_token.to_string());
                            index += op.chars().count();
                            tokens.push(op.to_string());
                            current_token.clear();
                        }
                        is_opr = true;
                        break 'a;
                    }
                }
            }
            if !is_opr {
                let mut is_delimit = false;
                'b: for delimit in delimiter {
                    if include_letter(delimit, &chars, index) && in_parentheses == 0 && !in_quote {
                        if current_token.is_empty() {
                            index += delimit.chars().count();
                        } else {
                            tokens.push(current_token.clone());
                            index += delimit.chars().count();
                            current_token.clear();
                        }
                        is_delimit = true;
                        break 'b;
                    }
                }
                if !is_delimit {
                    current_token.push_str(c.as_str());
                    index += 1;
                }
            }
        }
    }

    // Syntax error check
    if is_escape || in_quote || in_parentheses != 0 {
        return Err(Fault::Syntax);
    }
    if !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Ok(tokens)
}

pub fn is_identifier(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    if name == "_" {
        return true;
    }
    let mut chars = name.chars();
    let first_char = chars.next().unwrap();
    if !UnicodeXID::is_xid_start(first_char) {
        return false;
    }
    if !chars.all(UnicodeXID::is_xid_continue) {
        return false;
    }
    if RESERVED.contains(&name) {
        return false;
    }
    true
}

pub fn str_format(input: &str) -> Result<Vec<String>, Fault> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut is_escape = false;

    for c in input.chars() {
        if is_escape {
            current_token.push(match c {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                _ => c,
            });
            is_escape = false;
        } else {
            match c {
                '{' => {
                    if in_parentheses == 0 {
                        if !current_token.is_empty() {
                            tokens.push(current_token.clone());
                        }
                        current_token = c.to_string();
                    } else {
                        current_token.push(c)
                    }
                    in_parentheses += 1;
                }
                '}' => {
                    current_token.push(c);
                    if in_parentheses != 0 {
                        in_parentheses -= 1;
                    } else {
                        return Err(Fault::Syntax);
                    }
                    if in_parentheses == 0 {
                        if !current_token.is_empty() {
                            tokens.push(current_token.clone());
                        }
                        current_token.clear();
                    }
                }
                '\\' => {
                    current_token.push(c);
                    is_escape = true;
                }
                _ => current_token.push(c),
            }
        }
    }

    // Syntax error check
    if is_escape || in_parentheses != 0 {
        return Err(Fault::Syntax);
    }
    if !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Ok(tokens)
}

pub fn str_escape(str: &str) -> String {
    let mut result = String::new();
    let mut is_escape = false;
    for c in str.chars() {
        if is_escape {
            result.push(c);
            is_escape = false;
        } else {
            match c {
                '\\' => {
                    is_escape = true;
                }
                _ => result.push(c),
            }
        }
    }
    result
}
