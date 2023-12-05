use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_while1};
use nom::character::complete::anychar;
use nom::character::{is_newline, is_space};
use nom::combinator::opt;
use nom::sequence::preceded;
use nom::IResult;
use phf::phf_map;
use std::{env, error::Error, fs};

// Algorithm
//   1) Have stack for blocks we call context where each block has their own stack for spans.
//   2) Scan until find a qwikmark boundary and the push/pop Block and Span content.
//      NOTE: Will need ability to read stack from bottom up to determin if Block context
//      and thus Scan stack ends.
// Thus could have root > list > quote > list > span
// Keep track of block starts, especially blocks off of root as they represent contained sections
// of isolated changes. These start points are important for long logs where only want to render
// a section of the document and know that any previous text before the start point will not
// impact current text being rendered.

// Utilize nom nested checks for both Text content expansion and edge tag building.
// For edge checks have either last was whitespace or word.
// NOTE: Either Open brackets are under whitespace and closing brackets are under word. Or
//   They do not change last status. Need to look over cases to determin.
// If last was whitespace and on edge tag then
//   1) check for current closing tag and if encountered do not start edge tag content
//   2) If word or opening bracket tag encountered as next character, start edge tag content
//   3) If another edge tag then stack until find word or opening bracket.
//
// Text is the default span/tag that joins char runs

// Document -> Block -> Paragragh(If not a block then paragraph)/Text(Collect Non-Spans) -> Span

fn main() -> Result<(), Box<dyn Error>> {
    // TODO: These need to be part of context
    // NOTE: Links cannot be nested
    // NOTE: Verbatim cannot be nested
    let args: Vec<String> = env::args().collect();
    let name = args[1].clone();
    let contents = fs::read_to_string(name)?;
    let document = qwikmark(&contents);
    println!("{:?}", document);
    Ok(())
}

#[derive(Debug, PartialEq, Eq)]
enum Block<'a> {
    Paragraph(Vec<Span<'a>>),
}

// Strong      =  { "*" }
// Emphasis    =  { "_" }
// Superscript =  { "^" }
// Subscript   =  { "~" }
// Hash        =  { "#" }
// Verbatim    =  { "`"+ }
// Highlight   =  { "=" }
// Insert      =  { "+" }
// Delete      =  { "-" }
static SPANS: phf::Map<char, &'static str> = phf_map! {
    '*' => "Strong",
    '_' => "Emphasis",
    '^' => "Superscript",
    '~' => "Subscript",
    '#' => "Hash",
    '`' => "Verbatim",
    '=' => "Highlight",
    '+' => "Insert",
    '-' => "Delete",
};
#[derive(Debug, PartialEq, Eq)]
enum Span<'a> {
    LineBreak(char),
    NBWS(char),
    Esc(char),
    Text(&'a str),
    Link(&'a str, Vec<Span<'a>>),
    Hash(&'a str),
    Verbatim(&'a str, &'a str, &'a str),
    Strong(Vec<Span<'a>>),
    Emphasis(Vec<Span<'a>>),
    Superscript(Vec<Span<'a>>),
    Subscript(Vec<Span<'a>>),
    Highlight(Vec<Span<'a>>),
    Insert(Vec<Span<'a>>),
    Delete(Vec<Span<'a>>),
    EOM,
}

// Document = { Block* ~ NEWLINE* ~ EOI }
fn qwikmark<'a>(input: &'a str) -> IResult<&'a str, Vec<Block<'a>>> {
    let mut bs = Vec::new();
    let mut input: &'a str = input;
    while let Ok((i, b)) = block(input) {
        bs.push(b);
        input = i;
    }
    Ok((input, bs))
}

// NOTE: Simplfy by allowing all tags to be Bound with square brackets ([])
// Edge        =  { (" " | "\t")+ | NEWLINE | SOI | EOI }

// Format     = { "=" ~ Field }
// Identifier = { "#" ~ Field }
// Class      = { "." ~ Field }
// Attribute  = { Format | "{" ~ " "* ~ ((Format | Identifier | Class) ~ " "*)+ ~ " "* ~ "}" }

// End       =  { NEWLINE ~ NEWLINE | EOI }
fn eom<'a>(input: &'a str) -> IResult<&'a str, Span> {
    // Input has ended
    if input == "" {
        return Ok((input, Span::EOM));
    }
    // Common block terminator has ended
    // TODO: Account for whitespace and list indentations
    let (_i, _s) = tag("\n\n")(input)?;
    Ok((input, Span::EOM))
}

// LineBreak =  { "\\" ~ &NEWLINE }
fn escaped<'a>(input: &'a str) -> IResult<&'a str, Span> {
    let (i, b) = preceded(tag("\\"), anychar)(input)?;
    if is_space(b as u8) {
        Ok((i, Span::NBWS(b)))
    } else if is_newline(b as u8) {
        Ok((i, Span::LineBreak(b)))
    } else {
        Ok((i, Span::Esc(b)))
    }
}

// UnboundTag  = _{
//     Superscript
//   | Subscript
//   | Hash
//   | Verbatim
// }

// RawText   =  { (!(PEEK | NEWLINE) ~ ANY)+ }
// Raw       =  { PUSH(Verbatim) ~ RawText ~ (POP ~ Attribute* | &End ~ DROP) }
fn verbatim<'a>(input: &'a str) -> IResult<&'a str, Span> {
    let (input, svtag) = take_while1(|b| b == '`')(input)?;
    let mut char_total_length: usize = 0;
    let mut i = input;
    while i.len() > 0 {
        if let Ok((i, evtag)) = tag::<_, &str, ()>("\n")(i) {
            let (content, _) = input.split_at(char_total_length);
            return Ok((i, Span::Verbatim(svtag, evtag, content)));
        } else if let Ok((ti, evtag)) = take_while1::<_, &str, ()>(|b| b == '`')(i) {
            if svtag == evtag {
                let (content, _) = input.split_at(char_total_length);
                // NOTE: May want to strip whitespace around enclosing backticks:
                //  `` `verbatim` `` -> <code>`verbatim`</code>
                let content_trimmed = content.trim();
                if content_trimmed.starts_with('`') && content_trimmed.ends_with('`') {
                    return Ok((ti, Span::Verbatim(svtag, evtag, content_trimmed)));
                }
                return Ok((ti, Span::Verbatim(svtag, evtag, content)));
            }
            i = ti;
            char_total_length += evtag.len();
        } else {
            let char_length = i.chars().next().unwrap().len_utf8();
            (_, i) = i.split_at(char_length);
            char_total_length += char_length;
        }
    }
    let (content, _) = input.split_at(char_total_length);
    Ok((i, Span::Verbatim(svtag, i, content)))
}

//{=format #identifier .class key=value key="value" %comment%}
// Field      = { ASCII_ALPHA ~ (ASCII_ALPHANUMERIC | "_")* }
fn field(input: &str) -> IResult<&str, &str> {
    is_not(" \t\r\n]")(input)
}

// HashTag   =  { Edge ~ Hash ~ Location }
fn hash<'a>(input: &'a str) -> IResult<&'a str, Span> {
    let (i, h) = preceded(tag("#"), field)(input)?;
    Ok((i, Span::Hash(h)))
}

// brackettag  = _{
//     edgetag    // strong(*), emphasis(_)
//   | highlight  // (=)
//   | insert     // (+)
//   | delete     // (-)
// }
// NOTE: Added for consistency the Superscript and Subscript
//   Span types to bracket tags for consistency and versatility.
fn bracket(input: &str) -> IResult<&str, Span> {
    let (i, t) = preceded(tag("["), is_a("*_=+-^~"))(input)?;
    let closing_tag = t.to_string() + "]";
    let (i, ss) = spans(i, Some(&closing_tag))?;
    match t {
        "*" => Ok((i, Span::Strong(ss))),
        "_" => Ok((i, Span::Emphasis(ss))),
        "=" => Ok((i, Span::Highlight(ss))),
        "+" => Ok((i, Span::Insert(ss))),
        "-" => Ok((i, Span::Delete(ss))),
        "^" => Ok((i, Span::Superscript(ss))),
        "~" => Ok((i, Span::Subscript(ss))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alt,
        ))),
    }
}

// edgetag     = _{
//     strong
//   | emphasis
// }

// LinkDlmr  = _{ "|" | &("]" | NEWLINE | EOI) }
// Locator   =  { (("\\" | !LinkDlmr) ~ ANY)+ }
fn locator(input: &str) -> IResult<&str, &str> {
    let (i, l) = is_not("|]")(input)?;
    let (i, _) = opt(tag("|"))(i)?;
    Ok((i, l))
}

// Link      =  { "[[" ~ Locator ~ LinkDlmr? ~ (!"]]" ~ (Span | Char))* ~ ("]]" ~ Attribute* | &End) }
fn link<'a>(input: &'a str) -> IResult<&'a str, Span> {
    let (i, l) = preceded(tag("[["), locator)(input)?;
    let (i, ss) = spans(i, Some("]]"))?;
    Ok((i, Span::Link(l, ss)))
}

// Char      =  { !NEWLINE ~ "\\"? ~ ANY }
// Span      =  {
//     Break
//   | Raw
//   | HashTag
//   | Link
//   | "[" ~ PUSH(BracketTag) ~ (!(PEEK ~ "]") ~ (Span | Char))+ ~ (POP ~ "]" ~ Attribute* | &End ~ DROP)
//   | PUSH(UnboundTag) ~ (!PEEK ~ (Span | Char))+ ~ (POP | &End ~ DROP)
//   | Edge ~ PUSH(EdgeTag) ~ (!(PEEK ~ Edge) ~ (Span | Char))+ ~ (POP ~ &Edge | &End ~ DROP)
//   | Char
//   | NEWLINE ~ !NEWLINE
// }
fn spans<'a, 'b>(input: &'a str, closer: Option<&'b str>) -> IResult<&'a str, Vec<Span<'a>>> {
    let mut ss = Vec::new();
    let mut i = input;
    // Loop through text until reach two newlines
    // or in future matching valid list item.
    // Automatically collect breaks and escaped char
    // Also turn escaped spaces into non-breaking spaces
    let mut text_start = input;
    let mut char_total_length: usize = 0;
    let mut trim_closer = false;
    while i != "" {
        // println!("input: {:?}", i);
        if let Some(closer) = closer {
            if i.starts_with(closer) {
                trim_closer = true;
                break;
            }
        }
        if let Ok((input, s)) = alt((eom, escaped, verbatim, hash, link, bracket))(i) {
            if char_total_length > 0 {
                let (text, _) = text_start.split_at(char_total_length);
                ss.push(Span::Text(text));
                text_start = input;
                char_total_length = 0;
            }
            i = input;
            // End of Mark (EOM) Indicates a common ending point
            // such as an end to a block such as a paragraph or
            // that the file input as ended.
            if s == Span::EOM {
                break;
            }

            ss.push(s);
        } else {
            let char_length = i.chars().next().unwrap().len_utf8();
            (_, i) = i.split_at(char_length);
            char_total_length += char_length;
        }
    }
    if char_total_length > 0 {
        let (text, i) = text_start.split_at(char_total_length);
        ss.push(Span::Text(text));
        text_start = i;
    }
    if trim_closer {
        if let Some(closer) = closer {
            (_, text_start) = text_start.split_at(closer.len());
        }
    }
    Ok((text_start, ss))
}

// LineHash = { Edge ~ Hash ~ Location }
// LineChar = { !("|" | NEWLINE) ~ "\\"? ~ ANY }
// LineEnd  = { "|" | NEWLINE | EOI }
// LinkLine = { "[[" ~ Location ~ "|"? ~ (!"]]" ~ (Line | LineChar))+ ~ ("]]" ~ Attribute* | &LineEnd) }
// Line = {
//     Raw
//   | LineHash
//   | Link
//   | "[" ~ PUSH(BracketTag) ~ (!(PEEK ~ "]") ~ (Line | LineChar))+ ~ (POP ~ "]" ~ Attribute* | &LineEnd ~ DROP)
//   | PUSH(UnboundTag) ~ (!PEEK ~ (Line | LineChar))+ ~ (POP | &(LineEnd ~ DROP))
//   | Edge ~ PUSH(EdgeTag) ~ (!(PEEK ~ Edge) ~ (Line | LineChar))+ ~ (POP ~ &Edge | &LineEnd ~ DROP)
//   | LineChar
// }

// Div = {
//   (NEWLINE+ | SOI) ~ ":::" ~ Attribute* ~ " " ~ Field ~ &(NEWLINE | EOI) ~
//   (!(NEWLINE+ ~ ":::") ~ Block)* ~
//   NEWLINE+ ~ (":::" ~ &(NEWLINE | EOI) | EOI)
// }

// CodeStart = { (NEWLINE+ | SOI) ~ PUSH("`"{3, 6}) ~ Attribute* }
// CodeText  = { NEWLINE ~ (!NEWLINE ~ ANY)* }
// CodeStop  = { NEWLINE ~ POP }
// Code      = { CodeStart ~ (!CodeStop ~ CodeText)* ~ (CodeStop | &(NEWLINE | EOI)) }

// RomanLower = { "i" | "v" | "x" | "l" | "c" | "d" | "m" }
// RomanUpper = { "I" | "V" | "X" | "L" | "C" | "D" | "M" }
// Definition = { ": " ~ Field }
// Unordered  = { "-" | "+" | "*" }
// Ordered    = { (ASCII_DIGIT+ | RomanLower+ | RomanUpper+ | ASCII_ALPHA_LOWER+ | ASCII_ALPHA_UPPER+) ~ ("." | ")") }
// ListHead   = { ((NEWLINE+ | SOI) ~ PEEK[..] ~ (Unordered | Ordered | Definition) ~ (" " | NEWLINE) ~ ListItem)+ }
// ListItem   = { Span+ ~ ListBlock* }
// ListBlock  = {
//   NEWLINE+ ~
//   PEEK[..] ~ PUSH((" " | "\t")+) ~ (Unordered | Ordered | Definition) ~ (" " | NEWLINE) ~ ListItem ~
//   (PEEK[..] ~ (Unordered | Ordered) ~ " " ~ ListItem)* ~
//   DROP
// }

// H1      = { "#" }
// H2      = { "##" }
// H3      = { "###" }
// H4      = { "####" }
// H5      = { "#####" }
// H6      = { "######" }
// Heading = { (NEWLINE+ | SOI) ~ (H6 | H5 | H4 | H3 | H2 | H1) ~ (" " | "\t")+ ~ Location ~ ((LinkDlmr ~ Span+)? ~ &(NEWLINE | EOI)) }

// CellEnd      = _{ "|" | &(NEWLINE | EOI) }
// Cell         =  { "|" ~ Line+ }
// Row          =  { Cell+ ~ CellEnd }
// AlignRight   =  { "-"+ ~ ":" }
// AlignDefault =  { "-"+ }
// AlignCenter  =  { ":" ~ "-"+ ~ ":" }
// AlignLeft    =  { ":" ~ "-"+ }
// Layout       =  { ("|" ~ " "* ~ (AlignRight | AlignDefault | AlignCenter | AlignLeft) ~ " "*)+ ~ CellEnd }
// Table        =  {
//   (NEWLINE+ | SOI) ~ Row ~
//   NEWLINE ~ Layout ~
//   (NEWLINE ~ Row)*
// }

// Paragraph = { (NEWLINE+ | SOI) ~ Span+ ~ &(NEWLINE | EOI) }
fn paragraph<'a>(input: &'a str) -> IResult<&'a str, Block<'a>> {
    let (i, ss) = spans(input, None)?;
    Ok((i, Block::Paragraph(ss)))
}

// Block = {
//     Div
//   | Code
//   | ListHead
//   | Heading
//   | Table
//   | Paragraph
// }
fn block<'a>(input: &'a str) -> IResult<&'a str, Block<'a>> {
    // TODO: Or each block type with Paragraph as last default type.
    paragraph(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_block_paragraph_text_line_break() {
        assert_eq!(
            block("line\\\n"),
            Ok((
                "",
                Block::Paragraph(vec![Span::Text("line"), Span::LineBreak('\n')])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_nbsp() {
        assert_eq!(
            block("left\\\tright"),
            Ok((
                "",
                Block::Paragraph(vec![
                    Span::Text("left"),
                    Span::NBWS('\t'),
                    Span::Text("right")
                ])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_text_verbatim_text() {
        assert_eq!(
            block("left ``verbatim`` right"),
            Ok((
                "",
                Block::Paragraph(vec![
                    Span::Text("left "),
                    Span::Verbatim("``", "``", "verbatim"),
                    Span::Text(" right")
                ])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_text_verbatim_newline() {
        assert_eq!(
            block("left ``verbatim\n right"),
            Ok((
                "",
                Block::Paragraph(vec![
                    Span::Text("left "),
                    Span::Verbatim("``", "\n", "verbatim"),
                    Span::Text(" right")
                ])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_text_verbatim_with_nonmatching_backtick() {
        assert_eq!(
            block("left ``ver```batim`` right"),
            Ok((
                "",
                Block::Paragraph(vec![
                    Span::Text("left "),
                    Span::Verbatim("``", "``", "ver```batim"),
                    Span::Text(" right")
                ])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_text_verbatim_with_enclosing_backtick() {
        assert_eq!(
            block("left `` `verbatim` `` right"),
            Ok((
                "",
                Block::Paragraph(vec![
                    Span::Text("left "),
                    Span::Verbatim("``", "``", "`verbatim`"),
                    Span::Text(" right")
                ])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_hash_empty_eom() {
        assert_eq!(
            block("left #"),
            Ok(("", Block::Paragraph(vec![Span::Text("left #")])))
        );
    }

    #[test]
    fn test_block_paragraph_hash_empty_space() {
        assert_eq!(
            block("left # "),
            Ok(("", Block::Paragraph(vec![Span::Text("left # ")])))
        );
    }

    #[test]
    fn test_block_paragraph_hash_field_eom() {
        assert_eq!(
            block("left #hash"),
            Ok((
                "",
                Block::Paragraph(vec![Span::Text("left "), Span::Hash("hash")])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_hash_field_newline() {
        assert_eq!(
            block("left #hash\nnext line"),
            Ok((
                "",
                Block::Paragraph(vec![
                    Span::Text("left "),
                    Span::Hash("hash"),
                    Span::Text("\nnext line")
                ])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_link_location() {
        assert_eq!(
            block("left [[loc]]"),
            Ok((
                "",
                Block::Paragraph(vec![Span::Text("left "), Span::Link("loc", vec![])])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_link_with_location_and_text() {
        assert_eq!(
            block("left [[loc|text]] right"),
            Ok((
                "",
                Block::Paragraph(vec![
                    Span::Text("left "),
                    Span::Link("loc", vec![Span::Text("text")]),
                    Span::Text(" right")
                ])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_link_with_location_and_span() {
        assert_eq!(
            block("left [[loc|text `verbatim`]] right"),
            Ok((
                "",
                Block::Paragraph(vec![
                    Span::Text("left "),
                    Span::Link(
                        "loc",
                        vec![Span::Text("text "), Span::Verbatim("`", "`", "verbatim")]
                    ),
                    Span::Text(" right")
                ])
            ))
        );
    }

    #[test]
    fn test_block_paragraph_nested_spans() {
        assert_eq!(
            block("text-left [*strong-left [_emphasis-center_]\t[+insert-left [^superscript-center^] insert-right+] strong-right*] text-right"),
            Ok((
                "",
                Block::Paragraph(vec![
                  Span::Text("text-left "),
                  Span::Strong(vec![
                    Span::Text("strong-left "),
                    Span::Emphasis(vec![
                      Span::Text("emphasis-center")
                    ]),
                    Span::Text("\t"),
                    Span::Insert(vec![
                      Span::Text("insert-left "),
                      Span::Superscript(vec![
                        Span::Text("superscript-center")
                      ]),
                      Span::Text(" insert-right")
                    ]),
                    Span::Text(" strong-right")
                  ]),
                  Span::Text(" text-right")
                ])
            ))
        );
    }
}
