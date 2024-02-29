use indexmap::IndexMap;
use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_while1, take_while_m_n};
use nom::character::complete::{
    alpha1, anychar, char, digit1, line_ending, multispace1, not_line_ending, space0, space1,
};
use nom::combinator::{cond, consumed, eof, not, opt, peek, value};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, preceded, separated_pair, terminated, tuple};
use nom::IResult;
use phf::phf_map;
use std::error::Error;
use std::fmt;

// TODO: create composible block type/struct that can be used by heading, list block,
// and div so they can utilizen hash tags and filters produced by spans. Note
// list will group hash filters in different structural orders.
// Headings will have a embedded link feature and indexing on top of hash structures.

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
// Block::Paragraph is the default Block that captures text in te form of Spans
// Span::Text is the default span/tag that joins char runs

// ATTRIBUTES

// Format     = { "=" ~ Field }
// Identifier = { "#" ~ Field }
// Class      = { "." ~ Field }
// Attribute  = { Format | "{" ~ " "* ~ ((Format | Identifier | Class) ~ " "*)+ ~ " "* ~ "}" }
fn key(input: &str) -> IResult<&str, &str> {
    // let (input, (k, _)) = consumed(tuple((alpha1, many0(alt((alphanumeric0, is_a("_")))))))(input)?;
    is_not("= }\t\n\r")(input)
}
fn esc_value(input: &str) -> IResult<&str, &str> {
    let (input, (es, _)) = consumed(preceded(tag("\\"), anychar))(input)?;
    Ok((input, es))
}
fn key_value(input: &str) -> IResult<&str, &str> {
    // is_not(" }\t\n\r")(input)
    let (input, (v, _)) = consumed(many1(alt((esc_value, is_not(" }\t\n\r\\")))))(input)?;
    Ok((input, v))
}
fn attributes(input: &str) -> IResult<&str, IndexMap<&str, &str>> {
    let (input, kvs) = delimited(
        tuple((tag("{"), space0)),
        separated_list1(
            tuple((opt(line_ending), space1)),
            separated_pair(key, tag("="), key_value),
        ),
        tuple((space0, tag("}"))),
    )(input)?;
    let mut h = IndexMap::new();
    for (k, v) in kvs {
        // For security reasons only add first value of a key found.
        h.entry(k).or_insert(v);
    }
    Ok((input, h))
}

// SPANS

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum HashOp {
    NotEqual,
    Equal,
    GreaterThan,
    LessThan,
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Span<'a> {
    LineBreak(&'a str),
    NBWS(&'a str),
    Esc(&'a str),
    Text(&'a str),
    Hash(Option<HashOp>, &'a str),
    EOM,
    // Tags with Attributes
    Link(
        &'a str,
        bool,
        Vec<Span<'a>>,
        Option<IndexMap<&'a str, &'a str>>,
    ),
    Verbatim(&'a str, Option<&'a str>, Option<IndexMap<&'a str, &'a str>>),
    Strong(Vec<Span<'a>>, Option<IndexMap<&'a str, &'a str>>),
    Emphasis(Vec<Span<'a>>, Option<IndexMap<&'a str, &'a str>>),
    Superscript(Vec<Span<'a>>, Option<IndexMap<&'a str, &'a str>>),
    Subscript(Vec<Span<'a>>, Option<IndexMap<&'a str, &'a str>>),
    Highlight(Vec<Span<'a>>, Option<IndexMap<&'a str, &'a str>>),
    Insert(Vec<Span<'a>>, Option<IndexMap<&'a str, &'a str>>),
    Delete(Vec<Span<'a>>, Option<IndexMap<&'a str, &'a str>>),
}

fn span_with_attributes<'a>(span: Span<'a>, kvs: IndexMap<&'a str, &'a str>) -> Span<'a> {
    match span {
        // Tags with Attributes
        Span::Link(loc, e, ss, _) => Span::Link(loc, e, ss, Some(kvs)),
        Span::Verbatim(text, format, _) => Span::Verbatim(text, format, Some(kvs)),
        Span::Strong(ss, _) => Span::Strong(ss, Some(kvs)),
        Span::Emphasis(ss, _) => Span::Emphasis(ss, Some(kvs)),
        Span::Superscript(ss, _) => Span::Superscript(ss, Some(kvs)),
        Span::Subscript(ss, _) => Span::Subscript(ss, Some(kvs)),
        Span::Highlight(ss, _) => Span::Highlight(ss, Some(kvs)),
        Span::Insert(ss, _) => Span::Insert(ss, Some(kvs)),
        Span::Delete(ss, _) => Span::Delete(ss, Some(kvs)),
        Span::LineBreak(_)
        | Span::NBWS(_)
        | Span::Esc(_)
        | Span::Text(_)
        | Span::Hash(_, _)
        | Span::EOM => span,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct HashFilter {
    index: String,
    op: HashOp,
    tags: Vec<HashTag>,
}

//#[derive(Debug, PartialEq, Eq)]
//pub enum HashCmp {
//    And(Vec<HashCmp>),
//    Or(Vec<HashCmp>),
//    Cmp(Box<HashFilter>),
//}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub enum HashTag {
    // All adjacent non alphanumerical content
    // will be turned into a space which will
    // be represented in the hashtag as a single
    // dash "-" character
    Space,
    // All adjacent alpha content will be concatenated
    // into a string value
    Str(String),
    // All numerical data will be turnined into a
    // signed integer and represented a string of
    // numerical characters preceded with a single
    // dash "-" if negative. This will be the only
    // time that two dashes "-" will be adjacent.
    // Decimal points are turned into a space,
    // leaving two separate Numbers and loss of
    // their association.
    Num(isize),
}

impl fmt::Display for HashTag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Space => write!(f, "-"),
            Self::Str(s) => write!(f, "{}", &s),
            Self::Num(n) => write!(f, "{n}"),
        }
    }
}

fn number(input: &str) -> IResult<&str, &str> {
    let (i, (c, _)) = consumed(tuple((opt(alt((tag("-"), tag("+")))), digit1)))(input)?;
    Ok((i, c))
}

fn single_non_number(input: &str) -> IResult<&str, &str> {
    let (i, (c, _)) = consumed(anychar)(input)?;
    Ok((i, c))
}

fn filter_out_numbers(input: &str) -> IResult<&str, Vec<&str>> {
    let (i, v) = many0(preceded(opt(number), single_non_number))(input)?;
    return Ok((i, v));
}

// Contents concatenates the text within the nested vectors of spans
pub fn contents<'a>(outer: &Vec<Span<'a>>, label: bool) -> Vec<&'a str> {
    outer
        .iter()
        .fold(vec![], |mut unrolled, result| -> Vec<&'a str> {
            let rs = match result {
                Span::Text(t) | Span::Verbatim(t, _, _) => vec![*t],
                Span::Hash(_, t) => {
                    // Filter out digits with associated negative sign
                    if let (true, Ok((_, vs))) = (label, filter_out_numbers(t)) {
                        vs
                    } else {
                        vec![]
                    }
                }
                Span::Link(s, _, vs, _) => {
                    if vs.is_empty() {
                        vec![*s]
                    } else {
                        contents(&vs, label)
                    }
                }
                Span::Strong(vs, _)
                | Span::Emphasis(vs, _)
                | Span::Superscript(vs, _)
                | Span::Subscript(vs, _)
                | Span::Highlight(vs, _)
                | Span::Insert(vs, _)
                | Span::Delete(vs, _) => contents(&vs, label),
                Span::LineBreak(s) | Span::NBWS(s) | Span::Esc(s) => vec![*s],
                Span::EOM => vec![],
            };
            unrolled.extend(rs);
            unrolled
        })
}

// Hash tag labels generates indexes of hash tags minus any span formatting,
// nor numerical values, so that ranges of hashtags may be compared.
pub fn htlabel(hts: &Vec<HashTag>) -> String {
    let mut s = String::new();
    let mut space = false;
    for ht in hts {
        match ht {
            HashTag::Space => space = true,
            HashTag::Str(a) => {
                if space {
                    s += "-";
                    space = false;
                }
                s += &a.to_lowercase();
            }
            HashTag::Num(_) => (),
        }
    }
    // For empty lables that only contain numerical values
    if s.is_empty() {
        return "-".to_string();
    }
    s
}

// span label generates indexes used to compare content minus any span formatting.
pub fn span_label(hts: &Vec<HashTag>) -> String {
    let mut s = String::new();
    for ht in hts {
        match ht {
            HashTag::Space => s += "-",
            HashTag::Str(a) => s += &a.to_lowercase(),
            HashTag::Num(n) => s += &n.to_string(),
        }
    }
    if s.is_empty() {
        return "-".to_string();
    }
    s
}

// NOTE: Dash is excluded and must be checked separately,
// as it may be treated either as spacing or a negative number.
// Same for plus sign.
// Similar situation found with period as decimal,
// colen as a ratio, and forward slash as a fraction
// carrot and double asterisk as an exponential.
// TODO: add special quotations and hyphens
pub fn punctuation(input: &str) -> IResult<&str, &str> {
    is_a(r#"_~`'"\,;!¡?¿"#)(input)
}

// Tags turns contents into hash tags type that may be used for ordering
// anchor strings for referencing/indexing, or filtering.
pub fn hashtags(input: Vec<&str>) -> Vec<HashTag> {
    let (mut hts, ht) = input
        .iter()
        .fold((Vec::new(), None), |(mut hts, mut ht), c| {
            let mut i = *c;
            while !i.is_empty() {
                if let Ok((c, (cd, (n, d)))) = consumed(tuple((
                    opt(alt((
                        tag::<&str, &str, ()>("-"),
                        tag::<&str, &str, ()>("+"),
                    ))),
                    digit1,
                )))(i)
                {
                    let (d, dash_is_space) = if let Some(HashTag::Str(_)) = ht {
                        (d, true)
                    } else {
                        (cd, false)
                    };
                    if let Ok(d) = d.parse::<isize>() {
                        if let Some(oht) = ht {
                            if let HashTag::Str(s) = oht {
                                hts.push(HashTag::Str(s.to_lowercase()));
                            } else {
                                hts.push(oht);
                            }
                            if let (true, Some(_)) = (dash_is_space, n) {
                                hts.push(HashTag::Space);
                            }
                            ht = None;
                        } else if let (Some(_), 0) = (n, hts.len()) {
                            hts.push(HashTag::Space);
                        }
                        hts.push(HashTag::Num(d));
                    } else {
                        match ht {
                            Some(HashTag::Str(nas)) => {
                                ht = Some(HashTag::Str(nas + cd));
                            }
                            Some(HashTag::Space) => {
                                hts.push(HashTag::Space);
                                ht = Some(HashTag::Str(cd.to_string()));
                            }
                            Some(HashTag::Num(_)) | None => {
                                ht = Some(HashTag::Str(cd.to_string()));
                            }
                        }
                    }
                    i = c;
                } else if let Ok((c, a)) = alpha1::<&str, ()>(i) {
                    match ht {
                        Some(HashTag::Str(nas)) => {
                            ht = Some(HashTag::Str(nas + a));
                        }
                        Some(HashTag::Space) => {
                            hts.push(HashTag::Space);
                            ht = Some(HashTag::Str(a.to_string()));
                        }
                        Some(HashTag::Num(_)) | None => {
                            ht = Some(HashTag::Str(a.to_string()));
                        }
                    }
                    i = c;
                } else if let Ok((c, _s)) = alt((multispace1, value(" ", punctuation)))(i) {
                    match ht {
                        Some(HashTag::Str(s)) => {
                            hts.push(HashTag::Str(s.to_lowercase()));
                            ht = Some(HashTag::Space);
                        }
                        Some(HashTag::Space) => {}
                        Some(HashTag::Num(_)) | None => {
                            ht = Some(HashTag::Space);
                        }
                    }
                    i = c;
                } else if let Ok((c, (a, _))) = consumed(anychar::<&str, ()>)(i) {
                    if a == "-" || a == "+" {
                        match ht {
                            Some(HashTag::Str(s)) => {
                                hts.push(HashTag::Str(s.to_lowercase()));
                                ht = Some(HashTag::Space);
                            }
                            Some(HashTag::Space) => {}
                            Some(HashTag::Num(_)) | None => {
                                ht = Some(HashTag::Space);
                            }
                        }
                    } else {
                        match ht {
                            Some(HashTag::Str(nas)) => {
                                ht = Some(HashTag::Str(nas + a));
                            }
                            Some(HashTag::Space) => {
                                hts.push(HashTag::Space);
                                ht = Some(HashTag::Str(a.to_string()));
                            }
                            Some(HashTag::Num(_)) | None => {
                                ht = Some(HashTag::Str(a.to_string()));
                            }
                        }
                    }
                    i = c;
                }
            }
            (hts, ht)
        });
    // Do NOT push last space, only remain string
    if let Some(HashTag::Str(s)) = ht {
        hts.push(HashTag::Str(s.to_lowercase()));
    }
    hts
}

fn hash_field(input: &str) -> IResult<&str, &str> {
    let (input, (v, _)) = consumed(tuple((is_not(" \t\n\r]#"), opt(is_not("\t\r\n]#")))))(input)?;
    // NOTE: We may want to add any trailing spaces that where trimmed  back to input
    // as that could end up joining  words together that where seperated by closing tags.
    let v = v.trim();
    Ok((input, v))
}

//{=format #identifier .class key=value key="value" %comment%}
// Field      = { ASCII_ALPHA ~ (ASCII_ALPHANUMERIC | "_")* }
fn field(input: &str) -> IResult<&str, &str> {
    is_not(" \t\r\n]")(input)
}

// Edge        =  { (" " | "\t")+ | NEWLINE | SOI | EOI }
// edgetag     = _{
//     strong
//   | emphasis
// }
fn at_boundary_end<'a>(closer: &'a str, input: &'a str) -> IResult<&'a str, &'a str> {
    terminated(
        tag(closer),
        alt((
            tag(" "),
            tag("\t"),
            tag("\n"),
            tag("\r"),
            tag("*"),
            tag("_"),
            tag("=]"),
            tag("+]"),
            tag("-]"),
            tag("^"),
            tag("~"),
            tag("]]"),
            tag("{"),
        )),
    )(input)
}

// LinkDlmr  = _{ "|" | &("]" | NEWLINE | EOI) }
// Locator   =  { (("\\" | !LinkDlmr) ~ ANY)+ }
fn locator(input: &str) -> IResult<&str, &str> {
    let (i, l) = is_not("|]")(input)?;
    let (i, _) = opt(tag("|"))(i)?;
    Ok((i, l))
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct SpanRefs {
    tags: Option<IndexMap<String, Vec<HashTag>>>,
    filters: Option<Vec<HashFilter>>,
    embeds: Option<Vec<(String, String)>>,
}

impl SpanRefs {
    // End       =  { NEWLINE ~ NEWLINE | EOI }
    fn eom<'a, 'b>(&'a mut self, input: &'b str) -> IResult<&'b str, Span<'b>> {
        // Input has ended
        if input.is_empty() {
            return Ok((input, Span::EOM));
        }
        // Common block terminator has ended
        // TODO: Account for whitespace and list indentations
        let (_i, _s) = tuple((line_ending, line_ending))(input)?;
        Ok((input, Span::EOM))
    }

    // LineBreak =  { "\\" ~ &NEWLINE }
    fn esc<'a, 'b>(&'a mut self, input: &'b str) -> IResult<&'b str, Span<'b>> {
        let (i, e) = preceded(
            tag("\\"),
            alt((tag(" "), line_ending, take_while_m_n(1, 1, |c| c != ' '))),
        )(input)?;
        if e == " " {
            Ok((i, Span::NBWS(e)))
        } else if line_ending::<_, ()>(e).is_ok() {
            Ok((i, Span::LineBreak(e)))
        } else {
            Ok((i, Span::Esc(e)))
        }
    }

    // HashTag   =  { Edge ~ Hash ~ Location }
    fn hash<'a, 'b>(&'a mut self, input: &'b str) -> IResult<&'b str, Span<'b>> {
        let (i, (filter, h)) = tuple((opt(is_a("!<>=")), preceded(tag("#"), hash_field)))(input)?;
        filter.map_or(Ok((i, Span::Hash(None, h))), move |f| match f {
            "!" => Ok((i, Span::Hash(Some(HashOp::NotEqual), h))),
            "<" => Ok((i, Span::Hash(Some(HashOp::LessThan), h))),
            "=" => Ok((i, Span::Hash(Some(HashOp::Equal), h))),
            ">" => Ok((i, Span::Hash(Some(HashOp::GreaterThan), h))),
            _ => Ok((i, Span::Hash(None, h))),
        })
    }

    // UnboundTag  = _{
    //     Superscript
    //   | Subscript
    //   | Hash
    //   | Verbatim
    // }
    // NOTE: Hash and Verbatim where handles separately.
    fn nobracket<'a, 'b>(&'a mut self, input: &'b str) -> IResult<&'b str, Span<'b>> {
        let (i, t) = alt((tag("^"), tag("~")))(input)?;
        let (i, ss) = self.spans(i, Some(t), None);
        match t {
            "^" => Ok((i, Span::Superscript(ss, None))),
            "~" => Ok((i, Span::Subscript(ss, None))),
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Alt,
            ))),
        }
    }

    // RawText   =  { (!(PEEK | NEWLINE) ~ ANY)+ }
    // Raw       =  { PUSH(Verbatim) ~ RawText ~ (POP ~ Attribute* | &End ~ DROP) }
    fn verbatim<'a, 'b>(&'a mut self, input: &'b str) -> IResult<&'b str, Span<'b>> {
        let (input, svtag) = take_while1(|b| b == '`')(input)?;
        let mut char_total_length: usize = 0;
        let mut i = input;
        while !i.is_empty() {
            if let Ok((i, _evtag)) = tag::<_, &str, ()>("\n")(i) {
                let (content, _) = input.split_at(char_total_length);
                return Ok((i, Span::Verbatim(content, None, None)));
            } else if let Ok((ti, evtag)) = take_while1::<_, &str, ()>(|b| b == '`')(i) {
                if svtag == evtag {
                    let (content, _) = input.split_at(char_total_length);
                    // NOTE: May want to strip whitespace around enclosing backticks:
                    //  `` `verbatim` `` -> <code>`verbatim`</code>
                    let content_trimmed = content.trim();
                    if content_trimmed.starts_with('`') && content_trimmed.ends_with('`') {
                        return Ok((ti, Span::Verbatim(content_trimmed, None, None)));
                    }
                    return Ok((ti, Span::Verbatim(content, None, None)));
                }
                i = ti;
                char_total_length += evtag.len();
            } else if let Some(c) = i.chars().next() {
                let char_length = c.len_utf8();
                (_, i) = i.split_at(char_length);
                char_total_length += char_length;
            }
        }
        let (content, _) = input.split_at(char_total_length);
        Ok((i, Span::Verbatim(content, None, None)))
    }

    // brackettag  = _{
    //     edgetag    // strong(*), emphasis(_)
    //   | highlight  // (=)
    //   | insert     // (+)
    //   | delete     // (-)
    // }
    // NOTE: Added for consistency the Superscript and Subscript
    //   Span types to bracket tags for consistency and versatility.
    fn bracket<'a, 'b>(&'a mut self, input: &'b str) -> IResult<&'b str, Span<'b>> {
        let (i, t) = preceded(tag("["), is_a("*_=+-^~"))(input)?;
        let closing_tag = t.to_string() + "]";
        let (i, ss) = self.spans(i, Some(&closing_tag), None);
        match t {
            "*" => Ok((i, Span::Strong(ss, None))),
            "_" => Ok((i, Span::Emphasis(ss, None))),
            "=" => Ok((i, Span::Highlight(ss, None))),
            "+" => Ok((i, Span::Insert(ss, None))),
            "-" => Ok((i, Span::Delete(ss, None))),
            "^" => Ok((i, Span::Superscript(ss, None))),
            "~" => Ok((i, Span::Subscript(ss, None))),
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Alt,
            ))),
        }
    }

    fn edge<'a, 'b>(&'a mut self, input: &'b str) -> IResult<&'b str, Span<'b>> {
        let (i, t) = alt((tag("*"), tag("_")))(input)?;
        let _ = not(alt((tag(" "), tag("\n"), tag("\t"), tag("\r"))))(i)?;
        let (i, ss) = self.spans(i, Some(t), None);
        match t {
            "*" => Ok((i, Span::Strong(ss, None))),
            "_" => Ok((i, Span::Emphasis(ss, None))),
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Alt,
            ))),
        }
    }

    // Link      =  { "[[" ~ Locator ~ LinkDlmr? ~ (!"]]" ~ (Span | Char))* ~ ("]]" ~ Attribute* | &End) }
    fn link<'a, 'b>(&'a mut self, input: &'b str) -> IResult<&'b str, Span<'b>> {
        let (i, (e, l)) = tuple((opt(tag("!")), preceded(tag("[["), locator)))(input)?;
        let embed = e.is_some();
        let (i, ss) = self.spans(i, Some("]]"), None);
        if embed {
            let label = span_label(&hashtags(contents(&ss, false)));
            let link = l.to_string();
            if let Some(es) = &mut self.embeds {
                es.push((label, link));
            } else {
                self.embeds = Some(vec![(label, link)])
            }
        }
        Ok((i, Span::Link(l, embed, ss, None)))
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
    // TODO: Look at turn spans function signature
    //   from (input: &str, closer: Option<&str>)
    //     to (input: &str, closer: &str)
    //   where starting closer as "" happens to also be the same as eof/eom
    fn spans<'a, 'b>(
        &'b mut self,
        input: &'a str,
        closer: Option<&str>,
        inlist: Option<bool>,
    ) -> (
        // Unconsumed input
        &'a str,
        // Constructed spans
        Vec<Span<'a>>,
        // Heading associated hashtags
        //Option<IndexMap<String, Vec<HashTag>>>,
        // Heading associated hash filters
        //Option<HashFilter>,
        // Heading embedded links
        //Option<Vec<Span<'a>>>,
    ) {
        let mut i = input;
        let mut ss = Vec::new();
        //let mut hash_tags: IndexMap<String, Vec<HashTag>> = IndexMap::new();
        //let mut hash_filters: Vec<(String, HashOp, Vec<HashTag>)> = Vec::new();
        //let mut embedded_links: Option<Span<'a>> = None;
        // Loop through text until reach two newlines
        // or newline matches valid start of a list item.
        let mut boundary = true;
        let mut text_start = input;
        let mut char_total_length: usize = 0;
        let mut trim_closer = false;
        while !i.is_empty() {
            // println!(
            //     "input: {:?}\n  boundary: {:?}\n  closer: {:?}\n  text_start: {:?}",
            //     i, boundary, closer, text_start
            // );
            // if we just started or next char is boundary
            if let Some(closer) = closer {
                if i.starts_with(closer) {
                    if !boundary && (closer == "*" || closer == "_") {
                        if at_boundary_end(closer, i).is_ok() {
                            trim_closer = true;
                            break;
                        }
                    } else {
                        trim_closer = true;
                        break;
                    }
                }
            }
            // Escape loop if notice any list starting tags
            if let Some(new_list) = inlist {
                if let Ok((_, true)) = is_list_singleline_tag(new_list, i) {
                    break;
                }
            }
            // Automatically collect breaks and escaped char
            // and turn escaped spaces into non-breaking spaces
            // before checking for qwikmark tags
            let (input, s) = if let (true, Ok((input, s))) = (boundary, self.edge(i)) {
                (input, Some(s))
            } else if let Ok((input, s)) = self.eom(i) {
                (input, Some(s))
            } else if let Ok((input, s)) = self.esc(i) {
                (input, Some(s))
            } else if let Ok((input, s)) = self.hash(i) {
                (input, Some(s))
            } else if let Ok((input, s)) = self.verbatim(i) {
                (input, Some(s))
            } else if let Ok((input, s)) = self.link(i) {
                (input, Some(s))
            } else if let Ok((input, s)) = self.bracket(i) {
                (input, Some(s))
            } else if let Ok((input, s)) = self.nobracket(i) {
                (input, Some(s))
            } else {
                (i, None)
            };
            if let Some(s) = s {
                boundary = false;
                if char_total_length > 0 {
                    let (text, _) = text_start.split_at(char_total_length);
                    ss.push(Span::Text(text));
                    char_total_length = 0;
                }
                text_start = input;
                i = input;

                // End of Mark (EOM) Indicates a common ending point
                // such as an end to a block such as a paragraph or
                // that the file input as ended.
                match s {
                    Span::EOM => break,
                    Span::Esc(_) => ss.push(s),
                    // Bundle hash tag/filter to return to associated heading.
                    Span::Hash(op, ht) => {
                        ss.push(s);
                        let tags = hashtags(vec![ht]);
                        let label = htlabel(&tags);
                        if let Some(op) = op {
                            let hf = HashFilter {
                                index: label,
                                op,
                                tags,
                            };
                            if self.filters.is_some() {
                                self.filters.as_mut().map(move |v| v.push(hf));
                            } else {
                                self.filters = Some(vec![hf]);
                            }
                        } else if let Some(ref mut hts) = self.tags {
                            hts.insert(label, tags);
                        } else {
                            let mut hts = IndexMap::new();
                            hts.insert(label, tags);
                            self.tags = Some(hts);
                        }
                    }
                    _ => {
                        // TODO: if embedded link and span content of heading,
                        // then setup overlay for merging/overriding of destination sibling heading.
                        // if wrapped by heading then setup overlay for insertion.
                        if let Ok((input, kvs)) = attributes(i) {
                            let s = span_with_attributes(s, kvs);
                            ss.push(s);
                            text_start = input;
                            i = input;
                        } else if let Span::Verbatim(content, _, _) = s {
                            if let Ok((input, format)) = preceded(tag("="), key)(i) {
                                ss.push(Span::Verbatim(content, Some(format), None));
                                text_start = input;
                                i = input;
                            } else {
                                ss.push(s);
                            }
                        } else {
                            ss.push(s);
                        }
                    }
                }
            } else if let Some(c) = i.chars().next() {
                boundary = c == ' ' || c == '\n' || c == '\t' || c == '\r';
                let char_length = c.len_utf8();
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
        (text_start, ss)
    }
}

// BLOCKS

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum Id {
    Uid(usize),
    Label(String),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum LType {
    // : <<Locator>>
    Definition,
    // (e), e), e.
    Ordered,
    // - [ ], + [ ], * [ ]
    // contents may be a checkbox indicated with space, x, or X
    // or input field indicated with digit1 or ratio (digit1:digit1)
    Task,
    // -, +, *
    Unordered,
}

// Block = {
//     Div
//   | Quote
//   | Heading
//   | Code
//   | ListHead
//   | Table
//   | Paragraph
// }
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum Label {
    Div(Id),
    Heading(HType, Id),
    List(Id),
    ListItem(LType, Id),
    Code(Id),
    Paragraph(Id),
}

//#[derive(Debug, PartialEq, Eq)]
//pub struct Block<'a> {
//    content: Vec<Span<'a>>,
//    attrs: Option<IndexMap<&'a str, &'a str>>,
//    span_refs: Option<SpanRefs>,
//    blocks: Option<IndexMap<(Label, Option<usize>), Block<'a>>>,
//}

#[derive(Debug, PartialEq, Eq)]
pub enum Block<'a> {
    D(
        // Attributes
        Option<IndexMap<&'a str, &'a str>>,
        // Div Name
        &'a str,
        // Contained Blocks
        IndexMap<(Label, Option<usize>), Block<'a>>,
        // Relative hash tags, hash filters and embedded links
        SpanRefs,
    ),

    //Quote(Vec<Block<'a>>),

    // NOTE: Label/Index identifies Heading Level
    H(
        // Attributes
        Option<IndexMap<&'a str, &'a str>>,
        // contents made up of nested Spans
        Vec<Span<'a>>,
        // List of next level contained headings and other blocks
        //   Similar to Adjacency List Concept
        IndexMap<(Label, Option<usize>), Block<'a>>,
        // Relative hash tags, hash filters, and embedded links
        SpanRefs,
    ),

    // List(attributes, listitems) NOTE: May use attributes to designate Label for list block
    L(
        Option<IndexMap<&'a str, &'a str>>,
        IndexMap<(Label, Option<usize>), Block<'a>>,
    ),

    // ListItem(content, child list) NOTE: Label/Index identify list item type
    LI(Vec<Span<'a>>, Option<Box<(Label, Block<'a>)>>),

    // Following Block Types contain no nested children

    // Code(format, [Span::Text], attributes)
    C(Option<IndexMap<&'a str, &'a str>>, Option<&'a str>, &'a str),

    //Table(Vec<Span<'a>>, Option<Vec<Align>>, Vec<Vec<Span<'a>>>),

    // Paragraph(spans, attributes)
    P(Option<IndexMap<&'a str, &'a str>>, Vec<Span<'a>>),
}

// H1      = { "#" }
// H2      = { "##" }
// H3      = { "###" }
// H4      = { "####" }
// H5      = { "#####" }
// H6      = { "######" }
static HTYPE: phf::Map<&'static str, HType> = phf_map! {
    "#" => HType::H1,
    "##" => HType::H2,
    "###" => HType::H3,
    "####" => HType::H4,
    "#####" => HType::H5,
    "######" => HType::H6,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum HType {
    H1,
    H2,
    H3,
    H4,
    H5,
    H6,
}

fn div_start(input: &str) -> IResult<&str, &str> {
    preceded(tuple((tag(":::"), space1)), field)(input)
}

fn div_close(input: &str) -> IResult<&str, bool> {
    value(true, tuple((tag(":::"), alt((line_ending, eof)))))(input)
}

fn head_start(input: &str) -> IResult<&str, HType> {
    let (i, htag) = terminated(take_while_m_n(1, 6, |c| c == '#'), space1)(input)?;
    if let Some(level) = HTYPE.get(htag) {
        Ok((i, *level))
    } else {
        Ok((i, HType::H6))
    }
}

// Heading = { (NEWLINE+ | SOI) ~ (H6 | H5 | H4 | H3 | H2 | H1) ~ (" " | "\t")+ ~ Location ~ ((LinkDlmr ~ Span+)? ~ &(NEWLINE | EOI)) }
//fn heading<'a>(input: &'a str) -> IResult<&'a str, Block<'a>> {
//    let (i, level) = head_start(input)?;
//    let (i, ss) = spans(i, None, Some(false))?;
//    Ok((i, Block::Heading(level, ss)))
//}

// CodeStart = { (NEWLINE+ | SOI) ~ PUSH("`"{3, 6}) ~ Attribute* }
// CodeText  = { NEWLINE ~ (!NEWLINE ~ ANY)* }
// CodeStop  = { NEWLINE ~ POP }
// Code      = { CodeStart ~ (!CodeStop ~ CodeText)* ~ (CodeStop | &(NEWLINE | EOI)) }
fn code(input: &str) -> IResult<&str, Block<'_>> {
    let (input, sctag) = terminated(take_while_m_n(3, 16, |c| c == '`'), not(tag("`")))(input)?;
    let (input, format) = opt(preceded(tag("="), key))(input)?;
    // let (input, format) = opt(field)(input)?;
    let (input, _) = opt(alt((line_ending, eof)))(input)?;
    let mut i = input;
    let mut char_total_length: usize = 0;
    while !i.is_empty() {
        if let Ok((i, _)) = tuple((
            line_ending,
            tag::<_, &str, ()>(sctag),
            alt((line_ending, eof)),
        ))(i)
        {
            let (content, _) = input.split_at(char_total_length);
            return Ok((i, Block::C(None, format, content)));
        }
        if let Some(c) = i.chars().next() {
            let char_length = c.len_utf8();
            (_, i) = i.split_at(char_length);
            char_total_length += char_length;
        }
    }
    let (content, _) = input.split_at(char_total_length);
    Ok((i, Block::C(None, format, content)))
}

// RomanLower = { "i" | "v" | "x" | "l" | "c" | "d" | "m" }
// RomanUpper = { "I" | "V" | "X" | "L" | "C" | "D" | "M" }
//    // : <<Locator>>
//    Definition(&'a str),
//    // (e), e), e.
//    Ordered(Enumerator<'a>),
//    // - [ ], + [ ], * [ ]
//    // contents may be a checkbox indicated with space, x, or X
//    // or input field indicated with digit1 or ratio (digit1:digit1)
//    Task(&'a str),
//    // -, +, *
//    Unordered(&'a str),
//}
pub struct Index<'a>(LType, &'a str);

// Definition = { ": " ~ Field }
fn definition(input: &str) -> IResult<&str, (Index, Option<&str>)> {
    let (i, d) = preceded(tuple((tag(":"), space1)), not_line_ending)(input)?;
    Ok((i, (Index(LType::Definition, d), None)))
}

// Definition = { ": " ~ Field }
fn definition_simple(input: &str) -> IResult<&str, (Index, Option<&str>)> {
    let (i, _d) = tag(":")(input)?;
    Ok((i, (Index(LType::Definition, ""), None)))
}

// Ordered    = { (ASCII_DIGIT+ | RomanLower+ | RomanUpper+ | ASCII_ALPHA_LOWER+ | ASCII_ALPHA_UPPER+) ~ ("." | ")") }
fn ordered(input: &str) -> IResult<&str, (Index, Option<&str>)> {
    let (i, (start_tag, (o, _numerical), end_tag)) = tuple((
        opt(tag("(")),
        consumed(alt((value(false, alpha1), value(true, digit1)))),
        alt((tag(")"), tag("."))),
    ))(input)?;
    if start_tag == Some("(") && end_tag != ")" {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alt,
        )));
    }
    Ok((i, (Index(LType::Ordered, o), None)))
}

fn ratio(input: &str) -> IResult<&str, &str> {
    let (i, (r, _)) = consumed(separated_pair(digit1, char(':'), digit1))(input)?;
    Ok((i, r))
}

fn task(input: &str) -> IResult<&str, (Index, Option<&str>)> {
    let (i, (l, task_value)) = tuple((
        terminated(is_a("-+*"), tag(" [")),
        terminated(alt((tag(" "), tag("x"), tag("X"), ratio, number)), tag("]")),
    ))(input)?;
    Ok((i, (Index(LType::Task, l), Some(task_value))))
}

// Unordered  = { "-" | "+" | "*" }
fn unordered(input: &str) -> IResult<&str, (Index, Option<&str>)> {
    let (i, u) = alt((tag("*"), tag("-"), tag("+")))(input)?;
    Ok((i, (Index(LType::Unordered, u), None)))
}

fn list_tag(input: &str) -> IResult<&str, Option<(&str, (Index, Option<&str>))>> {
    if let (input, Some((_, d, (idx, _)))) = opt(tuple((
        many0(line_ending),
        space0,
        alt((
            tuple((task, space1)),
            tuple((unordered, space1)),
            tuple((ordered, space1)),
            tuple((definition, peek(line_ending))),
        )),
    )))(input)?
    {
        Ok((input, Some((d, idx))))
    } else {
        Ok((input, None))
    }
}

fn is_list_singleline_tag(new: bool, input: &str) -> IResult<&str, bool> {
    if let (input, Some((_, _, _idx, _))) = opt(tuple((
        line_ending,
        cond(new, space0),
        alt((task, unordered, ordered, definition_simple)),
        space1,
    )))(input)?
    {
        Ok((input, true))
    } else if let (input, Some(_)) = opt(tuple((line_ending, cond(new, space0), tag("{"))))(input)?
    {
        Ok((input, true))
    } else {
        Ok((input, false))
    }
}

// ListBlock  = {
//   NEWLINE+ ~
//   PEEK[..] ~ PUSH((" " | "\t")+) ~ (Unordered | Ordered | Definition)
//                                  ~ (" " | NEWLINE) ~ ListItem ~
//   (PEEK[..] ~ (Unordered | Ordered) ~ " " ~ ListItem)* ~
//   DROP
// }
fn list_block<'a>(
    input: &'a str,
    depth: &'a str,
    index: (Index<'a>, Option<&'a str>),
    attrs: Option<IndexMap<&'a str, &'a str>>,
    span_refs: &mut SpanRefs,
) -> IResult<&'a str, Option<Block<'a>>> {
    let mut lis: IndexMap<(Label, Option<usize>), Block<'a>> = IndexMap::new();
    let mut i = input;
    let mut idx = index;
    loop {
        let (input, (l, li)) = list_item(i, depth, idx, span_refs)?;
        i = input;
        let mut version: usize = 0;
        while lis.contains_key(&(l.clone(), Some(version))) {
            version += 1;
        }
        lis.insert((l, Some(version)), li);
        if let (input, Some((d, index))) = list_tag(i)? {
            if d != depth {
                break;
            }
            idx = index;
            i = input;
        } else {
            break;
        }
    }
    let lis = if !lis.is_empty() {
        Some(Block::L(attrs, lis))
    } else {
        None
    };
    Ok((i, lis))
}

fn nested_list_block<'a>(
    input: &'a str,
    depth: &'a str,
    span_refs: &mut SpanRefs,
) -> IResult<&'a str, Option<Box<(Label, Block<'a>)>>> {
    let (i, depth_attrs) = opt(terminated(
        tuple((line_ending, space0, attributes)),
        line_ending,
    ))(input)?;
    if let (i, Some((d, index))) = list_tag(i)? {
        if let Some((_, ad, ah)) = depth_attrs {
            if ad.len() == d.len() {
                if d.len() <= depth.len() {
                    Ok((input, None))
                } else {
                    let id = if let Some(label) = ah.get(LABEL) {
                        Id::Label(label.to_string())
                    } else {
                        Id::Uid(1)
                    };
                    if let (i, Some(lb)) = list_block(i, d, index, Some(ah), span_refs)? {
                        Ok((i, Some(Box::new((Label::List(id), lb)))))
                    } else {
                        Ok((input, None))
                    }
                }
            } else {
                Ok((input, None))
            }
        } else if d.len() <= depth.len() {
            Ok((input, None))
        } else {
            if let (i, Some(lb)) = list_block(i, d, index, None, span_refs)? {
                Ok((i, Some(Box::new((Label::List(Id::Uid(1)), lb)))))
            } else {
                Ok((input, None))
            }
        }
    } else {
        Ok((input, None))
    }
}

// ListItem   = { Span+ ~ ListBlock* }
fn list_item<'a>(
    input: &'a str,
    depth: &'a str,
    index: (Index<'a>, Option<&'a str>),
    span_refs: &mut SpanRefs,
) -> IResult<&'a str, (Label, Block<'a>)> {
    // NOTE: Verify spans should not be able to fail. i.e. Make a test case for empty string ""
    // Or if needs to fail wrap in opt(spans...)
    let (input, ss) = span_refs.spans(input, None, Some(true));
    let (input, nlb) = nested_list_block(input, depth, span_refs)?;
    match index {
        (Index(LType::Ordered, s), _v) => Ok((
            input,
            (
                Label::ListItem(LType::Ordered, Id::Label(s.to_string())),
                Block::LI(ss, nlb),
            ),
        )),
        (Index(LType::Definition, s), _v) => Ok((
            input,
            (
                Label::ListItem(LType::Definition, Id::Label(span_label(&hashtags(vec![s])))),
                Block::LI(ss, nlb),
            ),
        )),
        (Index(LType::Unordered, _), _v) => Ok((
            input,
            (
                Label::ListItem(
                    LType::Unordered,
                    Id::Label(span_label(&hashtags(contents(&ss, true)))),
                ),
                Block::LI(ss, nlb),
            ),
        )),
        (Index(LType::Task, _), Some(v)) => Ok((
            input,
            (
                Label::ListItem(
                    LType::Task,
                    Id::Label(span_label(&hashtags(contents(&ss, true)))),
                ),
                Block::LI(vec![Span::Text(v)], nlb),
            ),
        )),
        (Index(LType::Task, _), None) => unreachable!(),
    }
}

// ListHead   = { ((NEWLINE+ | SOI) ~ PEEK[..]
//                ~ (Unordered | Ordered | Definition)
//                ~ (" " | NEWLINE) ~ ListItem)+ }
fn list<'a>(input: &'a str, span_refs: &mut SpanRefs) -> IResult<&'a str, Block<'a>> {
    if let (i, Some((d, index))) = list_tag(input)? {
        if d.is_empty() {
            if let (i, Some(lb)) = list_block(i, d, index, None, span_refs)? {
                return Ok((i, lb));
            }
        }
    }
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Alt,
    )))
}

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
#[derive(Debug, PartialEq, Eq)]
pub enum Align {
    Right,   // --:
    Default, // ---
    Center,  // :-:
    Left,    // :--
}

// Paragraph = { (NEWLINE+ | SOI) ~ Span+ ~ &(NEWLINE | EOI) }
#[allow(clippy::unnecessary_wraps)]
fn paragraph<'a>(input: &'a str, span_refs: &mut SpanRefs) -> IResult<&'a str, Block<'a>> {
    let (i, ss) = span_refs.spans(input, None, None);
    Ok((i, Block::P(None, ss)))
}

const LABEL: &'static str = "label";

// Div = {
//   (NEWLINE+ | SOI) ~ ":::" ~ Attribute* ~ " " ~ Field ~ &(NEWLINE | EOI) ~
//   (!(NEWLINE+ ~ ":::") ~ Block)* ~
//   NEWLINE+ ~ (":::" ~ &(NEWLINE | EOI) | EOI)
// }
// Document = { Block* ~ NEWLINE* ~ EOI }
fn blocks<'a>(
    input: &'a str,
    divs: bool,
    refs: Option<HType>,
    parent_span_refs: &mut SpanRefs,
) -> IResult<&'a str, IndexMap<(Label, Option<usize>), Block<'a>>> {
    let mut list_id = 0;
    let mut code_id = 0;
    let mut para_id = 0;
    let mut children: IndexMap<(Label, Option<usize>), Block<'a>> = IndexMap::new();
    // WARN: utilizing multispace here would cause lists that start with spaces
    // to look like new lists that start right after a newline, so cannot greedy
    // consume newlines with spaces.
    let mut i = input;
    loop {
        (i, _) = many0(line_ending)(i)?;
        if i.is_empty() {
            break;
        }
        if divs {
            // Do NOT consume input. Leave cleanup to the div that spawned blocks
            if let Ok((_, Some(true))) = opt(div_close)(i) {
                return Ok((i, children));
            }
        }
        let (input, attrs) = opt(terminated(attributes, line_ending))(i)?;
        i = input;
        if let Ok((input, Some(name))) = opt(div_start)(i) {
            let mut span_refs = SpanRefs::default();
            let (input, div_bs) = blocks(input, true, refs, &mut span_refs)?;
            let (input, _) = opt(div_close)(input)?;
            i = input;
            let div_index = span_label(&hashtags(vec![name]));
            let label = Label::Div(Id::Label(div_index));
            let mut version: usize = 0;
            while children.contains_key(&(label.clone(), Some(version))) {
                version += 1;
            }
            children.insert(
                (label, Some(version)),
                Block::D(attrs, name, div_bs, span_refs),
            );
        } else if let Ok((input, Some(hl))) = opt(head_start)(i) {
            if let Some(level) = refs {
                if level >= hl {
                    // Do NOT consume input until can start new heading block
                    return Ok((i, children));
                }
            }
            let mut span_refs = SpanRefs::default();
            let (input, head_spans) = span_refs.spans(input, None, Some(false));
            let (input, head_blocks) = blocks(input, divs, Some(hl), &mut span_refs)?;
            i = input;
            let head_index = span_label(&hashtags(contents(&head_spans, false)));
            let label = Label::Heading(hl, Id::Label(head_index));
            let mut version: usize = 0;
            while children.contains_key(&(label.clone(), Some(version))) {
                version += 1;
            }
            children.insert(
                (label, Some(version)),
                Block::H(attrs, head_spans, head_blocks, span_refs),
            );
        } else if let Ok((input, Block::L(_, ls))) = list(i, parent_span_refs) {
            i = input;
            let label = if let Some(ref attrs) = &attrs {
                if let Some(l) = attrs.get(LABEL) {
                    Label::List(Id::Label(l.to_string()))
                } else {
                    list_id += 1;
                    Label::List(Id::Uid(list_id))
                }
            } else {
                list_id += 1;
                Label::List(Id::Uid(list_id))
            };
            let mut version: usize = 0;
            while children.contains_key(&(label.clone(), Some(version))) {
                version += 1;
            }
            children.insert((label, Some(version)), Block::L(attrs, ls));
        } else if let Ok((input, Block::C(_, f, c))) = code(i) {
            i = input;
            let label = if let Some(ref attrs) = &attrs {
                if let Some(l) = attrs.get(LABEL) {
                    Label::Code(Id::Label(l.to_string()))
                } else {
                    code_id += 1;
                    Label::Code(Id::Uid(code_id))
                }
            } else {
                code_id += 1;
                Label::Code(Id::Uid(code_id))
            };
            let mut version: usize = 0;
            while children.contains_key(&(label.clone(), Some(version))) {
                version += 1;
            }
            children.insert((label, Some(version)), Block::C(attrs, f, c));
        } else if let (input, Block::P(_, ss)) = paragraph(i, parent_span_refs)? {
            i = input;
            para_id += 1;
            children.insert(
                (Label::Paragraph(Id::Uid(para_id)), None),
                Block::P(attrs, ss),
            );
        }
    }
    Ok((i, children))
}

// TODO: change tag's vector of strings to Hash types
#[derive(Debug, PartialEq, Eq)]
pub struct Document<'a> {
    pub blocks: IndexMap<(Label, Option<usize>), Block<'a>>,
    // Block levle hash tags, filter tags, and embedded links
    pub span_refs: SpanRefs,
    // References to a list of headings within the document
    // pub head_refs: Option<IndexMap<&'a str, Block<'a>>>,
}

// Document = { Block* ~ NEWLINE* ~ EOI }
pub fn document(input: &str) -> Result<Document<'_>, Box<dyn Error>> {
    let mut span_refs = SpanRefs::default();
    match blocks(input, false, None, &mut span_refs) {
        Ok((_, bs)) => Ok(Document {
            blocks: bs,
            span_refs,
        }),
        Err(e) => Err(Box::from(format!("Unable to parse input: {e:?}"))),
    }
}

pub fn merge<'a>(_doc: &Document<'a>) -> Document<'a> {
    let mdoc = Document {
        blocks: IndexMap::new(),
        span_refs: SpanRefs {
            tags: None,
            filters: None,
            embeds: None,
        },
    };
    mdoc
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[fixture]
    fn setup() {
        #[allow(clippy::unwrap_used)]
        color_eyre::install().unwrap();
    }

    fn ast(input: &str) -> IndexMap<(Label, Option<usize>), Block> {
        let doc = document(input).unwrap();
        doc.blocks
    }

    #[test]
    fn test_block_paragraph_text_line_break() {
        assert_eq!(
            ast("line\\\n"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(None, vec![Span::Text("line"), Span::LineBreak("\n")]),
            )]),
        );
    }

    #[test]
    fn test_block_paragraph_nbsp() {
        assert_eq!(
            ast("left\\ right"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![Span::Text("left"), Span::NBWS(" "), Span::Text("right")]
                ),
            )]),
        );
    }

    #[test]
    fn test_block_paragraph_text_edge_text() {
        assert_eq!(
            ast("left *strong* right"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Strong(vec![Span::Text("strong")], None),
                        Span::Text(" right")
                    ]
                ),
            )]),
        );
    }

    #[test]
    fn test_block_paragraph_text_edge_textedge_text() {
        assert_eq!(
            ast("l _*s* e_ r"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("l "),
                        Span::Emphasis(
                            vec![Span::Strong(vec![Span::Text("s")], None), Span::Text(" e")],
                            None
                        ),
                        Span::Text(" r")
                    ]
                ),
            )])
        );
    }

    #[test]
    fn test_block_paragraph_text_edgetext_textedge_text() {
        assert_eq!(
            ast("l _e1 *s* e2_ r"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("l "),
                        Span::Emphasis(
                            vec![
                                Span::Text("e1 "),
                                Span::Strong(vec![Span::Text("s")], None),
                                Span::Text(" e2")
                            ],
                            None
                        ),
                        Span::Text(" r")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_text_edge_edge_text() {
        assert_eq!(
            ast("l _*s*_ r"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("l "),
                        Span::Emphasis(vec![Span::Strong(vec![Span::Text("s")], None)], None),
                        Span::Text(" r")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_text_verbatim_text() {
        assert_eq!(
            ast("left ``verbatim``=fmt right"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Verbatim("verbatim", Some("fmt"), None),
                        Span::Text(" right")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_text_verbatim_newline() {
        assert_eq!(
            ast("left ``verbatim\n right"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Verbatim("verbatim", None, None),
                        Span::Text(" right")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_text_verbatim_with_nonmatching_backtick() {
        assert_eq!(
            ast("left ``ver```batim``{format=fmt} right"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Verbatim(
                            "ver```batim",
                            None,
                            Some(IndexMap::from([("format", "fmt")]))
                        ),
                        Span::Text(" right")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_text_verbatim_with_enclosing_backtick() {
        assert_eq!(
            ast("left `` `verbatim` `` right"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Verbatim("`verbatim`", None, None),
                        Span::Text(" right")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_hash_empty_eom() {
        assert_eq!(
            ast("left #"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(None, vec![Span::Text("left #")])
            )])
        );
    }

    #[test]
    fn test_block_paragraph_hash_empty_space() {
        assert_eq!(
            ast("left # "),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(None, vec![Span::Text("left # ")])
            )])
        );
    }

    #[test]
    fn test_block_paragraph_hash_field_eom() {
        let doc = document("left !#Hash").unwrap();
        assert_eq!(
            doc.span_refs,
            SpanRefs {
                tags: None,
                filters: Some(vec![HashFilter {
                    index: "hash".to_string(),
                    op: HashOp::NotEqual,
                    tags: vec![HashTag::Str("hash".to_string())],
                }]),
                embeds: None
            }
        );
        assert_eq!(
            doc.blocks,
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Hash(Some(HashOp::NotEqual), "Hash")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_hash_field_newline() {
        let doc = document("left #hash 1 \nnext line").unwrap();
        assert_eq!(
            doc.span_refs,
            SpanRefs {
                tags: Some(IndexMap::from([(
                    "hash".to_string(),
                    vec![
                        HashTag::Str("hash".to_string()),
                        HashTag::Space,
                        HashTag::Num(1)
                    ]
                )])),
                filters: None,
                embeds: None
            },
        );
        assert_eq!(
            doc.blocks,
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Hash(None, "hash 1"),
                        Span::Text("\nnext line")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_link_location() {
        let doc = document("left ![[loc]]").unwrap();
        assert_eq!(
            doc.span_refs,
            SpanRefs {
                tags: None,
                filters: None,
                embeds: Some(vec![("-".to_string(), "loc".to_string())]),
            },
        );
        assert_eq!(
            doc.blocks,
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![Span::Text("left "), Span::Link("loc", true, vec![], None)],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_link_with_location_and_text_super() {
        let doc = document("left [[loc|text^SUP^]] right").unwrap();
        assert_eq!(
            doc.span_refs,
            SpanRefs {
                tags: None,
                filters: None,
                embeds: None,
            },
        );
        assert_eq!(
            doc.blocks,
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Link(
                            "loc",
                            false,
                            vec![
                                Span::Text("text"),
                                Span::Superscript(vec![Span::Text("SUP")], None)
                            ],
                            None
                        ),
                        Span::Text(" right")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_link_with_location_and_span() {
        let doc = document("left ![[Loc|text `Verbatim`]] right").unwrap();
        assert_eq!(
            doc.span_refs,
            SpanRefs {
                tags: None,
                filters: None,
                embeds: Some(vec![("text-verbatim".to_string(), "Loc".to_string())]),
            },
        );
        assert_eq!(
            doc.blocks,
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Link(
                            "Loc",
                            true,
                            vec![Span::Text("text "), Span::Verbatim("Verbatim", None, None)],
                            None
                        ),
                        Span::Text(" right")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_nested_spans() {
        assert_eq!(
            ast("text-left [*strong-left [_emphasis-center_]\t[+insert-left [^superscript-center^] insert-right+] strong-right*] text-right"),
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("text-left "),
                        Span::Strong(
                            vec![
                                Span::Text("strong-left "),
                                Span::Emphasis(
                                    vec![Span::Text("emphasis-center")], None),
                                Span::Text("\t"),
                                Span::Insert(
                                    vec![
                                        Span::Text("insert-left "),
                                        Span::Superscript(
                                            vec![
                                                Span::Text("superscript-center")
                                            ], None),
                                        Span::Text(" insert-right")
                                    ], None),
                                Span::Text(" strong-right")
                            ], None),
                        Span::Text(" text-right")
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_link_attributes() {
        let doc = document("left ![[LOC]]{k1=v1 k_2=v_2}").unwrap();
        assert_eq!(
            doc.span_refs,
            SpanRefs {
                tags: None,
                filters: None,
                embeds: Some(vec![("-".to_string(), "LOC".to_string())]),
            },
        );
        assert_eq!(
            doc.blocks,
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Link(
                            "LOC",
                            true,
                            vec![],
                            Some(IndexMap::from([("k1", "v1",), ("k_2", "v_2")]))
                        )
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_link_multiline_attributes() {
        let doc = ast("left [[loc]]{k1=v1\n               k_2=v_2}");
        assert_eq!(
            doc,
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Link(
                            "loc",
                            false,
                            vec![],
                            Some(IndexMap::from([("k1", "v1",), ("k_2", "v_2")]))
                        ),
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_paragraph_link_space_valued_attributes() {
        let doc = ast(r#"left [[loc]]{k1=v1 k_2=v\ 2}"#);
        assert_eq!(
            doc,
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("left "),
                        Span::Link(
                            "loc",
                            false,
                            vec![],
                            Some(IndexMap::from([("k1", "v1",), ("k_2", r#"v\ 2"#)])),
                        )
                    ],
                )
            )])
        );
    }

    #[test]
    fn test_block_header_field_paragraph() {
        assert_eq!(
            ast("## [*strong heading*]"),
            IndexMap::from([(
                (
                    Label::Heading(HType::H2, Id::Label("strong-heading".to_string())),
                    Some(0)
                ),
                Block::H(
                    None,
                    vec![Span::Strong(vec![Span::Text("strong heading")], None)],
                    IndexMap::new(),
                    SpanRefs {
                        tags: None,
                        filters: None,
                        embeds: None,
                    },
                )
            )])
        );
    }

    #[test]
    fn test_block_header_field_paragraph_starting_text() {
        assert_eq!(
            ast("## header\nnext line [*strong*]\n\nnew paragraph"),
            IndexMap::from([(
                (
                    Label::Heading(HType::H2, Id::Label("header-next-line-strong".to_string())),
                    Some(0)
                ),
                Block::H(
                    None,
                    vec![
                        Span::Text("header\nnext line "),
                        Span::Strong(vec![Span::Text("strong")], None)
                    ],
                    IndexMap::from([(
                        (Label::Paragraph(Id::Uid(1)), None),
                        Block::P(None, vec![Span::Text("new paragraph")]),
                    )]),
                    SpanRefs {
                        tags: None,
                        filters: None,
                        embeds: None,
                    },
                )
            )])
        );
    }

    #[test]
    fn test_block_div_w_para_in_div_w_heading() {
        assert_eq!(
            ast("::: div1\n\n## [*strong heading*]\n\n::: div2\n\n  line"),
            IndexMap::from([(
                (Label::Div(Id::Label("div1".to_string())), Some(0)),
                Block::D(
                    None,
                    "div1",
                    IndexMap::from([(
                        (
                            Label::Heading(HType::H2, Id::Label("strong-heading".to_string())),
                            Some(0)
                        ),
                        Block::H(
                            None,
                            vec![Span::Strong(vec![Span::Text("strong heading")], None)],
                            IndexMap::from([(
                                (Label::Div(Id::Label("div2".to_string())), Some(0)),
                                Block::D(
                                    None,
                                    "div2",
                                    IndexMap::from([(
                                        (Label::Paragraph(Id::Uid(1)), None),
                                        Block::P(None, vec![Span::Text("  line")])
                                    )]),
                                    SpanRefs {
                                        tags: None,
                                        filters: None,
                                        embeds: None,
                                    },
                                )
                            )]),
                            SpanRefs {
                                tags: None,
                                filters: None,
                                embeds: None,
                            },
                        )
                    )]),
                    SpanRefs {
                        tags: None,
                        filters: None,
                        embeds: None,
                    },
                )
            )]),
        );
    }

    #[test]
    fn test_block_div_w_code() {
        assert_eq!(
            ast("::: div---1\n\n```=code\nline1\n````\nline3\n```\n\n:::\n"),
            IndexMap::from([(
                (Label::Div(Id::Label("div--1".to_string())), Some(0)),
                Block::D(
                    None,
                    "div---1",
                    IndexMap::from([(
                        (Label::Code(Id::Uid(1)), Some(0)),
                        Block::C(None, Some("code"), "line1\n````\nline3"),
                    )]),
                    SpanRefs {
                        tags: None,
                        filters: None,
                        embeds: None,
                    },
                )
            )]),
        );
    }

    #[test]
    fn test_block_div_w_codeattrs() {
        assert_eq!(
            ast("::: div1\n\n{format=code}\n```=fmt\nline1\n````\nline3\n```\n\n:::\n"),
            IndexMap::from([(
                (Label::Div(Id::Label("div1".to_string())), Some(0)),
                Block::D(
                    None,
                    "div1",
                    IndexMap::from([(
                        (Label::Code(Id::Uid(1)), Some(0)),
                        Block::C(
                            Some(IndexMap::from([("format", "code")])),
                            Some("fmt"),
                            "line1\n````\nline3",
                        )
                    )]),
                    SpanRefs {
                        tags: None,
                        filters: None,
                        embeds: None,
                    },
                )
            )]),
        );
    }

    #[test]
    fn test_block_div_para_attrs() {
        let doc = document("::: div1\n\n{format=blockquote}\ntest paragraph").unwrap();
        assert_eq!(
            doc.blocks,
            IndexMap::from([(
                (Label::Div(Id::Label("div1".to_string())), Some(0)),
                Block::D(
                    None,
                    "div1",
                    IndexMap::from([(
                        (Label::Paragraph(Id::Uid(1)), None),
                        Block::P(
                            Some(IndexMap::from([("format", "blockquote")])),
                            vec![Span::Text("test paragraph")],
                        )
                    )]),
                    SpanRefs {
                        tags: None,
                        filters: None,
                        embeds: None,
                    },
                )
            )]),
        );
    }

    #[test]
    fn test_block_unordered_list() {
        let doc =
            document("- l1 #H 1\n\n- l2 #H 2\n\n  - l2,1 >#H 3\n\n  - l2,2 !#H 4\n\n    - l2,2,1\n\n  - l2,3\n\n- l3")
                .unwrap();
        assert_eq!(
            doc.span_refs,
            SpanRefs {
                tags: Some(IndexMap::from([(
                    "h".to_string(),
                    vec![
                        HashTag::Str("h".to_string()),
                        HashTag::Space,
                        HashTag::Num(2)
                    ]
                )])),
                filters: Some(vec![
                    HashFilter {
                        index: "h".to_string(),
                        op: HashOp::GreaterThan,
                        tags: vec![
                            HashTag::Str("h".to_string()),
                            HashTag::Space,
                            HashTag::Num(3)
                        ]
                    },
                    HashFilter {
                        index: "h".to_string(),
                        op: HashOp::NotEqual,
                        tags: vec![
                            HashTag::Str("h".to_string()),
                            HashTag::Space,
                            HashTag::Num(4)
                        ]
                    }
                ]),
                embeds: None,
            }
        );
        assert_eq!(
            doc.blocks,
            IndexMap::from([(
                (Label::List(Id::Uid(1)), Some(0)),
                Block::L(
                    None,
                    IndexMap::from([
                        (
                            (
                                Label::ListItem(LType::Unordered, Id::Label("l1-h".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("l1 "), Span::Hash(None, "H 1")], None),
                        ),
                        (
                            (
                                Label::ListItem(LType::Unordered, Id::Label("l2-h".to_string())),
                                Some(0)
                            ),
                            Block::LI(
                                vec![Span::Text("l2 "), Span::Hash(None, "H 2")],
                                Some(Box::new((
                                    (Label::List(Id::Uid(1))),
                                    Block::L(
                                        None,
                                        IndexMap::from([
                                            (
                                                (
                                                    Label::ListItem(
                                                        LType::Unordered,
                                                        Id::Label("l2-1-h".to_string())
                                                    ),
                                                    Some(0)
                                                ),
                                                Block::LI(
                                                    vec![
                                                        Span::Text("l2,1 "),
                                                        Span::Hash(
                                                            Some(HashOp::GreaterThan),
                                                            "H 3"
                                                        )
                                                    ],
                                                    None
                                                )
                                            ),
                                            (
                                                (
                                                    Label::ListItem(
                                                        LType::Unordered,
                                                        Id::Label("l2-2-h".to_string())
                                                    ),
                                                    Some(0)
                                                ),
                                                Block::LI(
                                                    vec![
                                                        Span::Text("l2,2 "),
                                                        Span::Hash(Some(HashOp::NotEqual), "H 4")
                                                    ],
                                                    Some(Box::new((
                                                        (Label::List(Id::Uid(1))),
                                                        Block::L(
                                                            None,
                                                            IndexMap::from([(
                                                                (
                                                                    Label::ListItem(
                                                                        LType::Unordered,
                                                                        Id::Label(
                                                                            "l2-2-1".to_string()
                                                                        )
                                                                    ),
                                                                    Some(0)
                                                                ),
                                                                Block::LI(
                                                                    vec![Span::Text("l2,2,1")],
                                                                    None
                                                                )
                                                            ),])
                                                        )
                                                    )))
                                                )
                                            ),
                                            (
                                                (
                                                    Label::ListItem(
                                                        LType::Unordered,
                                                        Id::Label("l2-3".to_string())
                                                    ),
                                                    Some(0)
                                                ),
                                                Block::LI(vec![Span::Text("l2,3")], None)
                                            )
                                        ])
                                    )
                                )))
                            )
                        ),
                        (
                            (
                                Label::ListItem(LType::Unordered, Id::Label("l3".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("l3")], None),
                        )
                    ])
                )
            )])
        );
    }

    #[test]
    fn test_block_unordered_list_singleline_w_attrs() {
        assert_eq!(
            ast(r#"{k1=v1}
- l1
- l2
  {k2=v2 label=SubList2}
  - l2,1
  - l2,2
    - l2,2,1
  - l2,3
- l3"#),
            IndexMap::from([(
                (Label::List(Id::Uid(1)), Some(0)),
                Block::L(
                    Some(IndexMap::from([("k1", "v1")])),
                    IndexMap::from([
                        (
                            (
                                Label::ListItem(LType::Unordered, Id::Label("l1".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("l1")], None)
                        ),
                        (
                            (
                                Label::ListItem(LType::Unordered, Id::Label("l2".to_string())),
                                Some(0)
                            ),
                            Block::LI(
                                vec![Span::Text("l2")],
                                Some(Box::new((
                                    (Label::List(Id::Label("SubList2".to_string()))),
                                    Block::L(
                                        Some(IndexMap::from([("k2", "v2"), ("label", "SubList2")])),
                                        IndexMap::from([
                                            (
                                                (
                                                    Label::ListItem(
                                                        LType::Unordered,
                                                        Id::Label("l2-1".to_string())
                                                    ),
                                                    Some(0)
                                                ),
                                                Block::LI(vec![Span::Text("l2,1")], None)
                                            ),
                                            (
                                                (
                                                    Label::ListItem(
                                                        LType::Unordered,
                                                        Id::Label("l2-2".to_string())
                                                    ),
                                                    Some(0)
                                                ),
                                                Block::LI(
                                                    vec![Span::Text("l2,2")],
                                                    Some(Box::new((
                                                        (Label::List(Id::Uid(1))),
                                                        Block::L(
                                                            None,
                                                            IndexMap::from([(
                                                                (
                                                                    Label::ListItem(
                                                                        LType::Unordered,
                                                                        Id::Label(
                                                                            "l2-2-1".to_string()
                                                                        )
                                                                    ),
                                                                    Some(0)
                                                                ),
                                                                Block::LI(
                                                                    vec![Span::Text("l2,2,1")],
                                                                    None
                                                                )
                                                            )])
                                                        )
                                                    )))
                                                )
                                            ),
                                            (
                                                (
                                                    Label::ListItem(
                                                        LType::Unordered,
                                                        Id::Label("l2-3".to_string())
                                                    ),
                                                    Some(0)
                                                ),
                                                Block::LI(vec![Span::Text("l2,3")], None)
                                            )
                                        ])
                                    )
                                )))
                            )
                        ),
                        (
                            (
                                Label::ListItem(LType::Unordered, Id::Label("l3".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("l3")], None)
                        )
                    ])
                )
            )])
        );
    }

    #[test]
    fn test_block_unordered_list_singleline() {
        assert_eq!(
            ast("- l1\n- l2\n  - l2,1\n  - l2,2\n    - l2,2,1\n  - l2,3\n- l3"),
            IndexMap::from([(
                (Label::List(Id::Uid(1)), Some(0)),
                Block::L(
                    None,
                    IndexMap::from([
                        (
                            (
                                Label::ListItem(LType::Unordered, Id::Label("l1".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("l1")], None)
                        ),
                        (
                            (
                                Label::ListItem(LType::Unordered, Id::Label("l2".to_string())),
                                Some(0)
                            ),
                            Block::LI(
                                vec![Span::Text("l2")],
                                Some(Box::new((
                                    (Label::List(Id::Uid(1))),
                                    Block::L(
                                        None,
                                        IndexMap::from([
                                            (
                                                (
                                                    Label::ListItem(
                                                        LType::Unordered,
                                                        Id::Label("l2-1".to_string())
                                                    ),
                                                    Some(0)
                                                ),
                                                Block::LI(vec![Span::Text("l2,1")], None)
                                            ),
                                            (
                                                (
                                                    Label::ListItem(
                                                        LType::Unordered,
                                                        Id::Label("l2-2".to_string())
                                                    ),
                                                    Some(0)
                                                ),
                                                Block::LI(
                                                    vec![Span::Text("l2,2")],
                                                    Some(Box::new((
                                                        (Label::List(Id::Uid(1))),
                                                        Block::L(
                                                            None,
                                                            IndexMap::from([(
                                                                (
                                                                    Label::ListItem(
                                                                        LType::Unordered,
                                                                        Id::Label(
                                                                            "l2-2-1".to_string()
                                                                        )
                                                                    ),
                                                                    Some(0)
                                                                ),
                                                                Block::LI(
                                                                    vec![Span::Text("l2,2,1")],
                                                                    None
                                                                )
                                                            )])
                                                        )
                                                    )))
                                                )
                                            ),
                                            (
                                                (
                                                    Label::ListItem(
                                                        LType::Unordered,
                                                        Id::Label("l2-3".to_string())
                                                    ),
                                                    Some(0)
                                                ),
                                                Block::LI(vec![Span::Text("l2,3")], None)
                                            )
                                        ])
                                    )
                                )))
                            )
                        ),
                        (
                            (
                                Label::ListItem(LType::Unordered, Id::Label("l3".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("l3")], None)
                        )
                    ])
                )
            )])
        );
    }

    #[test]
    fn test_block_ordered_list() {
        assert_eq!(
            ast("a) l1\n\n(B) l2\n\n  1. l2,1"),
            IndexMap::from([(
                (Label::List(Id::Uid(1)), Some(0)),
                Block::L(
                    None,
                    IndexMap::from([
                        (
                            (
                                Label::ListItem(LType::Ordered, Id::Label("a".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("l1")], None),
                        ),
                        (
                            (
                                Label::ListItem(LType::Ordered, Id::Label("B".to_string())),
                                Some(0)
                            ),
                            Block::LI(
                                vec![Span::Text("l2")],
                                Some(Box::new((
                                    Label::List(Id::Uid(1)),
                                    Block::L(
                                        None,
                                        IndexMap::from([(
                                            (
                                                Label::ListItem(
                                                    LType::Ordered,
                                                    Id::Label("1".to_string())
                                                ),
                                                Some(0)
                                            ),
                                            Block::LI(vec![Span::Text("l2,1")], None),
                                        )])
                                    )
                                )))
                            ),
                        ),
                    ])
                ),
            )]),
        );
    }

    #[test]
    fn test_block_definition_list() {
        assert_eq!(
            ast(": ab\n  alpha\n\n: 12\n  digit\n\n  : iv\n    roman"),
            IndexMap::from([(
                (Label::List(Id::Uid(1)), Some(0)),
                Block::L(
                    None,
                    IndexMap::from([
                        (
                            (
                                Label::ListItem(LType::Definition, Id::Label("ab".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("\n  alpha")], None,),
                        ),
                        (
                            (
                                Label::ListItem(LType::Definition, Id::Label("12".to_string())),
                                Some(0)
                            ),
                            Block::LI(
                                vec![Span::Text("\n  digit")],
                                Some(Box::new((
                                    (Label::List(Id::Uid(1))),
                                    Block::L(
                                        None,
                                        IndexMap::from([(
                                            (
                                                Label::ListItem(
                                                    LType::Definition,
                                                    Id::Label("iv".to_string())
                                                ),
                                                Some(0)
                                            ),
                                            Block::LI(vec![Span::Text("\n    roman")], None)
                                        )])
                                    ),
                                )))
                            )
                        )
                    ])
                )
            )])
        )
    }

    #[test]
    fn test_block_header_sigleline_unordered_list() {
        assert_eq!(
            ast("## [*strong heading*]\n- l1 #Ha 1\n- l2 #Hb -1"),
            IndexMap::from([(
                (
                    Label::Heading(HType::H2, Id::Label("strong-heading".to_string())),
                    Some(0)
                ),
                Block::H(
                    None,
                    vec![Span::Strong(vec![Span::Text("strong heading")], None)],
                    IndexMap::from([(
                        (Label::List(Id::Uid(1)), Some(0)),
                        Block::L(
                            None,
                            IndexMap::from([
                                (
                                    (
                                        Label::ListItem(
                                            LType::Unordered,
                                            Id::Label("l1-ha".to_string())
                                        ),
                                        Some(0)
                                    ),
                                    Block::LI(
                                        vec![Span::Text("l1 "), Span::Hash(None, "Ha 1")],
                                        None
                                    )
                                ),
                                (
                                    (
                                        Label::ListItem(
                                            LType::Unordered,
                                            Id::Label("l2-hb".to_string())
                                        ),
                                        Some(0)
                                    ),
                                    Block::LI(
                                        vec![Span::Text("l2 "), Span::Hash(None, "Hb -1")],
                                        None
                                    )
                                )
                            ])
                        )
                    )]),
                    SpanRefs {
                        tags: Some(IndexMap::from([
                            (
                                "ha".to_string(),
                                vec![
                                    HashTag::Str("ha".to_string()),
                                    HashTag::Space,
                                    HashTag::Num(1)
                                ]
                            ),
                            (
                                "hb".to_string(),
                                vec![
                                    HashTag::Str("hb".to_string()),
                                    HashTag::Space,
                                    HashTag::Num(-1)
                                ]
                            )
                        ])),
                        filters: None,
                        embeds: None,
                    },
                )
            )])
        )
    }

    #[test]
    fn test_block_header_sigleline_span_not_start_of_line_so_not_list() {
        assert_eq!(
            ast("## [*strong heading\n  - l1*]\n  - l2"),
            IndexMap::from([(
                (
                    Label::Heading(HType::H2, Id::Label("strong-heading-l1-l2".to_string())),
                    Some(0)
                ),
                Block::H(
                    None,
                    vec![
                        Span::Strong(vec![Span::Text("strong heading\n  - l1")], None),
                        Span::Text("\n  - l2")
                    ],
                    IndexMap::new(),
                    SpanRefs {
                        tags: None,
                        filters: None,
                        embeds: None,
                    }
                )
            )])
        )
    }

    #[test]
    fn test_block_task_list() {
        assert_eq!(
            ast(": ab\n  - [ ] alpha"),
            IndexMap::from([(
                (Label::List(Id::Uid(1)), Some(0)),
                Block::L(
                    None,
                    IndexMap::from([(
                        (
                            Label::ListItem(LType::Definition, Id::Label("ab".to_string())),
                            Some(0)
                        ),
                        Block::LI(
                            vec![],
                            Some(Box::new((
                                (Label::List(Id::Uid(1))),
                                Block::L(
                                    None,
                                    IndexMap::from([(
                                        (
                                            Label::ListItem(
                                                LType::Task,
                                                Id::Label("alpha".to_string())
                                            ),
                                            Some(0)
                                        ),
                                        Block::LI(vec![Span::Text(" ")], None)
                                    )])
                                )
                            )))
                        )
                    )])
                )
            )])
        )
    }

    #[test]
    fn test_block_task_values_list() {
        assert_eq!(
            ast(r#"- [ ] alpha
- [x] bravo
- [X] charlie
- [0:1] delta
- [10] echo
- [-10] foxtrot
- [+10] golf"#),
            IndexMap::from([(
                (Label::List(Id::Uid(1)), Some(0)),
                Block::L(
                    None,
                    IndexMap::from([
                        (
                            (
                                Label::ListItem(LType::Task, Id::Label("alpha".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text(" ")], None)
                        ),
                        (
                            (
                                Label::ListItem(LType::Task, Id::Label("bravo".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("x")], None)
                        ),
                        (
                            (
                                Label::ListItem(LType::Task, Id::Label("charlie".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("X")], None)
                        ),
                        (
                            (
                                Label::ListItem(LType::Task, Id::Label("delta".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("0:1")], None)
                        ),
                        (
                            (
                                Label::ListItem(LType::Task, Id::Label("echo".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("10")], None)
                        ),
                        (
                            (
                                Label::ListItem(LType::Task, Id::Label("foxtrot".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("-10")], None)
                        ),
                        (
                            (
                                Label::ListItem(LType::Task, Id::Label("golf".to_string())),
                                Some(0)
                            ),
                            Block::LI(vec![Span::Text("+10")], None)
                        )
                    ])
                )
            )])
        )
    }

    #[test]
    fn test_nested_divs_and_headers() {
        let doc = ast(r#"# h1

doc > h1

## h2a

doc > h1 > h2a

::: d1

doc > h1 > h2a > d1

### h3a

doc > h1 > h2a > d1 > h3a

#### h4a

doc > h1 > h2a > d1 > h3a > h4a

#### h4b

doc > h1 > h2a > d1 > h3a > h4b

:::

doc > h1 > h2a

### h3b

doc > h1 > h2a > h3b

::: d2
doc > h1 > h2a > h3b > d2

#### h4b

doc > h1 > h2a > h3b > d2 > h4b

## h2b

doc > h1 > h2b

:::
doc > h1 > h2b // No change
"#);
        assert_eq!(
            doc,
            IndexMap::from([(
                (Label::Heading(HType::H1, Id::Label("h1".to_string())), Some(0)),
                Block::H(
                    None,
                    vec![Span::Text("h1")],
                    IndexMap::from([
                        (
                            (Label::Paragraph(Id::Uid(1)), None),
                            Block::P(
                                None,
                                vec![Span::Text("doc > h1")]
                            )
                        ),
                        (
                            (Label::Heading(HType::H2, Id::Label("h2a".to_string())), Some(0)),
                            Block::H(
                                None,
                                vec![Span::Text("h2a")],
                                IndexMap::from([
                                    (
                                        (Label::Paragraph(Id::Uid(1)), None),
                                        Block::P(
                                            None,
                                            vec![Span::Text("doc > h1 > h2a")]
                                        ),
                                    ),(
                                        (Label::Div(Id::Label("d1".to_string())), Some(0)),
                                        Block::D(
                                            None,
                                            "d1",
                                            IndexMap::from([
                                                (
                                                    (Label::Paragraph(Id::Uid(1)), None),
                                                    Block::P(
                                                        None,
                                                        vec![Span::Text("doc > h1 > h2a > d1")]
                                                    )
                                                ),(
                                                    (Label::Heading(HType::H3, Id::Label("h3a".to_string())), Some(0)),
                                                    Block::H(
                                                        None,
                                                        vec![Span::Text("h3a")],
                                                        IndexMap::from([
                                                            (
                                                                (Label::Paragraph(Id::Uid(1)), None),
                                                                Block::P(
                                                                    None,
                                                                    vec![Span::Text("doc > h1 > h2a > d1 > h3a")]
                                                                )
                                                            ),(
                                                                (Label::Heading(HType::H4, Id::Label("h4a".to_string())), Some(0)),
                                                                Block::H(
                                                                    None,
                                                                    vec![Span::Text("h4a")],
                                                                    IndexMap::from([(
                                                                        (Label::Paragraph(Id::Uid(1)), None),
                                                                        Block::P(
                                                                            None,
                                                                            vec![Span::Text(
                                                                                "doc > h1 > h2a > d1 > h3a > h4a"
                                                                            )]
                                                                        )
                                                                    )]),
                                                                    SpanRefs {
                                                                        tags: None,
                                                                        filters: None,
                                                                        embeds: None
                                                                    }
                                                                )
                                                            ),(
                                                                (Label::Heading(HType::H4, Id::Label("h4b".to_string())), Some(0)),
                                                                Block::H(
                                                                    None,
                                                                    vec![Span::Text("h4b")],
                                                                    IndexMap::from([(
                                                                        (Label::Paragraph(Id::Uid(1)), None),
                                                                        Block::P(
                                                                            None,
                                                                            vec![Span::Text(
                                                                                "doc > h1 > h2a > d1 > h3a > h4b"
                                                                            )]
                                                                        )
                                                                    )]),
                                                                    SpanRefs {
                                                                        tags: None,
                                                                        filters: None,
                                                                        embeds: None
                                                                    }
                                                                )
                                                            )
                                                        ]),
                                                        SpanRefs {
                                                            tags: None,
                                                            filters: None,
                                                            embeds: None
                                                        }
                                                    )
                                                )
                                            ]),
                                            SpanRefs {
                                                tags: None,
                                                filters: None,
                                                embeds: None
                                            }
                                        )
                                    ),(
                                        (Label::Paragraph(Id::Uid(2)), None),
                                        Block::P(
                                            None,
                                            vec![Span::Text("doc > h1 > h2a")]
                                        ),
                                    ),(
                                        (Label::Heading(HType::H3, Id::Label("h3b".to_string())), Some(0)),
                                        Block::H(
                                            None,
                                            vec![Span::Text("h3b")],
                                            IndexMap::from([
                                                (
                                                    (Label::Paragraph(Id::Uid(1)), None),
                                                    Block::P(
                                                        None,
                                                        vec![Span::Text("doc > h1 > h2a > h3b")]
                                                    ),
                                                ),(
                                                    (Label::Div(Id::Label("d2".to_string())), Some(0)),
                                                    Block::D(
                                                        None,
                                                        "d2",
                                                        IndexMap::from([
                                                            (
                                                                (Label::Paragraph(Id::Uid(1)), None),
                                                                Block::P(
                                                                    None,
                                                                    vec![Span::Text("doc > h1 > h2a > h3b > d2")]
                                                                )
                                                            ),(
                                                                (Label::Heading(HType::H4, Id::Label("h4b".to_string())), Some(0)),
                                                                Block::H(
                                                                    None,
                                                                    vec![Span::Text("h4b")],
                                                                    IndexMap::from([(
                                                                        (Label::Paragraph(Id::Uid(1)), None),
                                                                        Block::P(
                                                                            None,
                                                                            vec![Span::Text(
                                                                                "doc > h1 > h2a > h3b > d2 > h4b"
                                                                            )]
                                                                        )
                                                                    )]),
                                                                    SpanRefs {
                                                                        tags: None,
                                                                        filters: None,
                                                                        embeds: None,
                                                                    }
                                                                )
                                                            )
                                                        ]),
                                                        SpanRefs {
                                                            tags: None,
                                                            filters: None,
                                                            embeds: None
                                                        }
                                                    )
                                                )
                                            ]),
                                            SpanRefs {
                                                tags: None,
                                                filters: None,
                                                embeds: None,
                                            }
                                        )
                                    )
                                ]),
                                SpanRefs {
                                    tags: None,
                                    filters: None,
                                    embeds: None,
                                }
                            )
                        ),(
                            (Label::Heading(HType::H2, Id::Label("h2b".to_string())), Some(0)),
                            Block::H(
                                None,
                                vec![Span::Text("h2b")],
                                IndexMap::from([
                                    (
                                        (Label::Paragraph(Id::Uid(1)), None),
                                        Block::P(
                                            None,
                                            vec![Span::Text("doc > h1 > h2b")]
                                        ),
                                    ),
                                    (
                                        (Label::Paragraph(Id::Uid(2)), None),
                                        Block::P(
                                            None,
                                            vec![Span::Text(":::\ndoc > h1 > h2b // No change\n")]
                                        ),
                                     )
                                ]),
                                SpanRefs {
                                    tags: None,
                                    filters: None,
                                    embeds: None,
                                }
                            )
                        )
                    ]),
                    SpanRefs {
                        tags: None,
                        filters: None,
                        embeds: None,
                    }
                )
            )])
        );
    }

    #[test]
    fn test_content_and_hashtag_index_of_block_paragraph_link_with_location_and_span() {
        let doc =
            document("Left \\\n![[loC|text-32 `-32v` [*a[_B_]*]]] Right #Level 1 \n").unwrap();
        assert_eq!(
            doc.span_refs,
            SpanRefs {
                tags: Some(IndexMap::from([(
                    "level".to_string(),
                    vec![
                        HashTag::Str("level".to_string()),
                        HashTag::Space,
                        HashTag::Num(1)
                    ]
                )])),
                filters: None,
                embeds: Some(vec![("text-32--32v-ab".to_string(), "loC".to_string())])
            }
        );
        assert_eq!(
            doc.blocks,
            IndexMap::from([(
                (Label::Paragraph(Id::Uid(1)), None),
                Block::P(
                    None,
                    vec![
                        Span::Text("Left "),
                        Span::LineBreak("\n"),
                        Span::Link(
                            "loC",
                            true,
                            vec![
                                Span::Text("text-32 "),
                                Span::Verbatim("-32v", None, None),
                                Span::Text(" "),
                                Span::Strong(
                                    vec![
                                        Span::Text("a"),
                                        Span::Emphasis(vec![Span::Text("B"),], None)
                                    ],
                                    None
                                )
                            ],
                            None
                        ),
                        Span::Text(" Right "),
                        Span::Hash(None, "Level 1"),
                        Span::Text("\n")
                    ],
                )
            )])
        );
        if let Some((_, Block::P(_, ss))) = doc.blocks.get_index(0) {
            let ts = contents(&ss, true);
            assert_eq!(
                ts,
                vec![
                    "Left ", "\n", "text-32 ", "-32v", " ", "a", "B", " Right ", "L", "e", "v",
                    "e", "l", " ", "\n"
                ]
            );
            let hts = hashtags(ts);
            assert_eq!(
                hts,
                vec![
                    HashTag::Str("left".to_string()),
                    HashTag::Space,
                    HashTag::Str("text".to_string()),
                    HashTag::Space,
                    HashTag::Num(32),
                    HashTag::Space,
                    HashTag::Num(-32),
                    HashTag::Str("v".to_string()),
                    HashTag::Space,
                    HashTag::Str("ab".to_string()),
                    HashTag::Space,
                    HashTag::Str("right".to_string()),
                    HashTag::Space,
                    HashTag::Str("level".to_string()),
                ]
            );
            let s = span_label(&hts);
            assert_eq!(s, "left-text-32--32v-ab-right-level");
            let s = htlabel(&hts);
            assert_eq!(s, "left-text-v-ab-right-level");
        } else {
            panic!(
                "Not able to get span from paragragh within vector {:?}",
                doc
            );
        }
    }

    #[test]
    fn test_hashtag_index_of_block_heading_link_span() {
        let doc = ast("# ![[loc|Level 0]]");
        if let Some(((label, Some(0)), Block::H(attrs, ss, _, srs))) = &doc.get_index(0) {
            assert_eq!(
                *label,
                Label::Heading(HType::H1, Id::Label("level-0".to_string()))
            );
            assert_eq!(*attrs, None);
            let ts = contents(&ss, false);
            let hts = hashtags(ts);
            let s = span_label(&hts);
            assert_eq!(s, "level-0");
            let s = htlabel(&hts);
            assert_eq!(s, "level");
            assert_eq!(
                *srs,
                SpanRefs {
                    tags: None,
                    filters: None,
                    embeds: Some(vec![("level-0".to_string(), "loc".to_string())])
                }
            );
        } else {
            panic!("Not able to get span from heading {:?}", doc);
        }
    }

    #[test]
    fn test_hashtag_index_of_block_heading_negative_digit_only() {
        let doc = ast("# -33-32 31 -30");
        if let Some(((label, Some(0)), Block::H(attrs, ss, _, srs))) = &doc.get_index(0) {
            assert_eq!(
                *label,
                Label::Heading(HType::H1, Id::Label("--33-32-31--30".to_string()))
            );
            assert_eq!(*attrs, None);
            let ts = contents(&ss, false);
            let hts = hashtags(ts);
            let s = span_label(&hts);
            assert_eq!(s, "--33-32-31--30");
            let s = htlabel(&hts);
            assert_eq!(s, "-");
            assert_eq!(
                *srs,
                SpanRefs {
                    tags: None,
                    filters: None,
                    embeds: None,
                }
            );
        } else {
            panic!("Not able to get span from heading {:?}", doc);
        }
    }
}
