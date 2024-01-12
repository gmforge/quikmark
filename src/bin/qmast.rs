extern crate qwikmark;
use qwikmark::{document, Document};
use std::rc::Rc;
use std::{env, error::Error, fs};

struct Documents<'a> {
    cache: Vec<(Rc<String>, Document<'a>)>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut docs = Documents { cache: vec![] };
    // TODO: These need to be part of context
    // NOTE: Links cannot be nested
    // NOTE: Verbatim cannot be nested
    let args: Vec<String> = env::args().collect();
    let name = args[1].clone();
    let content = fs::read_to_string(&name)?;
    let content = Rc::new(content);
    //let contents2 = Rc::clone(&contents);
    let doc = document(&content)?;
    docs.cache.push((Rc::clone(&content), doc));
    println!("{:?}", docs.cache[0].1.blocks);
    println!("{:?}", content);
    Ok(())
}
