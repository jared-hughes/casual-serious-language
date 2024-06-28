use casual_serious_language::compile_and_interpret;

fn main() {
    let r = run();
    if let Err(err) = r {
        eprintln!("{}", err);
    }
}

fn run() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        return Err("Incorrect number of arguments.\n\
        Usage: casual-serious-language filename.csl"
            .to_owned());
    }
    let path = &args[1];
    let contents = std::fs::read_to_string(path).map_err(|e| format!("{}", e))?;
    let ret = compile_and_interpret(&contents).map_err(|e| format!("{:#?}", e))?;
    println!("{}", ret);
    Ok(())
}
