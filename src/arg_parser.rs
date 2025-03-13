use std::collections::HashMap;

// util module for parsing cli args, i could use a crate for this but ehhh

pub struct FlagArg<'a> {
    name: &'a str,
    alias: char,
    fields: usize,
}
impl<'a> FlagArg<'a> {
    pub fn new(name: &'a str, alias: char, fields: usize) -> Self {
        Self {
            name,
            alias,
            fields,
        }
    }
}
pub struct Flags<'a> {
    full_flag_map: HashMap<&'a str, &'a FlagArg<'a>>,
    alias_flag_map: HashMap<char, &'a FlagArg<'a>>,
}
impl<'a> Flags<'a> {
    pub fn new(flag_definitions: &'a [FlagArg]) -> Self {
        let mut full_flag_map: HashMap<&str, &'a FlagArg<'a>> = HashMap::new();
        let mut alias_flag_map: HashMap<char, &'a FlagArg<'a>> = HashMap::new();
        for flag in flag_definitions {
            alias_flag_map.insert(flag.alias, flag);
            full_flag_map.insert(flag.name, flag);
        }
        Self {
            alias_flag_map,
            full_flag_map,
        }
    }
    fn get_flag_from_alias(&self, alias: &char) -> Option<&&FlagArg> {
        self.alias_flag_map.get(alias)
    }
    fn get_flag_from_full(&self, name: &str) -> Option<&&FlagArg> {
        self.full_flag_map.get(name)
    }
}
#[derive(Debug)]
pub struct ParsedArgEntry<'a> {
    pub name: &'a str,
    pub data: Vec<&'a str>,
}
#[derive(Debug)]
pub struct ParsedCLIArgs<'a> {
    pub raw: Vec<&'a str>,
    pub flags: Vec<ParsedArgEntry<'a>>,
}
impl<'a> ParsedCLIArgs<'a> {
    pub fn parse_arguments(flag_def: &'a Flags, cli_args: &'a [String]) -> Result<Self, String> {
        // let cli_args: &'a Vec<String> = std::env::args().collect();
        let mut raw: Vec<&str> = vec![];
        let mut flags: Vec<ParsedArgEntry<'a>> = vec![];
        let mut skip = 1;
        for (i, cli_arg) in cli_args.iter().enumerate() {
            if skip > 0 {
                skip -= 1;
                continue;
            }
            if cli_arg.starts_with('-') {
                // flag
                if cli_arg.starts_with("--") {
                    // full flag
                    let (entry, skip_by) = parse_full(cli_arg, flag_def, cli_args, i)?;
                    flags.push(entry);
                    skip += skip_by
                } else {
                    // alias
                    //
                    let (entries, skip_by) = parse_aliases(cli_arg, flag_def, cli_args, i)?;
                    for entry in entries {
                        flags.push(entry);
                    }
                    skip += skip_by
                }
            } else {
                // raw
                raw.push(cli_arg)
            }
        }
        Ok(Self { raw, flags })
    }
}
fn parse_aliases<'a>(
    arg: &'a str,
    flag_def: &'a Flags,
    cli_args: &'a [String],
    index: usize,
) -> Result<(Vec<ParsedArgEntry<'a>>, usize), String> {
    let mut skip = 0;
    let aliases: Vec<char> = arg[1..].chars().collect();
    let mut buf: Vec<ParsedArgEntry> = vec![];
    for (i, alias) in aliases.iter().enumerate() {
        let flag = if let Some(flag) = flag_def.get_flag_from_alias(&alias) {
            flag
        } else {
            return Err(format!("{alias} is not a valid flag"));
        };
        let mut fields: Vec<&'a str> = vec![];
        // if last element
        if flag.fields > 0 {
            if i == aliases.len() - 1 {
                //
                for ii in 1..=flag.fields {
                    let field = if let Some(f) = cli_args.get(index + ii) {
                        f
                    } else {
                        return Err(format!(
                            "{} requires {} argument(s) while only {} were provided",
                            flag.name,
                            flag.fields,
                            ii - 1
                        ));
                    };
                    fields.push(field);
                    skip += 1;
                }
            } else {
                return Err(format!("{} requires arguments which are only allowed for the last flag alias when in series ({arg})",flag.name));
            }
        }
        buf.push(ParsedArgEntry {
            name: flag.name,
            data: fields,
        });
    }
    Ok((buf, skip))
}

fn parse_full<'a>(
    arg: &'a str,
    flag_def: &'a Flags,
    cli_args: &'a [String],
    index: usize,
) -> Result<(ParsedArgEntry<'a>, usize), String> {
    let mut skip = 0;
    let flag = if let Some(flag) = flag_def.get_flag_from_full(&arg[2..]) {
        flag
    } else {
        return Err(format!("{arg} is not a valid flag"));
    };
    let mut fields: Vec<&'a str> = vec![];
    for ii in 1..=flag.fields {
        let field = if let Some(f) = cli_args.get(index + ii) {
            f
        } else {
            return Err(format!("{} requires {} arguments", flag.name, flag.fields));
        };
        fields.push(field);
        skip += 1;
    }
    let entry = ParsedArgEntry {
        name: flag.name,
        data: fields,
    };
    Ok((entry, skip))
}
