
#[derive(Debug)]
struct MacroDef {
    name: String,
    arg_names: Vec<String>,
    replace_str: String,
}

#[derive(Debug)]
struct Macro {
    name: String,
    args: Vec<String>,
    len: usize
}

#[derive(Debug)]
pub enum PreprocessorError {
    EmptyMacroName,
    MacroNotClosed,
    MissingArgumentList,
    MacroNotDefined,
    ArgCount,
    MacroAlreadyDefined,
    FileSystemError(String),
}

// $MACRO_NAME$$ args0 $ arg1 $ ... :: replace_str $$, $MACRO_NAME $$ $$
fn parse_macro(line_slice: &str) -> Result<Macro, PreprocessorError> {
    let (name, line_slice, len1) = if let Some(idx) = line_slice.find("$$") {
        (line_slice[..idx].trim().to_owned(), &line_slice[idx+2..], idx)
    } else {
        return Err(PreprocessorError::MissingArgumentList);
    };

    if name.is_empty() {
        return Err(PreprocessorError::EmptyMacroName);
    }

    let (args, len2) = if let Some(idx) = line_slice.find("$$") {
        let args = &line_slice[..idx].trim();
        let args = if !args.is_empty() {
            args.split("$")
                .map(|s| s.trim().to_owned())
                .collect()
        } else {
            vec![]
        };

        (args, idx)
    } else {
        return Err(PreprocessorError::MacroNotClosed);
    };


                             // $   NAME   $$  ARGS  $$    
    Ok(Macro { name, args, len: 1 + len1 + 2 + len2 + 2})
}

fn define_macro(r#macro: &Macro, macros: &mut Vec<MacroDef>) -> Result<(), PreprocessorError> {
    if r#macro.args.len() < 2 {
        return Err(PreprocessorError::ArgCount)
    } else if macros.iter().find(|m| m.name == r#macro.args[0]).is_some() {
        return Err(PreprocessorError::MacroAlreadyDefined);
    }

    let arg_names = if r#macro.args.len() > 2 {
        r#macro.args[1..r#macro.args.len()-1].to_vec()
    } else {
        vec![]
    };

    macros.push(MacroDef { 
        name: r#macro.args[0].clone(), 
        arg_names,
        replace_str: r#macro.args.last().unwrap().clone(),
    });
    Ok(())
}

fn import_macro(r#macro: &Macro) -> Result<String, PreprocessorError> {
    if r#macro.args.len() != 1 {
        return Err(PreprocessorError::ArgCount)
    }

    let file_path = &r#macro.args[0];
    let code = std::fs::read_to_string(file_path)
        .map_err(|err| PreprocessorError::FileSystemError(err.to_string()))?;
    Ok(code)
}

fn apply_macro(r#macro: &Macro, macro_def: &MacroDef) -> Result<String, PreprocessorError> {
    println!("{:?} {:?}", r#macro, macro_def);

    if r#macro.args.len() != macro_def.arg_names.len() {
        return Err(PreprocessorError::ArgCount);
    }

    let mut result = macro_def.replace_str.clone();
    for (idx, arg) in r#macro.args.iter().enumerate() {
        result = result.replace(&macro_def.arg_names[idx], arg);
    }

    Ok(result)
}

pub fn preprocess(source_code: &str) -> Result<String, (PreprocessorError, usize)> {
    let mut macros: Vec<MacroDef> = vec![
        // these 2 macros have special implementations hardcoded in the preprocessor
        MacroDef {
            name: "import".to_string(),
            arg_names: vec!["file_path".to_string()],
            replace_str: String::new(),
        },
        MacroDef {
            name: "define".to_string(),
            arg_names: vec![],
            replace_str: String::new(),
        },
    ];
        
    let mut builder = String::new();
    for (line_idx, line) in source_code.lines().enumerate() {
        let mut last_idx = 0;
        while let Some(idx) = line[last_idx..].find("$") {
            builder.push_str(&line[last_idx..idx]);
            let r#macro = parse_macro(&line[idx+1..])
                .map_err(|err| (err, line_idx + 1))?;
            last_idx = idx + r#macro.len;

            if let Some(macro_def) = macros.iter().find(|m| m.name == r#macro.name) {
                if macro_def.name == "define" {
                    let _ = define_macro(&r#macro, &mut macros)
                        .map_err(|err| (err, line_idx + 1))?;
                } else if macro_def.name == "import" {
                    let code = import_macro(&r#macro)
                        .map_err(|err| (err, line_idx + 1))?;
                    builder.push_str(&code);
                } else {
                    let res = apply_macro(&r#macro, macro_def)
                        .map_err(|err| (err, line_idx + 1))?;
                    builder.push_str(&res);
                }
            } else {
                return Err((PreprocessorError::MacroNotDefined, line_idx + 1));
            };
        }

        builder.push_str(&line[last_idx..]);
        builder.push('\n');
    }

    Ok(builder)
}