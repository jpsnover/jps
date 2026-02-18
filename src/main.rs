use jps::{debug, Parser, PipelineExecutor, Value};
use serde_json::Value as JsonValue;
use std::io::{self, Read};

/// PowerShell-style argument structure
struct Args {
    script: Option<String>,
    input_file: Option<String>,
    json_lines: bool,
    list_commands: bool,
    debug: bool,
    debug_file: Option<String>,
    begin: Option<String>,
    process: Option<String>,
    end: Option<String>,
}

impl Args {
    fn parse(argv: Vec<String>) -> Result<Self, String> {
        let mut args = Args {
            script: None,
            input_file: None,
            json_lines: false,
            list_commands: false,
            debug: false,
            debug_file: None,
            begin: None,
            process: None,
            end: None,
        };

        let mut i = 1; // skip program name
        while i < argv.len() {
            let arg = &argv[i];

            // Check for switch parameters (single - prefix, case-insensitive)
            if arg.starts_with('-') && arg.len() > 1 {
                let name = &arg[1..]; // strip leading -
                let name_lower = name.to_lowercase();

                // Check for abbreviated/full parameter names
                if is_param_match(&name_lower, "inputfile") || name_lower == "i" || name_lower == "in" || name_lower == "if" {
                    i += 1;
                    if i >= argv.len() {
                        return Err(format!("-{} requires a value", name));
                    }
                    args.input_file = Some(argv[i].clone());
                } else if is_param_match(&name_lower, "jsonlines") || name_lower == "l" {
                    args.json_lines = true;
                } else if is_param_match(&name_lower, "list-commands") || is_param_match(&name_lower, "listcommands") || name_lower == "list" {
                    args.list_commands = true;
                } else if is_param_match(&name_lower, "debug") || name_lower == "d" {
                    args.debug = true;
                } else if is_param_match(&name_lower, "debug-file") || is_param_match(&name_lower, "debugfile") {
                    i += 1;
                    if i >= argv.len() {
                        return Err(format!("-{} requires a value", name));
                    }
                    args.debug_file = Some(argv[i].clone());
                } else if is_param_match(&name_lower, "begin") || name_lower == "b" {
                    i += 1;
                    if i >= argv.len() {
                        return Err(format!("-{} requires a script block", name));
                    }
                    args.begin = Some(argv[i].clone());
                } else if is_param_match(&name_lower, "process") || name_lower == "p" {
                    i += 1;
                    if i >= argv.len() {
                        return Err(format!("-{} requires a script block", name));
                    }
                    args.process = Some(argv[i].clone());
                } else if is_param_match(&name_lower, "end") || name_lower == "e" {
                    i += 1;
                    if i >= argv.len() {
                        return Err(format!("-{} requires a script block", name));
                    }
                    args.end = Some(argv[i].clone());
                } else if is_param_match(&name_lower, "help") || name_lower == "h" || name_lower == "?" {
                    print_usage();
                    std::process::exit(0);
                } else if is_param_match(&name_lower, "version") {
                    println!("jps {}", env!("CARGO_PKG_VERSION"));
                    std::process::exit(0);
                } else {
                    return Err(format!("Unknown parameter: -{}", name));
                }
            } else {
                // Positional argument = the pipeline script
                if args.script.is_none() {
                    args.script = Some(arg.clone());
                } else {
                    return Err(format!("Unexpected positional argument: {}", arg));
                }
            }

            i += 1;
        }

        Ok(args)
    }
}

/// Check if a parameter name matches (supports abbreviation: typed name can be prefix of full name)
fn is_param_match(typed: &str, full: &str) -> bool {
    full.starts_with(typed) || typed == full
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let argv: Vec<String> = std::env::args().collect();
    let args = Args::parse(argv).map_err(|e| format!("Argument error: {}", e))?;

    // Initialize debug mode if requested
    if args.debug {
        let file_path = args.debug_file.as_deref();
        debug::init(true, file_path)?;
        debug::log("=== JPS Debug Mode Enabled ===");
        if let Some(path) = file_path {
            debug::log(&format!("Debug output file: {}", path));
        } else {
            debug::log("Debug output: stderr");
        }
    }

    // Handle -list-commands
    if args.list_commands {
        print_command_help();
        return Ok(());
    }

    // Determine execution mode:
    // 1. If -Begin/-Process/-End are specified, run in ForEach-Object-style top-level mode
    // 2. Otherwise, use the pipeline script
    let has_lifecycle_params = args.begin.is_some() || args.process.is_some() || args.end.is_some();

    if has_lifecycle_params {
        return run_lifecycle_mode(&args);
    }

    // Standard pipeline mode
    let script = match &args.script {
        Some(s) => s.clone(),
        None => {
            eprintln!("Usage: jps '{{ <pipeline> }}' [-InputFile <file>]");
            eprintln!("Example: echo '[{{\"name\":\"alice\"}}]' | jps '{{ where name -eq alice | table }}'");
            eprintln!("\nUse -list-commands to see available commands and operators.");
            std::process::exit(1);
        }
    };

    debug::log(&format!("Script: {}", script));
    debug::log(&format!(
        "Input source: {}",
        args.input_file.as_deref().unwrap_or("stdin")
    ));
    debug::log(&format!("JSON Lines mode: {}", args.json_lines));

    // Read input
    debug::log("Reading input...");
    let input_json = read_input(&args.input_file, args.json_lines)?;
    debug::log(&format!("Input length: {} bytes", input_json.len()));
    if debug::is_enabled() && input_json.len() < 1000 {
        debug::log(&format!("Input content: {}", input_json.trim()));
    }

    // Parse JSON into Value objects
    debug::log("Parsing JSON input...");
    let input_values = parse_json_input(&input_json)?;
    debug::log(&format!("Parsed {} input object(s)", input_values.len()));

    // Parse and execute the pipeline
    debug::log("Parsing pipeline script...");
    let pipeline = Parser::parse_script(&script)
        .map_err(|e| format!("Parse error: {}", e))?;
    debug::log(&format!(
        "Pipeline contains {} command(s)",
        pipeline.commands.len()
    ));

    debug::log("Executing pipeline...");
    let executor = PipelineExecutor::new();
    let output = executor.execute(&pipeline, input_values)?;
    debug::log(&format!("Output length: {} bytes", output.len()));

    // Print output
    if !output.is_empty() {
        println!("{}", output.trim_end());
    }

    debug::log("=== Execution complete ===");
    Ok(())
}

/// Run in lifecycle mode: -Begin/-Process/-End
fn run_lifecycle_mode(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    use jps::expression;

    debug::log("Running in lifecycle mode (-Begin/-Process/-End)");

    // Read input
    let input_json = read_input(&args.input_file, args.json_lines)?;
    let input_values = parse_json_input(&input_json)?;

    let begin = args.begin.as_deref().unwrap_or("");
    let process = args.process.as_deref()
        .or(args.script.as_deref())
        .unwrap_or("");
    let end = args.end.as_deref().unwrap_or("");

    let null_val = Value::new(JsonValue::Null);

    // Execute Begin block
    if !begin.trim().is_empty() {
        let script = strip_braces(begin);
        debug::log(&format!("Lifecycle: Running Begin block: {}", script));
        match expression::evaluate(script, &null_val) {
            Ok(val) => {
                if !val.is_null() {
                    println!("{}", serde_json::to_string_pretty(&val)?);
                }
            }
            Err(e) => debug::log(&format!("Begin block error: {}", e)),
        }
    }

    // Execute Process block for each input object
    if !process.trim().is_empty() {
        let script = strip_braces(process);
        debug::log(&format!("Lifecycle: Running Process block: {}", script));

        // If the script looks like a pipeline (contains |), run it as a pipeline script
        if script.contains('|') || !script.starts_with('$') && !script.contains('*') && !script.contains('+') {
            // Might be a pipeline script - try running as pipeline
            let pipeline = Parser::parse_script(script)
                .map_err(|e| format!("Parse error: {}", e))?;
            let executor = PipelineExecutor::new();
            let output = executor.execute(&pipeline, input_values.clone())?;
            if !output.is_empty() {
                println!("{}", output.trim_end());
            }
        } else {
            // Expression mode - evaluate for each object
            for obj in &input_values {
                match expression::evaluate(script, obj) {
                    Ok(val) => {
                        if !val.is_null() {
                            let s = match &val {
                                JsonValue::String(s) => s.clone(),
                                _ => serde_json::to_string_pretty(&val)?,
                            };
                            println!("{}", s);
                        }
                    }
                    Err(e) => debug::log(&format!("Process block error: {}", e)),
                }
            }
        }
    }

    // Execute End block
    if !end.trim().is_empty() {
        let script = strip_braces(end);
        debug::log(&format!("Lifecycle: Running End block: {}", script));
        match expression::evaluate(script, &null_val) {
            Ok(val) => {
                if !val.is_null() {
                    let s = match &val {
                        JsonValue::String(s) => s.clone(),
                        _ => serde_json::to_string_pretty(&val)?,
                    };
                    println!("{}", s);
                }
            }
            Err(e) => debug::log(&format!("End block error: {}", e)),
        }
    }

    debug::log("=== Lifecycle execution complete ===");
    Ok(())
}

/// Strip outer braces from a script block string
fn strip_braces(s: &str) -> &str {
    let trimmed = s.trim();
    if trimmed.starts_with('{') && trimmed.ends_with('}') {
        trimmed[1..trimmed.len()-1].trim()
    } else {
        trimmed
    }
}

fn print_usage() {
    println!("jps {} - Process JSON with PowerShell-like pipeline syntax", env!("CARGO_PKG_VERSION"));
    println!();
    println!("USAGE:");
    println!("  jps '{{ <pipeline> }}' [-InputFile <file>]");
    println!("  jps -Process '{{ <pipeline> }}' [-Begin '{{ block }}'] [-End '{{ block }}'] [-InputFile <file>]");
    println!();
    println!("PARAMETERS:");
    println!("  -InputFile <file>   Input JSON file (aliases: -i, -In, -IF)");
    println!("  -JsonLines          Read input as JSON Lines format (alias: -l)");
    println!("  -Begin <block>      Script block to run before processing (alias: -b)");
    println!("  -Process <block>    Script block to run for each object (alias: -p)");
    println!("  -End <block>        Script block to run after processing (alias: -e)");
    println!("  -Debug              Enable debug output (alias: -d)");
    println!("  -DebugFile <file>   Write debug output to file");
    println!("  -ListCommands       Show available pipeline commands");
    println!("  -Help               Show this help");
    println!();
    println!("Use -ListCommands to see available pipeline commands and operators.");
}

fn print_command_help() {
    println!(r#"
JPS - JSON PowerShell-like Pipeline Syntax
===========================================

SYNTAX
------
  jps '{{ command1 | command2 | ... }}' [-InputFile <file>]

  - Commands are separated by | (pipe)
  - Script can be wrapped in {{ }} braces (optional)
  - Parameter names are case-insensitive: -Property == -PROPERTY == -property
  - Parameter names can be abbreviated: -Property == -Prop == -P
  - Property names are case-insensitive with wildcard support: user* matches UserName, UserID
  - Use $_ or $this to reference the current object in expressions
  - Method calls: $_.Name.ToUpper(), $_.Handles.floor()

AVAILABLE COMMANDS
------------------

Where-Object (aliases: where, ?)
  Filter objects based on property values.

  Syntax: where <property> <operator> <value>

  Examples:
    where age -gt 30
    where name -eq 'alice'
    where status -like 'active*'
    where tags -contains 'important'

Select-Object (aliases: select)
  Select specific properties or limit number of objects.
  Supports calculated properties for creating new computed values.

  Parameters:
    -Property <names>       Properties to select (supports wildcards and calculated)
    -ExpandProperty <name>  Expand a property (array or object) into output objects
    -First <n>              Take first n objects
    -Last <n>               Take last n objects
    -Skip <n>               Skip first n objects

  Examples:
    select name, age
    select user*                    # Wildcard: UserName, UserID, etc.
    select -First 10
    select -Property name -Last 5

  ExpandProperty (collision: renamed with 'O' prefix):
    select -ExpandProperty tags
    select -ExpandProperty address
    select -ExpandProperty orders -Property customerId

  Calculated Properties:
    select name, @{{Name='Total'; Expression='$_.price * $_.quantity'}}
    select @{{n='FullName'; e='$_.firstName + " " + $_.lastName'}}
    select @{{Name='Upper'; Expression='$_.name.ToUpper()'}}

Sort-Object (aliases: sort)
  Sort objects by property values.

  Parameters:
    -Property <names>   Properties to sort by
    -Descending         Sort in descending order (alias: -Desc)
    -Unique             Remove duplicates
    -CaseSensitive      Case-sensitive string comparison

  Examples:
    sort age
    sort name -Descending
    sort -Property date, name -Desc

Format-Table (aliases: ft, table)
  Display output as a table. Supports calculated properties.

  Parameters:
    -Property <names>   Properties to display (can include @{{...}} calculated columns)
    -AutoSize           Auto-size column widths
    -HideTableHeaders   Hide column headers

  Examples:
    table
    table name, age -AutoSize
    table name, @{{name='TEST'; expression='handles'; width=15}}
    table -property name, @{{n='Total'; e='price * qty'; alignment='right'}}

Format-List (aliases: fl, list)
  Display output as a property list. Supports calculated properties.

  Parameters:
    -Property <names>   Properties to display (can include @{{...}} calculated properties)

  Examples:
    list
    list name, age
    list @{{name='Total'; expression='price * qty'}}

ForEach-Object (aliases: foreach, %)
  Perform an operation on each input object.

  Parameters:
    -Begin   {{block}}    Script block run once before processing
    -Process {{block}}    Script block run for each input object (default)
    -End     {{block}}    Script block run once after processing
    -MemberName <name>  Get named property or call named method on each object

  Examples:
    foreach {{ $_ / 1024 }}
    foreach {{ $_.name.ToUpper() }}
    foreach -MemberName name
    foreach -Begin {{ 'Starting' }} -Process {{ $_.value * 2 }} -End {{ 'Done' }}
    "Microsoft.PowerShell.Core" | foreach {{ $_.Split('.') }}

Measure-Object (aliases: measure)
  Calculate statistics on numeric properties.

  Parameters:
    -Property <name>    Property to measure
    -Sum                Calculate sum
    -Average            Calculate average (alias: -Avg)
    -Minimum            Calculate minimum (alias: -Min)
    -Maximum            Calculate maximum (alias: -Max)

  Examples:
    measure
    measure value -Sum -Average
    measure price -Sum -Min -Max

ConvertTo-Json (aliases: json)
  Output as JSON.

  Parameters:
    -Depth <n>          Maximum depth (default: 100)
    -Compress           Output without formatting

  Examples:
    json
    json -Compress
    json -Depth 3

ConvertTo-Csv (aliases: csv)
  Output as CSV.

  Parameters:
    -Delimiter <char>   Field delimiter (default: ,)
    -NoTypeInformation  Omit header row (alias: -NoHeader)

  Examples:
    csv
    csv -Delimiter ';'
    csv -NoHeader

COMPARISON OPERATORS (for Where-Object)
---------------------------------------

Equality:
  -eq         Equal (case-insensitive)
  -ceq        Equal (case-sensitive)
  -ne         Not equal (case-insensitive)
  -cne        Not equal (case-sensitive)

Comparison:
  -gt         Greater than
  -ge         Greater than or equal
  -lt         Less than
  -le         Less than or equal
  (Add 'c' prefix for case-sensitive: -cgt, -cge, -clt, -cle)

Pattern Matching:
  -like       Wildcard match (* and ?)
  -notlike    Wildcard non-match
  -match      Regex match
  -notmatch   Regex non-match
  (Add 'c' prefix for case-sensitive: -clike, -cmatch, etc.)

Containment:
  -contains      Collection contains value
  -notcontains   Collection does not contain value
  -in            Value is in collection
  -notin         Value is not in collection
  (Add 'c' prefix for case-sensitive)

EXPRESSION SYNTAX
-----------------

Use $_ or $this to reference the current object in expressions.
Properties accessed with . notation: $_.Name, $this.Age

Method calls (PowerShell style):
  $_.Name.ToUpper()       String to uppercase
  $_.Name.ToLower()       String to lowercase
  $_.Name.Trim()          Remove whitespace
  $_.Name.Length          String/array length
  $_.Name.Split('.')      Split string into array
  $_.Name.Substring(0,3)  Extract substring
  $_.Value.Floor()        Math floor
  $_.Value.Ceiling()      Math ceiling
  $_.Value.Round()        Math round
  $_.Value.Abs()          Absolute value

Function calls (traditional style also supported):
  upper($_.Name)   Convert to uppercase
  lower($_.Name)   Convert to lowercase
  len($_.Name)     String/array length
  floor($_.Value)  Math floor
  ceil($_.Value)   Math ceiling
  round($_.Value)  Math round

EXAMPLES
--------

# Filter and format as table
echo '[{{"name":"alice","age":30}},{{"name":"bob","age":25}}]' | jps '{{ where age -gt 26 | table }}'

# Select properties and sort
jps '{{ select name, age | sort age -desc }}' -InputFile data.json

# Get statistics
jps '{{ measure salary -Sum -Average -Min -Max }}' -i employees.json

# Complex pipeline
jps '{{ where status -eq active | select name, email | sort name | table -AutoSize }}'

# Calculated properties with PowerShell notation
jps '{{ select name, @{{Name="Total"; Expression="$_.price * $_.quantity"}} | table }}' -i orders.json

# ForEach expression
jps '{{ foreach {{ $_.price * 1.1 }} }}' -i items.json

# Top-level lifecycle mode
echo '[1,2,3,4,5]' | jps -Process '{{ foreach {{ $_ * 2 }} }}'
"#);
}

fn read_input(
    input_file: &Option<String>,
    json_lines: bool,
) -> Result<String, Box<dyn std::error::Error>> {
    let content = if let Some(path) = input_file {
        std::fs::read_to_string(path)?
    } else {
        // Read from stdin
        let mut buffer = String::new();

        // Check if stdin is a terminal (no piped input)
        if atty::is(atty::Stream::Stdin) {
            // No piped input, return empty array
            return Ok("[]".to_string());
        }

        io::stdin().lock().read_to_string(&mut buffer)?;
        buffer
    };

    // If JSON Lines mode, wrap in array
    if json_lines {
        let lines: Vec<&str> = content
            .lines()
            .filter(|l| !l.trim().is_empty())
            .collect();
        Ok(format!("[{}]", lines.join(",")))
    } else {
        Ok(content)
    }
}

fn parse_json_input(json: &str) -> Result<Vec<Value>, Box<dyn std::error::Error>> {
    let json = json.trim();

    if json.is_empty() {
        return Ok(Vec::new());
    }

    let parsed: serde_json::Value = serde_json::from_str(json)?;

    match parsed {
        serde_json::Value::Array(arr) => {
            Ok(arr.into_iter().map(Value::new).collect())
        }
        other => {
            // Single object, wrap in vec
            Ok(vec![Value::new(other)])
        }
    }
}
