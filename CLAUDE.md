# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**jps** is a CLI utility written in Rust that processes JSON documents using PowerShell-like pipeline syntax. It accepts JSON input from files (`-InputFile`) or stdin, converts it to objects, and executes a pipeline of cmdlet-like utilities.

## Key Design Requirements

### Case Insensitivity
- All parameter names are case-insensitive: `-INPUTFILE` == `-inputfile` == `-InputFile`
- All property access is case-insensitive with wildcard support: `USER*` matches `USERNAME`, `USERDIR`, etc.
- Cmdlet names are case-insensitive: `WHERE-OBJECT` == `where-object`

### Parameter Abbreviation
- Parameters can be partially specified: `-InputFile` == `-Input` == `-In`
- Must handle ambiguous abbreviations appropriately

### Cmdlet Name Shortcuts
- `Format-<X>` cmdlets can be called as just `<X>`: `Format-Table` == `Table`
- `<X>-Object` cmdlets can be called as just `<X>`: `Where-Object` == `Where`

## Supported Utilities

The following PowerShell-like cmdlets must be implemented:

| Cmdlet | Aliases | Purpose |
|--------|---------|---------|
| Format-Table | ft, Table | Tabular output formatting |
| Format-List | fl, List | List-style output formatting |
| Select-Object | select, Select, so | Property selection, First/Last/Skip,  Property Expansion |
| Where-Object | where, ?, Where, wo | Filtering with comparison operators |
| Sort-Object | sort, Sort | Sorting by properties |
| Measure-Object | measure, Measure | Statistics (Sum, Average, Min, Max, Count) |
| ConvertTo-Json | tojson, tj | Output as JSON |
| ConvertTo-Csv | tocsv, tc | Output as CSV |
| Group-Object | go, Group | organizes objects in groups based on the value of a specified property. `Group-Object` returns a table with one row for each property value and a column that displays the number of items with that value.

    If you specify more than one property, `Group-Object` first groups them by the values of the first property, and then, within each property group, it groups by the value of the next
    property. 

## Property Expansion (`-ExpandProperty`)

When using `-ExpandProperty <String>` within `Select-Object`, follow these strict behavioral rules:

* Type Transformation: If the property is an array, output each value individually. If it is an object, expand its properties for every input object. The output type must match the expanded property's type.
* Error Handling: If the specified property does not exist, return an error.
* Collision Constraints: Property expansion cannot replace existing properties. If the expanded object or the selected object contains a property with the same name then rename the original property by prepending the string "O" for Original.  e.g. Name become OName.
* Side Effects: If `-Property` is also specified, attempt to add those selected properties as members to every outputted object.


## Comparison Operators for Where-Object

Case-insensitive (default): `-eq`, `-ne`, `-gt`, `-ge`, `-lt`, `-le`, `-like`, `-notlike`, `-match`, `-notmatch`, `-contains`, `-notcontains`, `-in`, `-notin`

Case-sensitive variants: `-ceq`, `-cne`, `-cgt`, `-cge`, `-clt`, `-cle`, `-clike`, `-cnotlike`, `-cmatch`, `-cnotmatch`, `-ccontains`, `-cnotcontains`, `-cin`, `-cnotin`

## Example Pipeline Syntax

```bash
# Full syntax 
somecommand | jps { Where-Object handlecount -gt 1000 | Select-Object name, id, handlecount | Sort-Object handlecount -Descending | Format-Table -AutoSize }

# Abbreviated syntax
somecommand | jps { where handle* -gt 1000 | select N*, id, H* | sort handlecount -desc | table -autosize }
```

## Build Commands

```bash
cargo build              # Debug build
cargo build --release    # Release build
cargo test               # Run all tests
cargo test <test_name>   # Run specific test
cargo clippy             # Lint
cargo fmt                # Format code
```

## Architecture Notes

Key components to implement:
1. **Parser** - PowerShell-like script parsing with parameter binding
2. **JSON Engine** - Deserialization and object model
3. **Pipeline Executor** - Streaming pipeline with lazy evaluation where possible
4. **Cmdlets** - Individual cmdlet implementations
5. **Property Resolver** - Case-insensitive property access with wildcard matching
6. **Output Formatters** - Table, List, JSON, CSV rendering
