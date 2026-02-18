use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn test_basic_where_filter() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ where age -gt 26 }")
        .write_stdin(r#"[{"name":"alice","age":30},{"name":"bob","age":25}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("alice"))
        .stdout(predicate::str::contains("30"));
}

#[test]
fn test_select_properties() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ select name }")
        .write_stdin(r#"[{"name":"alice","age":30}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("alice"))
        .stdout(predicate::str::contains("age").not());
}

#[test]
fn test_sort_descending() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ sort age -desc }")
        .write_stdin(r#"[{"name":"alice","age":30},{"name":"bob","age":25}]"#)
        .assert()
        .success();
}

#[test]
fn test_format_table() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ table }")
        .write_stdin(r#"[{"name":"alice","age":30}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("name"))
        .stdout(predicate::str::contains("alice"));
}

#[test]
fn test_format_list() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ list }")
        .write_stdin(r#"[{"name":"alice","age":30}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("name : alice"));
}

#[test]
fn test_measure_object() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ measure value -Sum -Average }")
        .write_stdin(r#"[{"value":10},{"value":20},{"value":30}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("60"))
        .stdout(predicate::str::contains("20"));
}

#[test]
fn test_convert_to_csv() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ csv }")
        .write_stdin(r#"[{"name":"alice","age":30}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains(","))
        .stdout(predicate::str::contains("alice"));
}

#[test]
fn test_chained_pipeline() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ where age -gt 26 | select name | table }")
        .write_stdin(r#"[{"name":"alice","age":30},{"name":"bob","age":25}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("alice"))
        .stdout(predicate::str::contains("bob").not());
}

#[test]
fn test_case_insensitive_property() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ where NAME -eq alice }")
        .write_stdin(r#"[{"name":"alice"}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("alice"));
}

#[test]
fn test_wildcard_property() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ select user* }")
        .write_stdin(r#"[{"UserName":"alice","UserID":123,"Email":"test@test.com"}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("alice"))
        .stdout(predicate::str::contains("123"))
        .stdout(predicate::str::contains("Email").not());
}

#[test]
fn test_like_operator() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ where name -like 'a*' }")
        .write_stdin(r#"[{"name":"alice"},{"name":"bob"}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("alice"))
        .stdout(predicate::str::contains("bob").not());
}

#[test]
fn test_input_file() {
    use std::io::Write;

    let temp_dir = std::env::temp_dir();
    let input_file = temp_dir.join("jps_test_input.json");

    let mut file = std::fs::File::create(&input_file).unwrap();
    writeln!(file, r#"[{{"name":"test"}}]"#).unwrap();

    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ select name }")
        .arg("-i")
        .arg(&input_file)
        .assert()
        .success()
        .stdout(predicate::str::contains("test"));

    std::fs::remove_file(input_file).ok();
}

#[test]
fn test_single_object_input() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ select name }")
        .write_stdin(r#"{"name":"alice","age":30}"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("alice"));
}

#[test]
fn test_parameter_abbreviation() {
    let mut cmd = Command::cargo_bin("jps").unwrap();

    cmd.arg("{ select -Prop name }")
        .write_stdin(r#"[{"name":"alice","age":30}]"#)
        .assert()
        .success()
        .stdout(predicate::str::contains("alice"))
        .stdout(predicate::str::contains("age").not());
}
