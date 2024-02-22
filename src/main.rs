use std::fmt::Debug;
use std::io::Error;
use std::{fmt, fs};
use std::{fmt::Display, fmt::Formatter};

use clap::{Parser, Subcommand, ValueEnum};
use colored::Colorize;
use nickel_lang_core::error::report::ErrorFormat;
use nickel_lang_core::serialize::{to_string, ExportFormat};
use nickel_lang_core::{eval::cache::CacheImpl, program::Program};

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
  #[command(subcommand)]
  command: Commands,
}

#[derive(Debug, Clone, ValueEnum, PartialEq)]
enum Language {
  Json,
  Purescript,
  Haskell,
  Elm,
  Rust,
}

impl Display for Language {
  fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
    match *self {
      Language::Json => write!(formatter, "json"),
      Language::Purescript => write!(formatter, "purescript"),
      Language::Haskell => write!(formatter, "haskell"),
      Language::Elm => write!(formatter, "elm"),
      Language::Rust => write!(formatter, "rust"),
    }
  }
}

#[derive(Subcommand)]
enum Commands {
  /// Reads `oclis.ncl`, detects main programming language of project,
  /// writes corresponding parsing code to source directory
  Build {
    /// The path to the spec file
    #[arg(default_value = "./oclis.ncl")]
    spec_file: Option<String>,
    //
    // TODO: Reactivate when the `language` argument is used
    // #[arg(short, long, default_value_t = Language::Json)]
    // language: Language,
  },
  /// Creates a bare bones `oclis.ncl` file
  Init,
}

fn convert_spec_to_json(
  prog: &mut Program<CacheImpl>,
) -> Result<String, String> {
  let richterm_res = prog.eval_full_for_export();

  match richterm_res {
    Ok(richterm) => match to_string(ExportFormat::Json, &richterm) {
      Ok(json_spec) => Ok(json_spec),
      Err(error) => {
        prog.report(error, ErrorFormat::Text);
        Err(String::from(""))
      }
    },
    Err(error) => {
      prog.report(error, ErrorFormat::Text);
      Err(String::from(""))
    }
  }
}

#[rustfmt::skip]
macro_rules! OCLIS_SRC { () => { "src/Oclis/" }; }
macro_rules! include_and_write_file {
  ($filename:expr) => {{
    std::fs::write(
      concat!(OCLIS_SRC!(), $filename),
      include_str!(concat!("../purescript/", OCLIS_SRC!(), $filename))
        .replace("{{version}}", VERSION),
    )?;
  }};
}

// TODO: Check if the code is really in `src` or in another directory
fn write_purescript_code(json_spec: String) -> Result<(), Error> {
  fs::create_dir_all(OCLIS_SRC!())?;

  include_and_write_file!("readme.md");
  include_and_write_file!("Types.purs");
  include_and_write_file!("Tokenizer.purs");
  include_and_write_file!("Parser.purs");
  include_and_write_file!("Executor.purs");

  let spec_embed = format!(
    "\
      -- | CAUTION:\n\
      -- | THIS FILE WAS GENERATED BASED ON `oclis.ncl`.\n\
      -- | DO NOT EDIT MANUALLY!\n\
      \n\
      module Oclis.SpecEmbed where\n\
      \n\
      fileContent :: String\n\
      fileContent = \"\"\"\n\
        {}\n\
        \"\"\"\n\
    ",
    json_spec
  );
  fs::write(concat!(OCLIS_SRC!(), "SpecEmbed.purs"), spec_embed)?;

  println!(
    "{}",
    concat!("✅ Created PureScript files at \"./", OCLIS_SRC!(), "\"").green()
  );

  Ok(())
}

// TODO: Also check if directory name starts with `purescript`
// Misusing the std::io Error here type for simplicity's sake
fn if_purescript_write_files(json_spec: String) -> Result<(), Error> {
  if [
    "spago.yaml",
    "spago.dhall",
    ".purs.repl",
    "bower.json", // TODO: Check if the file includes PureScript dependencies
  ]
  .iter()
  .any(|file| fs::metadata(file).is_ok())
  {
    write_purescript_code(json_spec)
  } else {
    Err(Error::new(
      std::io::ErrorKind::NotFound,
      "No PureScript files found",
    ))
  }
}

// Misusing the std::io Error here type for simplicity's sake
fn if_haskell_write_files() -> Result<(), Error> {
  if [
    "stack.yaml", //
    "Setup.hs",
    "cabal.project",
  ]
  .iter()
  .any(|file| fs::metadata(file).is_ok())
  {
    Err(Error::new(
      std::io::ErrorKind::InvalidData,
      "Haskell is not yet supported",
    ))
  } else {
    Err(Error::new(
      std::io::ErrorKind::NotFound,
      "No Haskell files found",
    ))
  }
}

// Misusing the std::io Error here type for simplicity's sake
fn if_javascript_write_files() -> Result<(), Error> {
  if [
    "package.json", //
    "package-lock.json",
  ]
  .iter()
  .any(|file| fs::metadata(file).is_ok())
  {
    Err(Error::new(
      std::io::ErrorKind::InvalidData,
      "JavaScript is not yet supported",
    ))
  } else {
    Err(Error::new(
      std::io::ErrorKind::NotFound,
      "No JavaScript files found",
    ))
  }
}

fn detect_language_and_write_files(json_spec: String) -> Result<(), Error> {
  fn try_next(
    test_func: impl FnOnce() -> Result<(), Error>,
    err: Error,
  ) -> Result<(), Error> {
    match err.kind() {
      std::io::ErrorKind::NotFound => test_func(),
      _ => Err(err),
    }
  }

  if_purescript_write_files(json_spec)
    .or_else(|err| try_next(if_haskell_write_files, err))
    .or_else(|err| try_next(if_javascript_write_files, err))
    .map_err(|err| match err.kind() {
      std::io::ErrorKind::NotFound => Error::new(
        std::io::ErrorKind::InvalidData,
        "No supported language was detected",
      ),
      _ => err,
    })
}

fn load_and_serialize_spec(rel_path: &str) -> Result<String, String> {
  const NICKEL_CLI_CONTRACT: &str = include_str!("../oclis-contract.ncl");
  let user_spec_file_res = fs::read_to_string(rel_path);

  match user_spec_file_res {
    Err(error) => {
      let err_msg = format!("Error while opening \"{}\"", rel_path);
      eprintln!("{}", err_msg.red());
      Err(error.to_string())
    }
    Ok(user_spec_file) => {
      let combined_spec = format!(
        "({}) & ({})",
        user_spec_file,
        NICKEL_CLI_CONTRACT //
      );

      let combined_spec_reader = std::io::Cursor::new(combined_spec);
      let prog_res = Program::new_from_source(
        combined_spec_reader,
        "combined spec",
        std::io::stderr(),
      );

      match prog_res {
        Ok(mut prog) => convert_spec_to_json(&mut prog),
        Err(error) => Err(error.to_string()),
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_load_and_serialize() {
    let json_str_res = load_and_serialize_spec("./oclis.ncl");
    assert_eq!(json_str_res.is_ok(), true);
  }
}

// TODO: Return a custom error type and implement the Termination trait
fn main() -> Result<(), String> {
  let cli = Cli::parse();

  match cli.command {
    Commands::Build {
      spec_file,
      // language,
    } => {
      // if language != Language::Json {
      //   println!("Value for language: {:?}", language);
      // }

      match spec_file.as_deref() {
        Some(spec_file) => {
          let json_spec = load_and_serialize_spec(spec_file)?;
          detect_language_and_write_files(json_spec)
            .map_err(|err| err.to_string())
        }
        None => Err(String::from("No spec file provided")),
      }
    }

    Commands::Init => {
      let cwd_path = std::env::current_dir().unwrap();
      let cwd = cwd_path.file_name().unwrap();
      let cwd_str = cwd.to_str().unwrap();

      if fs::metadata("oclis.ncl").is_ok() {
        Err(String::from("Spec file 'oclis.ncl' already exists"))
      } else {
        const TEMPLATE: &str = include_str!("template.ncl");
        let spec = TEMPLATE.replace("{{name}}", cwd_str);
        fs::write("oclis.ncl", spec).map_err(|err| err.to_string())
      }
    }
  }
}
