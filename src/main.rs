use std::fmt::Debug;
use std::{fmt, fs};
use std::{fmt::Display, fmt::Formatter};

use clap::{Parser, Subcommand, ValueEnum};
use colored::Colorize;
use nickel_lang_core::error::report::ErrorFormat;
use nickel_lang_core::serialize::{to_string, ExportFormat};
use nickel_lang_core::{eval::cache::CacheImpl, program::Program};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
  #[command(subcommand)]
  command: Commands,
}

#[derive(Debug, Clone, ValueEnum, PartialEq)]
enum Language {
  JSON,
  // // TODO: Add support for other languages
  // Purescript,
  // Haskell,
  // Elm,
  // Rust,
}

impl Display for Language {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match *self {
      Language::JSON => write!(f, "json"),
      // // TODO: Add support for other languages
      // Language::Purescript => write!(f, "purescript"),
      // Language::Haskell => write!(f, "haskell"),
      // Language::Elm => write!(f, "elm"),
      // Language::Rust => write!(f, "rust"),
    }
  }
}

#[derive(Subcommand)]
enum Commands {
  /// Build the oclis.json file
  Build {
    /// The path to the spec file
    #[arg(default_value = "./oclis.ncl")]
    spec_file: Option<String>,

    #[arg(short, long, default_value_t = Language::JSON)]
    language: Language,
  },
}

fn export_spec_as_json(prog: &mut Program<CacheImpl>) -> Result<(), String> {
  let richterm_res = prog.eval_full_for_export();

  match richterm_res {
    Ok(richterm) => match to_string(ExportFormat::Json, &richterm) {
      Ok(json_str) => match fs::write("oclis.json", json_str + "\n") {
        Ok(_) => {
          println!("{}", "Successfully created \"oclis.json\" file.".green());
          Ok(())
        }
        Err(error) => Err(error.to_string()),
      },
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

fn load_and_serialize(rel_path: &str) -> Result<(), String> {
  let nickel_cli_contract = include_str!("../oclis-contract.ncl");
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
        nickel_cli_contract //
      );

      let combined_spec_reader = std::io::Cursor::new(combined_spec);
      let prog_res = Program::new_from_source(
        combined_spec_reader,
        "combined spec",
        std::io::stderr(),
      );

      match prog_res {
        Ok(mut prog) => export_spec_as_json(&mut prog),
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
    assert_eq!(load_and_serialize("oclis.ncl"), Ok(()),);
  }
}

// TODO: Return a custom error type and implement the Termination trait
fn main() -> Result<(), String> {
  let cli = Cli::parse();

  match cli.command {
    Commands::Build {
      spec_file,
      language,
    } => {
      if language != Language::JSON {
        println!("Value for language: {:?}", language);
      }

      // TODO: Return Result instead of ExitCodes
      match spec_file.as_deref() {
        Some(spec_file) => load_and_serialize(spec_file),
        None => Err(String::from("No spec file provided")),
      }
    }
  }
}
