use crate::ast::ModuleName;
use std::cmp::{max, min};
use std::fmt;
use std::fs;
use std::io;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::slice::SliceIndex;
use std::str::CharIndices;

pub enum Error {
    InvalidExtension(String),
    InvalidFileName(String),
    IO(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            InvalidExtension(extension) => write!(
                f,
                "Invalid file extension '{extension}'. Please use the '.alma' extension",
            ),
            InvalidFileName(name) => write!(
                f,
                "Invalid file name '{name}'. Alma file names must look like this 'MyFile.alma'",
            ),
            IO(err) => write!(f, "There was an error reading the file.\n\n{err}"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum SourceOrigin {
    File(PathBuf),
    NotAFile,
}

type LinesAround<'str> = (Vec<&'str str>, (usize, Vec<&'str str>), Vec<&'str str>);

#[derive(Debug, PartialEq)]
pub struct Source {
    source: SourceOrigin,
    code: String,
}

impl Source {
    pub fn new_file(file: String) -> Result<Self, Error> {
        let path = Path::new(&file).to_path_buf();

        Source::validate_file_name(&path)?;
        let code = Source::read_file(&path)?;

        Ok(Source {
            source: SourceOrigin::File(path),
            code,
        })
    }

    fn validate_file_name(file_path: &Path) -> Result<(), Error> {
        let file_name = file_path
            .file_stem()
            // Convert OSStr to str
            .and_then(|s| s.to_str())
            .unwrap_or_default();
        let extension = file_path
            .extension()
            // Convert OSStr to str
            .and_then(|s| s.to_str())
            .unwrap_or_default();

        if extension != "alma" {
            Err(Error::InvalidExtension(extension.to_string()))
        } else if !ModuleName::valid_part(file_name) {
            Err(Error::InvalidFileName(file_name.to_string()))
        } else {
            Ok(())
        }
    }

    fn read_file(file_path: &Path) -> Result<String, Error> {
        fs::read_to_string(file_path).map_err(Error::IO)
    }

    pub fn new_orphan(code: String) -> Self {
        Source {
            source: SourceOrigin::NotAFile,
            code,
        }
    }

    pub fn name(&self) -> &str {
        match &self.source {
            SourceOrigin::File(path) => path.to_str().unwrap(),
            SourceOrigin::NotAFile => "",
        }
    }

    pub fn file_stem(&self) -> Option<&str> {
        match &self.source {
            SourceOrigin::File(path) => Some(path.file_stem().unwrap().to_str().unwrap()),
            SourceOrigin::NotAFile => None,
        }
    }

    pub fn _char_at(&self, i: usize) -> Option<char> {
        self.code.get(i..).and_then(|code| code.chars().next())
    }

    pub fn text_at<I>(&self, i: I) -> Option<&<I as SliceIndex<str>>::Output>
    where
        I: SliceIndex<str>,
    {
        self.code.get(i)
    }

    fn _line_at(&self, position: usize) -> Option<&str> {
        self.code.get(line_at(&self.code, position)?)
    }

    pub fn lines_around_position(
        &self,
        position: usize,
        end_position: Option<usize>,
        number_of_lines: u32,
    ) -> Option<LinesAround> {
        lines_around_position(&self.code, position, end_position, number_of_lines)
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn char_indices(&self) -> CharIndices {
        self.code.char_indices()
    }

    pub fn lines_report_at_position(
        &self,
        position: usize,
        end_position: Option<usize>,
        line_number: u32,
        show_pointer: bool,
    ) -> Option<String> {
        if self.len() == 0 {
            return None;
        }

        let mut message = String::new();

        let point_to_end_of_input = position == self.len();
        let (position, end_position) = if point_to_end_of_input {
            (position - 1, None)
        } else {
            (position, end_position)
        };

        let (lines_before, (lines_start_position, lines), lines_after) =
            self.lines_around_position(position, end_position, 2)?;

        let line_number_width: usize = (line_number as usize + lines.len() + lines_after.len())
            .to_string()
            .len();

        let column_number = position - lines_start_position;

        for (i, l) in lines_before.iter().enumerate() {
            if !message.is_empty() {
                message.push('\n');
            }
            message.push_str(&format!(
                "  {line_number:line_number_width$}│  {l}",
                line_number = line_number - lines_before.len() as u32 + i as u32,
            ));
        }

        match lines.as_slice() {
            [] => panic!("Empty code for report, should've bailed earlier at line_at"),
            [line] => {
                if !message.is_empty() {
                    message.push('\n');
                }
                message.push_str(&format!("  {line_number:line_number_width$}│  {line}\n"));

                if show_pointer {
                    let num_pointers = max(1, end_position.unwrap_or(position) - position);

                    let num_spaces = if point_to_end_of_input {
                        column_number + 1
                    } else {
                        column_number
                    };
                    // In the case input ends with \n and we point to it, num_spaces here will end
                    // up being off the line by 1 position. Instead of checking somehow if it ended
                    // in \n, and it was point_to_end_of_input, just cap the pointer to point to
                    // the end of line at the furthest.
                    let num_spaces = min(line.len(), num_spaces);

                    message.push_str(&format!(
                        "  {space:line_number_width$}│  {spaces}{pointers}",
                        space = ' ',
                        spaces = str::repeat(" ", num_spaces as usize),
                        pointers = str::repeat("↑", num_pointers as usize)
                    ));
                }
            }
            lines => {
                for (i, l) in lines.iter().enumerate() {
                    if !message.is_empty() {
                        message.push('\n');
                    }
                    message.push_str(&format!(
                        "  {line_number:line_number_width$}│{line_marker} {l}",
                        line_number = line_number + i as u32,
                        line_marker = if show_pointer { "→" } else { " " },
                    ));
                }
            }
        }

        for (i, l) in lines_after.iter().enumerate() {
            message.push('\n');
            message.push_str(&format!(
                "  {line_number:line_number_width$}│  {l}",
                line_number = line_number + 1 + i as u32,
            ));
        }
        Some(message)
    }

    pub fn lines_report_at_position_with_pointer(
        &self,
        position: usize,
        end_position: Option<usize>,
        line_number: u32,
    ) -> Option<String> {
        self.lines_report_at_position(position, end_position, line_number, true)
    }

    pub fn to_string_with_line_and_col(&self, line: u32, column: u32) -> String {
        let file_name = match &self.source {
            SourceOrigin::File(path) => path.to_str().unwrap(),
            SourceOrigin::NotAFile => "",
        };
        let space = if file_name.is_empty() { "" } else { " " };
        format!("{file_name}{space}[{line}:{column}]")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    fn report(code: &str, position: usize, end_position: Option<usize>, lines: u32) -> String {
        let source = Source::new_orphan(code.to_string());
        let result = source.lines_report_at_position_with_pointer(position, end_position, lines);
        format!(
            "Input:\n\n{code}\n\nResult:\n\n{result}",
            result = match &result {
                Some(res) => res,
                None => "None",
            }
        )
    }

    #[test]
    fn test_pointing_to_the_end_of_file_points_to_last_character_even_with_new_lines() {
        assert_snapshot!(report("type Banana\n", 12, None, 1));
    }
}

fn line_at(code: &str, position: usize) -> Option<Range<usize>> {
    if position >= code.len() {
        return None;
    }

    let line_start = {
        let back = code.get(..position)?;
        if back.is_empty() {
            position
        } else {
            back.char_indices()
                .rfind(|(i, c)| *c == '\n' || *i == 0)
                .map(|(i, c)| if i == 0 && c != '\n' { i } else { i + 1 })?
        }
    };

    // Exclusive end index
    let line_end = {
        let front = code.get(position..)?;
        front
            .char_indices()
            .find(|(i, c)| *c == '\n' || *i == front.len() - 1)
            .map(|(i, c)| {
                (if i == front.len() - 1 && c != '\n' {
                    i + 1
                } else {
                    i
                }) + position
            })?
    };

    Some(line_start..line_end)
}

#[test]
fn test_line_at() {
    let tests = vec![
        ("", 0, None),
        ("\n", 0, Some("")),
        ("\n", 1, None),
        ("\n\n", 0, Some("")),
        ("\n\n", 1, Some("")),
        ("a", 0, Some("a")),
        ("a\n", 1, Some("a")),
        ("\na", 1, Some("a")),
        ("\n\na", 1, Some("")),
        ("\n\na", 2, Some("a")),
        ("\n\na\n", 2, Some("a")),
        ("hello\nhi\nbye", 0, Some("hello")),
        ("hello\nhi\nbye", 2, Some("hello")),
        ("hello\nhi\nbye", 4, Some("hello")),
        ("hello\nhi\nbye", 5, Some("hello")),
        ("hello\nhi\nbye", 6, Some("hi")),
        ("hello\nhi\nbye", 7, Some("hi")),
        ("hello\nhi\nbye", 8, Some("hi")),
        ("hello\nhi\nbye", 9, Some("bye")),
        ("hello\nhi\nbye", 10, Some("bye")),
        ("hello\nhi\nbye", 11, Some("bye")),
    ];

    for (s, i, expected) in tests {
        let line = line_at(s, i).and_then(|i| s.get(i));
        assert_eq!(line, expected, "text = {:?}, get line at char {}", s, i);
    }
}

fn lines_before_position(code: &str, position: usize, number_of_lines: u32) -> Option<Vec<&str>> {
    let mut lines = Vec::new();
    let mut how_many_lines = number_of_lines;

    if position > 0 && how_many_lines > 0 {
        let mut idx = position - 1;
        while let Some(bounds) = line_at(code, idx) {
            let start = bounds.start;
            lines.push(code.get(bounds)?);
            how_many_lines -= 1;
            if start == 0 || how_many_lines == 0 {
                break;
            }
            idx = start - 1;
        }
        lines.reverse();
    }

    Some(lines)
}

fn lines_after_position(code: &str, position: usize, number_of_lines: u32) -> Option<Vec<&str>> {
    let mut lines = Vec::new();
    let mut how_many_lines = number_of_lines;

    if position < code.len() - 1 && how_many_lines > 0 {
        let mut idx = position + 1;
        while let Some(bounds) = line_at(code, idx) {
            let end = bounds.end;
            lines.push(code.get(bounds)?);
            how_many_lines -= 1;
            if end >= code.len() - 1 || how_many_lines == 0 {
                break;
            }
            idx = end + 1;
        }
    }

    Some(lines)
}

fn lines_around_position(
    code: &str,
    position: usize,
    end_position: Option<usize>,
    number_of_lines: u32,
) -> Option<LinesAround> {
    let line_bounds = line_at(code, position)?;

    match end_position {
        Some(end) => {
            // end is exclusive, so substract 1 to get the line at the actual last character
            let end_line_bounds = line_at(code, end - 1)?;

            let lines = code
                .get(line_bounds.start..end_line_bounds.end)?
                .lines()
                .collect();

            let lines_before = lines_before_position(code, line_bounds.start, number_of_lines)?;

            let lines_after = lines_after_position(code, end_line_bounds.end, number_of_lines)?;

            Some((lines_before, (line_bounds.start, lines), lines_after))
        }
        None => {
            let line = code.get(line_bounds.clone())?;

            let lines_before = lines_before_position(code, line_bounds.start, number_of_lines)?;

            let lines_after = lines_after_position(code, line_bounds.end, number_of_lines)?;

            Some((lines_before, (line_bounds.start, vec![line]), lines_after))
        }
    }
}

#[test]
fn test_lines_around_position() {
    let tests = vec![
        ("", 0, None, 1, None),
        ("\n", 0, None, 1, Some((vec![], (0, vec![""]), vec![]))),
        ("\n\n", 0, None, 1, Some((vec![], (0, vec![""]), vec![""]))),
        ("\n\n", 1, None, 1, Some((vec![""], (1, vec![""]), vec![]))),
        ("a", 0, None, 1, Some((vec![], (0, vec!["a"]), vec![]))),
        ("a\n", 0, None, 1, Some((vec![], (0, vec!["a"]), vec![]))),
        ("a\n", 1, None, 1, Some((vec![], (0, vec!["a"]), vec![]))),
        ("\na", 0, None, 1, Some((vec![], (0, vec![""]), vec!["a"]))),
        ("\na", 1, None, 1, Some((vec![""], (1, vec!["a"]), vec![]))),
        ("\n\na", 0, None, 1, Some((vec![], (0, vec![""]), vec![""]))),
        (
            "\n\na",
            0,
            None,
            2,
            Some((vec![], (0, vec![""]), vec!["", "a"])),
        ),
        (
            "\n\na",
            0,
            None,
            3,
            Some((vec![], (0, vec![""]), vec!["", "a"])),
        ),
        (
            "\n\na",
            1,
            None,
            1,
            Some((vec![""], (1, vec![""]), vec!["a"])),
        ),
        (
            "\n\na",
            1,
            None,
            2,
            Some((vec![""], (1, vec![""]), vec!["a"])),
        ),
        (
            "\n\na",
            1,
            None,
            3,
            Some((vec![""], (1, vec![""]), vec!["a"])),
        ),
        (
            "\n\na",
            2,
            None,
            1,
            Some((vec![""], (2, vec!["a"]), vec![])),
        ),
        (
            "\n\na",
            2,
            None,
            2,
            Some((vec!["", ""], (2, vec!["a"]), vec![])),
        ),
        (
            "\n\na",
            2,
            None,
            3,
            Some((vec!["", ""], (2, vec!["a"]), vec![])),
        ),
        (
            "hello\nhi\nbye",
            0,
            None,
            1,
            Some((vec![], (0, vec!["hello"]), vec!["hi"])),
        ),
        (
            "hello\nhi\nbye",
            0,
            None,
            2,
            Some((vec![], (0, vec!["hello"]), vec!["hi", "bye"])),
        ),
        (
            "hello\nhi\nbye",
            0,
            None,
            3,
            Some((vec![], (0, vec!["hello"]), vec!["hi", "bye"])),
        ),
        (
            "hello\nhi\nbye",
            2,
            None,
            1,
            Some((vec![], (0, vec!["hello"]), vec!["hi"])),
        ),
        (
            "hello\nhi\nbye",
            2,
            None,
            2,
            Some((vec![], (0, vec!["hello"]), vec!["hi", "bye"])),
        ),
        (
            "hello\nhi\nbye",
            2,
            None,
            3,
            Some((vec![], (0, vec!["hello"]), vec!["hi", "bye"])),
        ),
        (
            "hello\nhi\nbye",
            5,
            None,
            1,
            Some((vec![], (0, vec!["hello"]), vec!["hi"])),
        ),
        (
            "hello\nhi\nbye",
            5,
            None,
            2,
            Some((vec![], (0, vec!["hello"]), vec!["hi", "bye"])),
        ),
        (
            "hello\nhi\nbye",
            5,
            None,
            3,
            Some((vec![], (0, vec!["hello"]), vec!["hi", "bye"])),
        ),
        (
            "hello\nhi\nbye",
            6,
            None,
            1,
            Some((vec!["hello"], (6, vec!["hi"]), vec!["bye"])),
        ),
        (
            "hello\nhi\nbye",
            6,
            None,
            2,
            Some((vec!["hello"], (6, vec!["hi"]), vec!["bye"])),
        ),
        (
            "hello\nhi\nbye",
            8,
            None,
            1,
            Some((vec!["hello"], (6, vec!["hi"]), vec!["bye"])),
        ),
        (
            "hello\nhi\nbye",
            8,
            None,
            2,
            Some((vec!["hello"], (6, vec!["hi"]), vec!["bye"])),
        ),
        (
            "hello\nhi\nbye",
            9,
            None,
            1,
            Some((vec!["hi"], (9, vec!["bye"]), vec![])),
        ),
        (
            "hello\nhi\nbye",
            9,
            None,
            2,
            Some((vec!["hello", "hi"], (9, vec!["bye"]), vec![])),
        ),
        (
            "hello\nhi\nbye",
            9,
            None,
            3,
            Some((vec!["hello", "hi"], (9, vec!["bye"]), vec![])),
        ),
        (
            "hello\nhi\nbye",
            11,
            None,
            1,
            Some((vec!["hi"], (9, vec!["bye"]), vec![])),
        ),
        (
            "hello\nhi\nbye",
            11,
            None,
            2,
            Some((vec!["hello", "hi"], (9, vec!["bye"]), vec![])),
        ),
        (
            "hello\nhi\nbye",
            11,
            None,
            3,
            Some((vec!["hello", "hi"], (9, vec!["bye"]), vec![])),
        ),
        (
            "hello\nhi\nbye\n",
            12,
            None,
            1,
            Some((vec!["hi"], (9, vec!["bye"]), vec![])),
        ),
        (
            "hello\nhi\nbye\n",
            12,
            None,
            2,
            Some((vec!["hello", "hi"], (9, vec!["bye"]), vec![])),
        ),
        (
            "hello\nhi\nbye\n",
            12,
            None,
            3,
            Some((vec!["hello", "hi"], (9, vec!["bye"]), vec![])),
        ),
        (
            "line1\nline2\nline3\nline4\nline5\nline6",
            3,
            Some(5),
            3,
            Some((vec![], (0, vec!["line1"]), vec!["line2", "line3", "line4"])),
        ),
        (
            "line1\nline2\nline3\nline4\nline5\nline6",
            3,
            // Exclusive end. This is still line 1
            Some(6),
            3,
            Some((vec![], (0, vec!["line1"]), vec!["line2", "line3", "line4"])),
        ),
        (
            "line1\nline2\nline3\nline4\nline5\nline6",
            3,
            Some(7),
            3,
            Some((
                vec![],
                (0, vec!["line1", "line2"]),
                vec!["line3", "line4", "line5"],
            )),
        ),
        (
            "line1\nline2\nline3\nline4\nline5\nline6",
            7,
            Some(13),
            3,
            Some((
                vec!["line1"],
                (6, vec!["line2", "line3"]),
                vec!["line4", "line5", "line6"],
            )),
        ),
    ];

    for (s, position, end_position, n_lines, expected) in tests {
        let lines = lines_around_position(s, position, end_position, n_lines);
        assert_eq!(
            lines, expected,
            "text = {:?}, get {} lines around char {} and {:?}",
            s, n_lines, position, end_position
        );
    }
}
