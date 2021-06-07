use std::cmp::max;
use std::ops::Range;
use std::slice::SliceIndex;
use std::str::CharIndices;

#[derive(Debug)]
enum SourceOrigin {
    File(String),
    NotAFile,
}

#[derive(Debug)]
pub struct Source<'a> {
    source: SourceOrigin,
    code: &'a str,
}

impl<'a> Source<'a> {
    pub fn new_file(path: String, code: &'a str) -> Self {
        Source {
            source: SourceOrigin::File(path),
            code,
        }
    }

    pub fn new_orphan(code: &'a str) -> Self {
        Source {
            source: SourceOrigin::NotAFile,
            code,
        }
    }

    pub fn name(&self) -> String {
        match &self.source {
            SourceOrigin::File(path) => path.clone(),
            SourceOrigin::NotAFile => "Main".to_owned(),
        }
    }

    pub fn char_at(&self, i: usize) -> Option<char> {
        self.code.get(i..).and_then(|code| code.chars().next())
    }

    pub fn text_at<I>(&self, i: I) -> Option<&<I as SliceIndex<str>>::Output>
    where
        I: SliceIndex<str>,
    {
        self.code.get(i)
    }

    fn line_at(&self, position: usize) -> Option<&str> {
        self.code.get(line_at(&self.code, position)?)
    }

    pub fn lines_around_position(
        &self,
        position: usize,
        end_position: Option<usize>,
        number_of_lines: u32,
    ) -> Option<(Vec<&str>, (usize, Vec<&str>), Vec<&str>)> {
        lines_around_position(&self.code, position, end_position, number_of_lines)
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn char_indices(&self) -> CharIndices {
        self.code.char_indices()
    }

    pub fn lines_report_at_position_with_pointer(
        &self,
        position: usize,
        end_position: Option<usize>,
        line_number: u32,
    ) -> Option<String> {
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
            message.push_str(&format!(
                "  {0:1$}│  {2}\n",
                line_number - lines_before.len() as u32 + i as u32,
                line_number_width,
                l
            ));
        }

        match lines.as_slice() {
            [] => panic!("Empty code for report, should've bailed earlier at line_at"),
            [line] => {
                message.push_str(&format!(
                    "  {0:1$}│  {2}\n",
                    line_number, line_number_width, line
                ));

                let num_pointers = max(1, end_position.unwrap_or(position) - position);
                message.push_str(&format!(
                    "  {0:1$}│  {2}{3}\n",
                    " ",
                    line_number_width,
                    str::repeat(" ", column_number as usize),
                    str::repeat("↑", num_pointers as usize)
                ));
            }
            lines => {
                for (i, l) in lines.iter().enumerate() {
                    message.push_str(&format!(
                        "  {0:1$}│→ {2}\n",
                        line_number + i as u32,
                        line_number_width,
                        l
                    ));
                }
            }
        }

        for (i, l) in lines_after.iter().enumerate() {
            message.push_str(&format!(
                "  {0:1$}│ {2}\n",
                line_number + 1 + i as u32,
                line_number_width,
                l
            ));
        }
        Some(message)
    }

    pub fn to_string_with_line_and_col(&self, line: usize, column: usize) -> String {
        let s = match &self.source {
            SourceOrigin::File(path) => Some(path.to_string()),
            SourceOrigin::NotAFile => None,
        };
        let space = if s.is_some() { " " } else { "" }.to_string();
        format!(
            "{}{}[{}:{}]",
            s.unwrap_or("".to_string()),
            space,
            line,
            column
        )
    }
}

/*
mod tests {
    use super::*;

    #[test]
    fn test_lines_report_at_position_with_pointer() {
        use insta::assert_debug_snapshot_matches;

        let strings = vec![
            "",
            "1 + 2 + 3",
            "fn test () {\n    1 + 2\n}",
            "module Test {\n    fn test () {\n        1 + 2\n    }\n\n    fn test2 () {\n        1 + 2\n    }\n}",
        ];
        for code in strings {
            let file = SourceFile::new_orphan(&code);
            assert_debug_snapshot_matches!(file.lines_around_position(0, 2));
            assert_debug_snapshot_matches!(file.lines_around_position(code.len() / 2, 2));
            assert_debug_snapshot_matches!(
                file.lines_around_position(if code.len() > 0 { code.len() - 1 } else { 0 }, 2)
            );
        }
    }
}
*/

fn line_at(code: &str, position: usize) -> Option<Range<usize>> {
    if position >= code.len() {
        return None;
    }

    let line_start = {
        let back = code.get(..position)?;
        if back.len() == 0 {
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
) -> Option<(Vec<&str>, (usize, Vec<&str>), Vec<&str>)> {
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
