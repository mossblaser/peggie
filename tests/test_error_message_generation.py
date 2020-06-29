import pytest  # type: ignore

from peggie.error_message_generation import (
    offset_to_line_and_column,
    extract_line,
    format_error_message,
)


@pytest.mark.parametrize(
    "string, offset, exp_line, exp_column",
    [
        # Special case: Empty string
        ("", 0, 1, 1),
        # Special case: Beyond end of string
        ("foobar", 111, 1, 7),
        ("foobar\n", 111, 1, 8),
        ("foo\nbar", 111, 2, 4),
        # Single line
        ("foobar", 0, 1, 1),
        ("foobar", 3, 1, 4),
        ("foobar", 5, 1, 6),
        # Multiple lines
        ("foo\nbar", 0, 1, 1),
        ("foo\nbar", 2, 1, 3),
        ("foo\nbar", 3, 1, 4),  # The newline
        ("foo\nbar", 4, 2, 1),
        ("foo\nbar", 6, 2, 3),
    ],
)
def test_offset_to_line_and_column(
    string: str, offset: int, exp_line: int, exp_column: int
) -> None:
    assert offset_to_line_and_column(string, offset) == (exp_line, exp_column)


@pytest.mark.parametrize(
    "string, line, exp",
    [
        # Special case: Empty string
        ("", 1, ""),
        # Single line (new line ending)
        ("foo", 1, "foo"),
        # Single line (with line ending)
        ("foo\n", 1, "foo"),
        ("foo\r", 1, "foo"),
        ("foo\r\n", 1, "foo"),
        # Multiple lines
        ("foo\nbar\n", 1, "foo"),
        ("foo\nbar\n", 2, "bar"),
        ("foo\rbar\r", 1, "foo"),
        ("foo\rbar\r", 2, "bar"),
        ("foo\r\nbar\r\rn", 1, "foo"),
        ("foo\r\nbar\r\rn", 2, "bar"),
    ],
)
def test_extract_line(string: str, line: int, exp: str) -> None:
    assert extract_line(string, line) == exp


def test_format_error_message() -> None:
    assert format_error_message(100, 10, "    I love strings   ", "Some message") == (
        "At line 100 column 10:\n"
        "        I love strings\n"
        "             ^\n"
        "Some message"
    )
