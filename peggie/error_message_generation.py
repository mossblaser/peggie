"""
Utility functions for generating error messages.
"""

from typing import Tuple

from textwrap import indent


def offset_to_line_and_column(string: str, offset: int) -> Tuple[int, int]:
    """
    Return the (1-indexed) line and column number corresponding with the
    specified offset.
    """
    lineno = 0
    line = ""
    remaining = offset
    for lineno, line in enumerate(string.splitlines(keepends=True)):
        if remaining < len(line):
            return lineno + 1, remaining + 1
        else:
            remaining -= len(line)

    # When beyond the end of the file, just point off the end of the last line
    return lineno + 1, len(line) + 1


def extract_line(string: str, line: int) -> str:
    """
    Given a line number (from :py:func:`offset_to_line_and_column`), return
    just that line (without any trailing newlines).
    """
    if string:
        return string.splitlines()[line - 1]
    else:
        # Special case: str.splitlines returns an empty list for the empty string
        return ""


def format_error_message(line: int, column: int, snippet: str, message: str) -> str:
    """
    Generate a formatted error message of the style::

        At line 100 column 6:
            your snippet here...
                 ^
        Your message here...

    Takes a line and column number (from :py:func:`offset_to_line_and_column`)
    and a one-line snippet (from :py:func:`extract_line`) and a message.
    """
    snippet = snippet.rstrip()
    pointer = (" " * (column - 1)) + "^"
    indented_snippet = indent(f"{snippet}\n{pointer}", "    ")

    return f"At line {line} column {column}:\n{indented_snippet}\n{message}"
