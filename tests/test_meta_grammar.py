import pytest  # type: ignore

from peggie.parser import (
    Parser,
    ParseError,
)

from peggie.meta_grammar import (
    grammar,
    grammar_source,
)


@pytest.mark.parametrize(
    "string, exp_matches",
    [
        # Invalid: Empty grammar
        ("", False),
        # Not a rule
        ("foo", False),
        ("'foo'", False),
        # Valid: Empty rule
        ("rule <-", True),
        # Valid: simple rule
        ("rule <- 'hi'", True),
        # Valid: whitespace variants
        ("   rule   <-   'hi'   ", True),
        ("rule<-'hi'", True),
        ("\nrule\n<-\n'hi'", True),
        # Valid: Multiple rules
        ("rule<-'hi'\nanother<-'bye'", True),
        # Valid: Matching strings
        ("rule<-'hello'", True),
        ('rule<-"hello"', True),
        ("rule<-'hello \"there\"'", True),
        ("rule<-\"hello 'there'\"", True),
        ("rule<-'\\' escaped'", True),
        ('rule<-"\\" escaped"', True),
        ("rule<-'\\n escaped'", True),
        ("rule<-'\\\\ escaped'", True),
        # Valid: Matching regexes
        ("rule<-r'hello'", True),
        ('rule<-r"hello"', True),
        # Valid: Matching char classes
        ("rule<-[a]", True),
        ("rule<-[a-z0-9_]", True),
        ("rule<-[[\\]]", True),
        ("rule<-[\\[\\]]", True),
        # Invalid: Char classes with unescaped bracket
        ("rule<-[]]", False),
        # Invalid: Empty char class
        ("rule<-[]", False),
        # Invalid: Mismatched regex quotes
        ("rule <- 'foo", False),
        ("rule <- 'foo\\'", False),
        ('rule <- "foo', False),
        ('rule <- "foo\\"', False),
        # Valid: Concatenation
        ("rule <- 'a' 'b' 'c'", True),
        # Valid: Alternation
        ("rule <- 'a' / 'b' / 'c'", True),
        # Valid: Lookahead
        ("rule <- !'a'", True),
        ("rule <- &'a'", True),
        # Valid: Maybe
        ("rule <- 'a'?", True),
        # Valid: Suffix
        ("rule <- 'a'?", True),
        ("rule <- 'a'*", True),
        ("rule <- 'a'+", True),
        # Valid: Indentation prefixes
        ("rule <- @* other", True),
        ("rule <- @= other", True),
        ("rule <- @>= other", True),
        ("rule <- @> other", True),
        # Invalid indentation prefix
        ("rule <- @? other", False),
        # Valid: Rule refernced
        ("rule <- another_rule123", True),
        # Invalid: Invalid rule name
        ("rule <- 123", False),
        # Valid: Wildcard
        ("rule <- .", True),
        # Valid: Perenthesised expression
        ("rule <- (foo)", True),
        ("rule <- (foo / bar) / baz", True),
        # Invalid: Mismatched brackets
        ("rule <- (foo", False),
        # Valid: The meta-grammar itself
        (grammar_source, True),
    ],
)
def test_peg_meta_grammar(string: str, exp_matches: bool) -> None:
    p = Parser(grammar)
    if exp_matches:
        assert p.parse(string) is not None
    else:
        with pytest.raises(ParseError):
            p.parse(string)
