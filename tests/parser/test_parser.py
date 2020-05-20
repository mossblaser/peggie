import pytest  # type: ignore

from typing import List, Union, Optional, Set, Type

import re

from peggie.parser.parser import (
    string_to_indentations,
    RelativeIndentation,
    Parser,
    Expr,
    AltExpr,
    ConcatExpr,
    StarExpr,
    LookaheadExpr,
    RuleExpr,
    RegexExpr,
    EmptyExpr,
    MaybeExpr,
    PlusExpr,
    PositiveLookaheadExpr,
    Grammar,
    ParseTree,
    Alt,
    Concat,
    Star,
    Lookahead,
    Rule,
    Regex,
    Empty,
    Maybe,
    Plus,
    PositiveLookahead,
    GrammarError,
    RepeatedEmptyTermInGrammarError,
    LeftRecursiveGrammarError,
    UndefinedRuleError,
    ParseError,
)


@pytest.mark.parametrize(
    "string, exp",
    [
        # Special case: empty
        ("", []),
        # Single line, not indented
        ("foo", [0, 0, 0]),
        # Single line, indented
        ("  \t foo", [4, 4, 4, 4, 4, 4, 4]),
        # Multiple lines
        ("  foo\n bar", [2] * 6 + [1] * 4),
        # Empty line in middle
        (" foo\n\n  bar", [1] * 5 + [0] + [2] * 5),
        # Empty line at end
        ("foo\n", [0] * 4),
        # Empty line with whitespace
        (" foo\n   \n", [1] * 5 + [3] * 4),
        (" foo\n   ", [1] * 5 + [3] * 3),
        # Windows line endings
        ("foo\r\n bar", [0] * 5 + [1] * 4),
    ],
)
def test_string_to_indentations(string: str, exp: List[int]) -> None:
    assert string_to_indentations(string) == exp


@pytest.mark.parametrize(
    "value, a, b, exp",
    [
        # Any
        ("@*", 0, 1, True),
        ("@*", 1, 1, True),
        ("@*", 1, 0, True),
        # Equal
        ("@=", 0, 1, False),
        ("@=", 1, 1, True),
        ("@=", 1, 0, False),
        # Greater or equal
        ("@>=", 0, 1, True),
        ("@>=", 1, 1, True),
        ("@>=", 1, 0, False),
        # Greater
        ("@>", 0, 1, True),
        ("@>", 1, 1, False),
        ("@>", 1, 0, False),
    ],
)
def test_relative_indentation(
    value: Union[RelativeIndentation, str], a: int, b: int, exp: bool,
) -> None:
    assert RelativeIndentation(value).check(a, b) is exp


class TestParser:
    @pytest.mark.parametrize(
        "start_expr, string, exp",
        [
            # EmptyExpr
            (EmptyExpr(), "", Empty()),
            (EmptyExpr(), "foo", Empty()),
            # RegexExpr
            (RegexExpr(re.compile(r"..")), "foo", Regex("fo", 0)),
            (RegexExpr(re.compile(r"fo*")), "foo", Regex("foo", 0)),
            (RegexExpr(re.compile(r"xxx")), "foo", None),
            # AltExpr: Both match
            (
                AltExpr((RegexExpr(re.compile(r".")), RegexExpr(re.compile(r"foo")))),
                "foo",
                Alt(Regex("f", 0), 0),
            ),
            # AltExpr: Only first matches
            (
                AltExpr((RegexExpr(re.compile(r".")), RegexExpr(re.compile(r"x")))),
                "foo",
                Alt(Regex("f", 0), 0),
            ),
            # AltExpr: Only second matches
            (
                AltExpr((RegexExpr(re.compile(r"x")), RegexExpr(re.compile(r"foo")))),
                "foo",
                Alt(Regex("foo", 0), 1),
            ),
            # AltExpr: Neither matches
            (
                AltExpr(
                    (RegexExpr(re.compile(r"x")), RegexExpr(re.compile(r"foobar")))
                ),
                "foo",
                None,
            ),
            # ConcatExpr: Both match
            (
                ConcatExpr((RegexExpr(re.compile(r".")), RegexExpr(re.compile(r"o*")))),
                "foo",
                Concat((Regex("f", 0), Regex("oo", 1))),
            ),
            # ConcatExpr: Only first matches
            (
                ConcatExpr((RegexExpr(re.compile(r".")), RegexExpr(re.compile(r"x")))),
                "foo",
                None,
            ),
            # ConcatExpr: Only second matches
            (
                ConcatExpr(
                    (RegexExpr(re.compile(r"x")), RegexExpr(re.compile(r"foo")))
                ),
                "foo",
                None,
            ),
            # ConcatExpr: Matching empty at start of string
            (ConcatExpr((EmptyExpr(), EmptyExpr())), "", Concat((Empty(), Empty()))),
            # ConcatExpr: Matching empty at end of string
            (
                ConcatExpr((RegexExpr(re.compile(r"f")), EmptyExpr())),
                "f",
                Concat((Regex("f", 0), Empty())),
            ),
            # ConcatExpr: Neither matches
            (
                ConcatExpr(
                    (RegexExpr(re.compile(r"x")), RegexExpr(re.compile(r"foobar")))
                ),
                "foo",
                None,
            ),
            # StarExpr: Matches none
            (StarExpr(RegexExpr(re.compile(r"x"))), "foo", Star(()),),
            # StarExpr: Matches one
            (StarExpr(RegexExpr(re.compile(r"f"))), "foo", Star((Regex("f", 0),)),),
            # StarExpr: Matches several
            (
                StarExpr(RegexExpr(re.compile(r"[fo]"))),
                "foo",
                Star((Regex("f", 0), Regex("o", 1), Regex("o", 2))),
            ),
            # StarExpr: Special case: empty string
            (StarExpr(RegexExpr(re.compile(r"x"))), "", Star(()),),
            # LookaheadExpr: match end-of-input
            (LookaheadExpr(RegexExpr(re.compile(r"."))), "", Lookahead(),),
            (LookaheadExpr(RegexExpr(re.compile(r"."))), "foo", None,),
            # LookaheadExpr: doesn't consume input
            (
                ConcatExpr(
                    (
                        LookaheadExpr(RegexExpr(re.compile(r"bar"))),
                        RegexExpr(re.compile(r"foo")),
                    )
                ),
                "foobar",
                Concat((Lookahead(), Regex("foo", 0))),
            ),
            # LookaheadExpr: Positive lookahead
            (
                ConcatExpr(
                    (
                        LookaheadExpr(LookaheadExpr(RegexExpr(re.compile(r"foo")))),
                        RegexExpr(re.compile(r"foo")),
                    )
                ),
                "foobar",
                Concat((Lookahead(), Regex("foo", 0))),
            ),
            (
                ConcatExpr(
                    (
                        LookaheadExpr(LookaheadExpr(RegexExpr(re.compile(r"bar")))),
                        RegexExpr(re.compile(r"foo")),
                    )
                ),
                "foobar",
                None,
            ),
            # RuleExpr: simple reference
            (RuleExpr("foo_or_bar"), "foo", Rule("foo_or_bar", Regex("foo", 0))),
            (RuleExpr("foo_or_bar"), "bar", Rule("foo_or_bar", Regex("bar", 0))),
            # RuleExpr: right-recursion
            (
                AltExpr(
                    (
                        ConcatExpr((RegexExpr(re.compile(r"x")), RuleExpr("start"),)),
                        LookaheadExpr(RegexExpr(re.compile(r"."))),
                    )
                ),
                "",
                Alt(Lookahead(), 1),
            ),
            (
                AltExpr(
                    (
                        ConcatExpr((RegexExpr(re.compile(r"x")), RuleExpr("start"),)),
                        LookaheadExpr(RegexExpr(re.compile(r"."))),
                    )
                ),
                "xx",
                Alt(
                    Concat(
                        (
                            Regex("x", 0),
                            Rule(
                                "start",
                                Alt(
                                    Concat(
                                        (
                                            Regex("x", 1),
                                            Rule("start", Alt(Lookahead(), 1)),
                                        )
                                    ),
                                    0,
                                ),
                            ),
                        )
                    ),
                    0,
                ),
            ),
            # MaybeExpr
            (MaybeExpr(RegexExpr(re.compile("foo"))), "foo", Maybe(Regex("foo", 0))),
            (MaybeExpr(RegexExpr(re.compile("bar"))), "foo", Maybe(None)),
            (MaybeExpr(EmptyExpr()), "", Maybe(Empty())),
            # PlusExpr: Doesn't match none
            (PlusExpr(RegexExpr(re.compile(r"y"))), "", None),
            (PlusExpr(RegexExpr(re.compile(r"y"))), "xxx", None),
            # PlusExpr: Matches one
            (PlusExpr(RegexExpr(re.compile(r"x"))), "x", Plus((Regex("x", 0),))),
            (PlusExpr(RegexExpr(re.compile(r"x"))), "xyy", Plus((Regex("x", 0),))),
            # PlusExpr: Matches many
            (
                PlusExpr(RegexExpr(re.compile(r"x"))),
                "xxx",
                Plus((Regex("x", 0), Regex("x", 1), Regex("x", 2))),
            ),
            (
                PlusExpr(RegexExpr(re.compile(r"x"))),
                "xxxy",
                Plus((Regex("x", 0), Regex("x", 1), Regex("x", 2))),
            ),
            # PositiveLookaheadExpr
            (
                PositiveLookaheadExpr(RegexExpr(re.compile("foo"))),
                "foo",
                PositiveLookahead(),
            ),
            (PositiveLookaheadExpr(RegexExpr(re.compile("foo"))), "bar", None),
        ],
    )
    def test_basic_parsing(
        self, start_expr: Expr, string: str, exp: Optional[ParseTree]
    ) -> None:
        parser = Parser(
            Grammar(
                rules={
                    "start": start_expr,
                    "foo_or_bar": RegexExpr(re.compile(r"foo|bar")),
                },
            )
        )
        if exp is None:
            with pytest.raises(ParseError):
                parser.parse(string)
        else:
            parse_tree = parser.parse(string)
            assert isinstance(parse_tree, Rule)
            assert parse_tree.name == "start"
            assert parse_tree.value == exp

    @pytest.mark.parametrize(
        "case_name, string",
        [
            ("none", "foo\nfoo\nfoo"),
            ("constant", "  foo\n  foo\n  foo"),
            ("increasing_0", "foo\n foo\n  foo"),
            ("increasing_1", " foo\n  foo\n   foo"),
            ("monotonic_0", "foo\nfoo\n foo"),
            ("monotonic_1", " foo\n foo\n  foo"),
            ("non_monotonic", " foo\nfoo\n  foo"),
        ],
    )
    @pytest.mark.parametrize(
        "indentation, exp_matches",
        [
            (
                RelativeIndentation("@*"),
                {
                    "none",
                    "constant",
                    "increasing_0",
                    "increasing_1",
                    "monotonic_0",
                    "monotonic_1",
                    "non_monotonic",
                },
            ),
            (RelativeIndentation("@="), {"none", "constant"}),
            (
                RelativeIndentation("@>="),
                {
                    "none",
                    "constant",
                    "increasing_0",
                    "increasing_1",
                    "monotonic_0",
                    "monotonic_1",
                },
            ),
            (RelativeIndentation("@>"), {"increasing_0", "increasing_1"}),
        ],
    )
    def test_concat_indentation_modes(
        self,
        case_name: str,
        string: str,
        indentation: RelativeIndentation,
        exp_matches: Set[str],
    ) -> None:
        expr = ConcatExpr(
            (
                RegexExpr(re.compile(r"\s*foo\s*", re.DOTALL)),
                RegexExpr(re.compile(r"\s*foo\s*", re.DOTALL), indentation),
                RegexExpr(re.compile(r"\s*foo\s*", re.DOTALL), indentation),
                LookaheadExpr(RegexExpr(re.compile(".", re.DOTALL))),
            )
        )

        p = Parser(Grammar(rules={"start": expr}))
        if case_name in exp_matches:
            assert p.parse(string) is not None
        else:
            with pytest.raises(ParseError):
                p.parse(string)

    @pytest.mark.parametrize("expr_type", [StarExpr, PlusExpr])
    @pytest.mark.parametrize(
        "case_name, string",
        [
            ("none", "foo\nfoo\nfoo"),
            ("constant", "  foo\n  foo\n  foo"),
            ("increasing_0", "foo\n foo\n  foo"),
            ("increasing_1", " foo\n  foo\n   foo"),
            ("monotonic_0", "foo\nfoo\n foo"),
            ("monotonic_1", " foo\n foo\n  foo"),
            ("non_monotonic", " foo\nfoo\n  foo"),
        ],
    )
    @pytest.mark.parametrize(
        "indentation, exp_matches",
        [
            (
                RelativeIndentation("@*"),
                {
                    "none",
                    "constant",
                    "increasing_0",
                    "increasing_1",
                    "monotonic_0",
                    "monotonic_1",
                    "non_monotonic",
                },
            ),
            (RelativeIndentation("@="), {"none", "constant"}),
            (
                RelativeIndentation("@>="),
                {
                    "none",
                    "constant",
                    "increasing_0",
                    "increasing_1",
                    "monotonic_0",
                    "monotonic_1",
                },
            ),
            # NB: The first item in the repeated pattern cannot have greater
            # indentation than itself so in this special case, no patterns are
            # allowed
            (RelativeIndentation("@>"), set()),
        ],
    )
    def test_star_and_plus_indentation_modes(
        self,
        expr_type: Type[Union[StarExpr, PlusExpr]],
        case_name: str,
        string: str,
        indentation: RelativeIndentation,
        exp_matches: Set[str],
    ) -> None:
        expr = ConcatExpr(
            (
                expr_type(RegexExpr(re.compile(r"\s*foo\s*", re.DOTALL), indentation)),
                LookaheadExpr(RegexExpr(re.compile(".", re.DOTALL))),
            )
        )
        p = Parser(Grammar(rules={"start": expr}))
        if case_name in exp_matches:
            assert p.parse(string) is not None
        else:
            with pytest.raises(ParseError):
                p.parse(string)

    @pytest.mark.parametrize(
        "expr, exp",
        [
            # StarExpr matches empty (direct)
            (StarExpr(EmptyExpr()), RepeatedEmptyTermInGrammarError),
            # StarExpr matches empty (hidden)
            (
                StarExpr(RegexExpr(re.compile(r"[xy]?"))),
                RepeatedEmptyTermInGrammarError,
            ),
            # PlusExpr matches empty (direct)
            (PlusExpr(EmptyExpr()), RepeatedEmptyTermInGrammarError),
            # PlusExpr matches empty (hidden)
            (
                PlusExpr(MaybeExpr(RegexExpr(re.compile("zzz")))),
                RepeatedEmptyTermInGrammarError,
            ),
            # Left recursion: direct
            (RuleExpr("start"), LeftRecursiveGrammarError),
            (AltExpr((RuleExpr("start"), EmptyExpr())), LeftRecursiveGrammarError),
            (
                AltExpr(
                    (
                        ConcatExpr((RuleExpr("start"), RegexExpr(re.compile("x")))),
                        EmptyExpr(),
                    )
                ),
                LeftRecursiveGrammarError,
            ),
            # Left recursion: indirect
            (RuleExpr("start_alias"), LeftRecursiveGrammarError),
            # Left recursion: hidden
            (
                ConcatExpr(
                    (MaybeExpr(RegexExpr(re.compile(r"x|y"))), RuleExpr("start"),)
                ),
                LeftRecursiveGrammarError,
            ),
            # Undefined rule
            (RuleExpr("undefined"), UndefinedRuleError),
        ],
    )
    def test_infinite_loop_protections(
        self, expr: Expr, exp: Type[GrammarError]
    ) -> None:
        parser = Parser(
            Grammar(rules={"start": expr, "start_alias": RuleExpr("start")},)
        )

        with pytest.raises(exp):
            parser.parse("xxxyyy")
