import pytest  # type: ignore

from typing import List, Union, Optional, Set, Type, Mapping

import re

from peggie.peg_parser.parser import (
    string_to_indentations,
    offset_to_line_and_column,
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
    RepeatedEmptyTermError,
    LeftRecursionError,
    UndefinedRuleError,
    ParseError,
    GrammarWellFormedness,
    WellFormed,
    UndefinedRule,
    RepeatedEmptyTerm,
    LeftRecursion,
    AbsoluteIndentation,
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


class TestExpr:
    @pytest.mark.parametrize(
        "expr, exp_matches_empty",
        [
            # AltExpr: None match empty
            (AltExpr((RegexExpr(re.compile(".")), RegexExpr(re.compile(".")))), False),
            # AltExpr: Some, but not all match empty
            (AltExpr((RegexExpr(re.compile(".?")), RegexExpr(re.compile(".")))), True),
            (AltExpr((RegexExpr(re.compile(".")), RegexExpr(re.compile(".?")))), True),
            # AltExpr: All match empty
            (AltExpr((RegexExpr(re.compile(".?")), RegexExpr(re.compile(".?")))), True),
            # AltExpr: Rule used multiple times
            (ConcatExpr((RuleExpr("a"), RuleExpr("a"))), False),
            (ConcatExpr((RuleExpr("b"), RuleExpr("b"))), True),
            # ConcatExpr: None match empty
            (
                ConcatExpr((RegexExpr(re.compile(".")), RegexExpr(re.compile(".")))),
                False,
            ),
            # ConcatExpr: Some, but not all match empty
            (
                ConcatExpr((RegexExpr(re.compile(".?")), RegexExpr(re.compile(".")))),
                False,
            ),
            (
                ConcatExpr((RegexExpr(re.compile(".")), RegexExpr(re.compile(".?")))),
                False,
            ),
            # ConcatExpr: All match empty
            (
                ConcatExpr((RegexExpr(re.compile(".?")), RegexExpr(re.compile(".?")))),
                True,
            ),
            # ConcatExpr: Rule used multiple times
            (ConcatExpr((RuleExpr("a"), RuleExpr("a"))), False),
            (ConcatExpr((RuleExpr("b"), RuleExpr("b"))), True),
            # StarExpr
            (StarExpr(RegexExpr(re.compile("."))), True),
            (StarExpr(RegexExpr(re.compile(".?"))), True),
            # LookaheadExpr
            (LookaheadExpr(RegexExpr(re.compile("."))), True),
            (LookaheadExpr(RegexExpr(re.compile(".?"))), True),
            # RegexExpr
            (RegexExpr(re.compile(".")), False),
            (RegexExpr(re.compile(".?")), True),
            # EmptyExpr
            (EmptyExpr(), True),
            # MaybeExpr
            (MaybeExpr(RegexExpr(re.compile("."))), True),
            (MaybeExpr(RegexExpr(re.compile(".?"))), True),
            # PlusExpr
            (PlusExpr(RegexExpr(re.compile("."))), False),
            (PlusExpr(RegexExpr(re.compile(".?"))), True),
            # PositiveLookaheadExpr
            (PositiveLookaheadExpr(RegexExpr(re.compile("."))), True),
            (PositiveLookaheadExpr(RegexExpr(re.compile(".?"))), True),
        ],
    )
    def test_matches_empty_non_rule_cases(
        self, expr: Expr, exp_matches_empty: bool
    ) -> None:
        g = Grammar(
            {
                "start": expr,
                "a": RegexExpr(re.compile("a")),
                "b": RegexExpr(re.compile("b?")),
            }
        )
        assert expr.matches_empty(g) is exp_matches_empty

    @pytest.mark.parametrize(
        "other_regex, exp_matches_empty", [(".", False), (".?", True)]
    )
    def test_matches_empty_well_formed_rule(
        self, other_regex: str, exp_matches_empty: bool
    ) -> None:
        g = Grammar(
            {"start": RuleExpr("other"), "other": RegexExpr(re.compile(other_regex))}
        )
        expr = g.rules["start"]
        assert expr.matches_empty(g) is exp_matches_empty

    def test_matches_empty_left_recursive_rule(self) -> None:
        g = Grammar(
            {
                "start": RuleExpr("other"),
                "other": ConcatExpr((AltExpr((RuleExpr("start"),)),)),
            }
        )
        expr = g.rules["start"]
        assert expr.matches_empty(g) is True

    a = RegexExpr(re.compile("a"))
    b = RegexExpr(re.compile("b"))

    @pytest.mark.parametrize(
        "expr, exp_subexpressions",
        [
            (AltExpr((a, b)), {a, b}),
            (ConcatExpr((a, b)), {a, b}),
            (StarExpr(a), {a}),
            (LookaheadExpr(a), {a}),
            (RuleExpr("b"), {b}),
            (RegexExpr(re.compile(".")), set()),
            (EmptyExpr(), set()),
            (MaybeExpr(a), {a}),
            (PlusExpr(a), {a}),
            (PositiveLookaheadExpr(a), {a}),
        ],
    )
    def test_iter_subexpressions(
        self, expr: Expr, exp_subexpressions: Set[Expr]
    ) -> None:
        g = Grammar({"start": expr, "b": self.b})
        assert set(expr.iter_subexpressions(g)) == exp_subexpressions

    @pytest.mark.parametrize(
        "expr, exp_subexpressions",
        [
            (AltExpr((a, b)), {a, b}),
            (ConcatExpr((a, b)), {a}),
            (ConcatExpr((EmptyExpr(), b)), {EmptyExpr(), b}),
            (StarExpr(a), {a}),
            (LookaheadExpr(a), {a}),
            (RuleExpr("b"), {b}),
            (RegexExpr(re.compile(".")), set()),
            (EmptyExpr(), set()),
            (MaybeExpr(a), {a}),
            (PlusExpr(a), {a}),
            (PositiveLookaheadExpr(a), {a}),
        ],
    )
    def test_iter_first_subexpressions(
        self, expr: Expr, exp_subexpressions: Set[Expr]
    ) -> None:
        g = Grammar({"start": expr, "b": self.b})
        assert set(expr.iter_first_subexpressions(g)) == exp_subexpressions

    @pytest.mark.parametrize(
        "expr, exp_first_set",
        [
            (AltExpr((MaybeExpr(a), b)), {MaybeExpr(a), a, b}),
            (ConcatExpr((a, b)), {a}),
            (ConcatExpr((MaybeExpr(a), b)), {MaybeExpr(a), a, b}),
            (StarExpr(PlusExpr(a)), {PlusExpr(a), a}),
            (LookaheadExpr(PlusExpr(a)), {PlusExpr(a), a}),
            (RuleExpr("b"), {b}),
            (RuleExpr("a"), {AltExpr((a, RuleExpr("a"))), a, RuleExpr("a")}),
            (RegexExpr(re.compile(".")), set()),
            (EmptyExpr(), set()),
            (MaybeExpr(PlusExpr(a)), {PlusExpr(a), a}),
            (PlusExpr(StarExpr(a)), {StarExpr(a), a}),
            (PositiveLookaheadExpr(PlusExpr(a)), {PlusExpr(a), a}),
        ],
    )
    def test_first_set(self, expr: Expr, exp_first_set: Set[Expr]) -> None:
        g = Grammar({"start": expr, "a": AltExpr((self.a, RuleExpr("a"))), "b": self.b})
        assert set(expr.first_set(g)) == exp_first_set


class TestGrammar:

    a = RegexExpr(re.compile("a"))
    b = RegexExpr(re.compile("b"))

    @pytest.mark.parametrize(
        "expr, exp",
        [
            # Well formed rules
            (AltExpr((MaybeExpr(a), b)), WellFormed()),
            (AltExpr((RuleExpr("b"), RuleExpr("b"))), WellFormed()),
            (ConcatExpr((a, b)), WellFormed()),
            (ConcatExpr((RuleExpr("b"), RuleExpr("b"))), WellFormed()),
            (ConcatExpr((MaybeExpr(a), b)), WellFormed()),
            (StarExpr(PlusExpr(a)), WellFormed()),
            (LookaheadExpr(PlusExpr(a)), WellFormed()),
            (RuleExpr("b"), WellFormed()),
            (RegexExpr(re.compile(".")), WellFormed()),
            (EmptyExpr(), WellFormed()),
            (MaybeExpr(PlusExpr(a)), WellFormed()),
            (PlusExpr(a), WellFormed()),
            (PositiveLookaheadExpr(PlusExpr(a)), WellFormed()),
            # StarExpr matches empty (direct)
            (StarExpr(EmptyExpr()), RepeatedEmptyTerm(StarExpr(EmptyExpr()))),
            # StarExpr matches empty (hidden)
            (
                StarExpr(RegexExpr(re.compile(r"[xy]?"))),
                RepeatedEmptyTerm(StarExpr(RegexExpr(re.compile(r"[xy]?")))),
            ),
            # PlusExpr matches empty (direct)
            (PlusExpr(EmptyExpr()), RepeatedEmptyTerm(PlusExpr(EmptyExpr()))),
            # PlusExpr matches empty (hidden)
            (
                PlusExpr(MaybeExpr(RegexExpr(re.compile("zzz")))),
                RepeatedEmptyTerm(PlusExpr(MaybeExpr(RegexExpr(re.compile("zzz"))))),
            ),
            # Left recursion: direct
            (RuleExpr("start"), LeftRecursion("start")),
            (AltExpr((RuleExpr("start"), EmptyExpr())), LeftRecursion("start")),
            (
                AltExpr(
                    (
                        ConcatExpr((RuleExpr("start"), RegexExpr(re.compile("x")))),
                        EmptyExpr(),
                    )
                ),
                LeftRecursion("start"),
            ),
            # Left recursion: indirect
            (RuleExpr("start_alias"), LeftRecursion("start")),
            # Left recursion: hidden
            (
                ConcatExpr(
                    (MaybeExpr(RegexExpr(re.compile(r"x|y"))), RuleExpr("start"),)
                ),
                LeftRecursion("start"),
            ),
            # Undefined rule
            (RuleExpr("undefined"), UndefinedRule("undefined")),
        ],
    )
    def test_is_well_formed(self, expr: Expr, exp: GrammarWellFormedness) -> None:
        g = Grammar(
            rules={"start": expr, "start_alias": RuleExpr("start"), "b": self.b}
        )
        assert g.is_well_formed() == exp

    def test_is_well_formed_missing_start_rule(self) -> None:
        g = Grammar(rules={})
        assert g.is_well_formed() == UndefinedRule("start")


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

    @pytest.mark.parametrize(
        "string, exp_match",
        [
            # Foos with equal indentation
            ("foo\nfoo\nfoo\n", True),
            ("  foo\n  foo\n  foo\n", True),
            # Foos with non-equal indentation
            ("foo\n foo\n  foo\n", False),
            ("foo\nfoo\n foo\n", False),
            ("  foo\nfoo\nfoo\n", False),
            # With internal indented blocks
            ("foo\n bar\nfoo\n bar\nfoo\n", True),
            # With non-indented 'bar' blocks
            ("foo\nbar\nfoo\nbar\nfoo\n", False),
            # With differently-indented internal indented blocks
            ("foo\n  bar\nfoo\n bar\nfoo\n", True),
            # Foos with non-equal indentation and indented blocks
            ("foo\n bar\n foo\n bar\nfoo\n", False),
        ],
    )
    def test_concat_ignore_empty_match_indentation(
        self, string: str, exp_match: bool
    ) -> None:
        g = Grammar(
            {
                "start": ConcatExpr(
                    (
                        RegexExpr(
                            re.compile(r"\s*foo\n\s*", re.DOTALL),
                            RelativeIndentation.equal,
                        ),
                        MaybeExpr(
                            RegexExpr(re.compile(r"\s*bar\n\s*", re.DOTALL)),
                            RelativeIndentation.greater,
                        ),
                        RegexExpr(
                            re.compile(r"\s*foo\n\s*", re.DOTALL),
                            RelativeIndentation.equal,
                        ),
                        MaybeExpr(
                            RegexExpr(re.compile(r"\s*bar\n\s*", re.DOTALL)),
                            RelativeIndentation.greater,
                        ),
                        RegexExpr(
                            re.compile(r"\s*foo\n\s*", re.DOTALL),
                            RelativeIndentation.equal,
                        ),
                        LookaheadExpr(RegexExpr(re.compile(r".", re.DOTALL))),
                    )
                )
            }
        )
        p = Parser(g)

        if exp_match:
            assert isinstance(p.parse(string), ParseTree)
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
                print(p.parse(string))

    @pytest.mark.parametrize(
        "expr, exp",
        [
            # StarExpr matches empty (direct)
            (StarExpr(EmptyExpr()), RepeatedEmptyTermError),
            # StarExpr matches empty (hidden)
            (StarExpr(RegexExpr(re.compile(r"[xy]?"))), RepeatedEmptyTermError,),
            # PlusExpr matches empty (direct)
            (PlusExpr(EmptyExpr()), RepeatedEmptyTermError),
            # PlusExpr matches empty (hidden)
            (
                PlusExpr(MaybeExpr(RegexExpr(re.compile("zzz")))),
                RepeatedEmptyTermError,
            ),
            # Left recursion: direct
            (RuleExpr("start"), LeftRecursionError),
            (AltExpr((RuleExpr("start"), EmptyExpr())), LeftRecursionError),
            (
                AltExpr(
                    (
                        ConcatExpr((RuleExpr("start"), RegexExpr(re.compile("x")))),
                        EmptyExpr(),
                    )
                ),
                LeftRecursionError,
            ),
            # Left recursion: indirect
            (RuleExpr("start_alias"), LeftRecursionError),
            # Left recursion: hidden
            (
                ConcatExpr(
                    (MaybeExpr(RegexExpr(re.compile(r"x|y"))), RuleExpr("start"),)
                ),
                LeftRecursionError,
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

    @pytest.mark.parametrize(
        "string, exp_parse_error",
        [
            # Sanity check rules match what they're expected to
            ("abc", None),
            ("abxy", None),
            ("a\n   nl", None),
            ("a\nNL", None),
            ("a\n  NL", None),
            ("a.", None),
            ("a...", None),
            ("a\nin\nin\nin\n", None),
            ("a\n  in\n  in\n  in\n", None),
            ("ahi", None),
            # Failure of root rule (and empty string case)
            ("", ParseError(1, 1, "", {(RuleExpr("start"), frozenset({None}))})),
            # Single branch
            (
                "abx?",
                ParseError(
                    1, 4, "abx?", {(RegexExpr(re.compile("y")), frozenset({None}))}
                ),
            ),
            # Multiple branches (must iterate over all first set candidates)
            (
                "ab?",
                ParseError(
                    1,
                    3,
                    "ab?",
                    {
                        (RegexExpr(re.compile("c")), frozenset({None})),
                        (RegexExpr(re.compile("x")), frozenset({None})),
                        (RegexExpr(re.compile("y")), frozenset({None})),
                    },
                ),
            ),
            # Don't recurse into other rules when no part of the rule matches
            (
                "a?",
                ParseError(
                    1,
                    2,
                    "a?",
                    {
                        (RegexExpr(re.compile("b")), frozenset({None})),
                        (RegexExpr(re.compile("\n")), frozenset({None})),
                        (RuleExpr("hi_rule"), frozenset({None})),
                        (RegexExpr(re.compile(r"\.")), frozenset({None})),
                    },
                ),
            ),
            # But do when part of the rule matches
            (
                "ah?",
                ParseError(
                    1, 3, "ah?", {(RegexExpr(re.compile("i")), frozenset({None}))}
                ),
            ),
            # Should omit newline in snippet
            ("x\n", ParseError(1, 1, "x", {(RuleExpr("start"), frozenset({None}))})),
            # Should identify correct line for non-indent error
            (
                "a\n    ?",
                ParseError(
                    2,
                    5,
                    "    ?",
                    {
                        (RegexExpr(re.compile("n")), frozenset({None})),
                        (RegexExpr(re.compile("N")), frozenset({None})),
                    },
                ),
            ),
            # Should identify correct line for indent error (and report correct
            # indentation issues, propagating these to the expressions they
            # actually apply to, not just the outermost expression)
            (
                "a\nnl",
                ParseError(
                    2,
                    1,
                    "nl",
                    {
                        (
                            RegexExpr(re.compile(" *")),
                            frozenset(
                                {
                                    None,
                                    AbsoluteIndentation(0, RelativeIndentation.greater),
                                }
                            ),
                        ),
                        (
                            RegexExpr(re.compile("n")),
                            frozenset(
                                {AbsoluteIndentation(0, RelativeIndentation.greater)}
                            ),
                        ),
                        (RegexExpr(re.compile("N")), frozenset({None})),
                        (
                            RegexExpr(re.compile(r" *in\n"), RelativeIndentation.equal),
                            frozenset({None}),
                        ),
                    },
                ),
            ),
            # Should include failiures to match in plus/star
            (
                "a..?",
                ParseError(
                    1,
                    4,
                    "a..?",
                    {
                        (RegexExpr(re.compile(r"\.")), frozenset({None})),
                        (RuleExpr("eof"), frozenset({None})),
                    },
                ),
            ),
            # Should include indentation match failiures in plus/star
            (
                "a\n in\nin",
                ParseError(
                    3,
                    1,
                    "in",
                    {
                        (
                            RegexExpr(re.compile(r" *in\n"), RelativeIndentation.equal),
                            frozenset(
                                {AbsoluteIndentation(1, RelativeIndentation.equal)}
                            ),
                        ),
                        (RuleExpr("eof"), frozenset({None})),
                    },
                ),
            ),
        ],
    )
    def test_parse_error_generation(
        self, string: str, exp_parse_error: Optional[ParseError]
    ) -> None:
        parser = Parser(
            Grammar(
                rules={
                    "start": ConcatExpr(
                        (
                            RegexExpr(re.compile("a")),
                            AltExpr(
                                (
                                    ConcatExpr(
                                        (
                                            RegexExpr(re.compile("b")),
                                            RegexExpr(re.compile("c")),
                                        )
                                    ),
                                    ConcatExpr(
                                        (
                                            RegexExpr(re.compile("b")),
                                            MaybeExpr(RegexExpr(re.compile("x"))),
                                            RegexExpr(re.compile("y")),
                                        )
                                    ),
                                    ConcatExpr(
                                        (
                                            RegexExpr(re.compile("\n")),
                                            ConcatExpr(
                                                (
                                                    RegexExpr(re.compile(r" *")),
                                                    RegexExpr(re.compile("n")),
                                                    RegexExpr(re.compile("l")),
                                                ),
                                                RelativeIndentation.greater,
                                            ),
                                        )
                                    ),
                                    ConcatExpr(
                                        (
                                            RegexExpr(re.compile("\n")),
                                            ConcatExpr(
                                                (
                                                    RegexExpr(re.compile(r" *")),
                                                    RegexExpr(re.compile("N")),
                                                    RegexExpr(re.compile("L")),
                                                )
                                            ),
                                        )
                                    ),
                                    ConcatExpr(
                                        (
                                            PlusExpr(RegexExpr(re.compile(r"\."))),
                                            RuleExpr("eof"),
                                        )
                                    ),
                                    ConcatExpr(
                                        (
                                            RegexExpr(re.compile("\n")),
                                            PlusExpr(
                                                RegexExpr(
                                                    re.compile(r" *in\n"),
                                                    RelativeIndentation.equal,
                                                )
                                            ),
                                            RuleExpr("eof"),
                                        )
                                    ),
                                    RuleExpr("hi_rule"),
                                )
                            ),
                        )
                    ),
                    "hi_rule": ConcatExpr(
                        (RegexExpr(re.compile("h")), RegexExpr(re.compile("i")),)
                    ),
                    "eof": LookaheadExpr(RegexExpr(re.compile("."))),
                },
            )
        )

        if exp_parse_error is None:
            assert parser.parse(string)
        else:
            with pytest.raises(ParseError) as exc_info:
                parser.parse(string)
            print(exc_info.value)
            assert exc_info.value == exp_parse_error


class TestParseError:
    @pytest.mark.parametrize(
        "parse_error, exp_string",
        [
            # Special case: No expected expressions
            (ParseError(1, 1, "", set()), "Parsing failure"),
            # Expected a rule (name should not be escaped)
            (
                ParseError(1, 1, "", {(RuleExpr("foo"), frozenset({None}))}),
                "Expected foo",
            ),
            # Expected a regex (pattern should be escaped)
            (
                ParseError(
                    1, 1, "", {(RegexExpr(re.compile("foo\nbar")), frozenset({None}))}
                ),
                r"Expected 'foo\nbar'",
            ),
            # Expected indentation
            (
                ParseError(
                    1,
                    1,
                    "",
                    {
                        (
                            RuleExpr("foo"),
                            frozenset(
                                {AbsoluteIndentation(10, RelativeIndentation.equal)}
                            ),
                        )
                    },
                ),
                r"Expected foo (with indentation = 10)",
            ),
            # Expected multiple indentations
            (
                ParseError(
                    1,
                    1,
                    "",
                    {
                        (
                            RuleExpr("foo"),
                            frozenset(
                                {
                                    AbsoluteIndentation(10, RelativeIndentation.equal),
                                    AbsoluteIndentation(20, RelativeIndentation.equal),
                                }
                            ),
                        )
                    },
                ),
                r"Expected foo (with indentation = 10 or = 20)",
            ),
            (
                ParseError(
                    1,
                    1,
                    "",
                    {
                        (
                            RuleExpr("foo"),
                            frozenset(
                                {
                                    AbsoluteIndentation(10, RelativeIndentation.equal),
                                    AbsoluteIndentation(
                                        10, RelativeIndentation.greater
                                    ),
                                }
                            ),
                        )
                    },
                ),
                r"Expected foo (with indentation = 10 or > 10)",
            ),
            # Expected indentation or non-indentation
            (
                ParseError(
                    1,
                    1,
                    "",
                    {
                        (
                            RuleExpr("foo"),
                            frozenset(
                                {
                                    None,
                                    AbsoluteIndentation(10, RelativeIndentation.equal),
                                }
                            ),
                        )
                    },
                ),
                r"Expected foo (optionally with indentation = 10)",
            ),
        ],
    )
    def test_explain_expected_default(
        self, parse_error: ParseError, exp_string: str
    ) -> None:
        assert parse_error.explain_expected() == exp_string

    @pytest.mark.parametrize(
        "expr_explanations, exp_string",
        [
            # No explanations
            ({}, "Expected bar or baz or foo"),
            # Explain non-indented
            ({RuleExpr("foo"): "FOO"}, "Expected FOO or bar or baz"),
            # Explain ignore indent
            (
                {RuleExpr("foo", RelativeIndentation.equal): "FOO"},
                "Expected FOO or bar or baz",
            ),
            ({RuleExpr("baz"): "BAZ"}, "Expected BAZ or bar or foo"),
            # Omit values
            ({RuleExpr("foo"): None}, "Expected bar or baz"),
            # Omit all values
            (
                {RuleExpr("foo"): None, RuleExpr("bar"): None, RuleExpr("baz"): None},
                "Parsing failure",
            ),
            # Multiple values mapped to same name
            ({RuleExpr("foo"): "X", RuleExpr("bar"): "X"}, "Expected X or baz"),
        ],
    )
    def test_explain_expected_expr_explanations(
        self,
        expr_explanations: Mapping[Union[RuleExpr, RegexExpr], Optional[str]],
        exp_string: str,
    ) -> None:
        parse_error = ParseError(
            1,
            1,
            "",
            {
                (RuleExpr("foo"), frozenset({None})),
                (RuleExpr("bar"), frozenset({None})),
                (RuleExpr("baz", RelativeIndentation.equal), frozenset({None})),
            },
        )
        assert parse_error.explain_expected(expr_explanations) == exp_string

    @pytest.mark.parametrize(
        "parse_error, last_resort_exprs, exp_string",
        [
            # No last resort
            (
                ParseError(
                    1,
                    1,
                    "",
                    {
                        (RuleExpr("foo"), frozenset({None})),
                        (RuleExpr("bar"), frozenset({None})),
                        (RuleExpr("baz"), frozenset({None})),
                    },
                ),
                set(),
                "Expected bar or baz or foo",
            ),
            # Last resort specified and removed when other options available
            (
                ParseError(
                    1,
                    1,
                    "",
                    {
                        (RuleExpr("foo"), frozenset({None})),
                        (RuleExpr("bar"), frozenset({None})),
                        (RuleExpr("baz"), frozenset({None})),
                    },
                ),
                {RuleExpr("foo"), RuleExpr("qux")},
                "Expected bar or baz",
            ),
            # Last resort specified and not removed when no other options
            # available
            (
                ParseError(1, 1, "", {(RuleExpr("foo"), frozenset({None}))}),
                {RuleExpr("foo"), RuleExpr("qux")},
                "Expected foo",
            ),
            # Ignore indent requirements
            (
                ParseError(
                    1,
                    1,
                    "",
                    {
                        (
                            RuleExpr("foo", RelativeIndentation.greater),
                            frozenset({None}),
                        ),
                        (RuleExpr("bar"), frozenset({None})),
                    },
                ),
                {RuleExpr("foo", RelativeIndentation.equal)},
                "Expected bar",
            ),
        ],
    )
    def test_explain_expected_last_resort(
        self,
        parse_error: ParseError,
        last_resort_exprs: Set[Union[RuleExpr, RegexExpr]],
        exp_string: str,
    ) -> None:
        assert (
            parse_error.explain_expected(last_resort_exprs=last_resort_exprs)
            == exp_string
        )

    @pytest.mark.parametrize(
        "just_indentation, exp_string_with_indentation, exp_string_without_indentation",
        [
            (
                False,
                (
                    "Expected bar (with indentation = 10) "
                    "or baz (optionally with indentation = 10) "
                    "or foo"
                ),
                "Expected bar or baz or foo",
            ),
            (
                True,
                (
                    "Expected bar (with indentation = 10) "
                    "or baz (optionally with indentation = 10)"
                ),
                "Expected bar or baz or foo",
            ),
        ],
    )
    def test_explain_expected_just_indentation(
        self,
        just_indentation: bool,
        exp_string_with_indentation: str,
        exp_string_without_indentation: str,
    ) -> None:
        parse_error_with_indentation = ParseError(
            1,
            1,
            "",
            {
                (RuleExpr("foo"), frozenset({None})),
                (
                    RuleExpr("bar"),
                    frozenset({AbsoluteIndentation(10, RelativeIndentation.equal)}),
                ),
                (
                    RuleExpr("baz"),
                    frozenset(
                        {None, AbsoluteIndentation(10, RelativeIndentation.equal)}
                    ),
                ),
            },
        )
        parse_error_without_indentation = ParseError(
            1,
            1,
            "",
            {
                (RuleExpr("foo"), frozenset({None})),
                (RuleExpr("bar"), frozenset({None})),
                (RuleExpr("baz"), frozenset({None})),
            },
        )
        assert (
            parse_error_with_indentation.explain_expected(
                just_indentation=just_indentation
            )
            == exp_string_with_indentation
        )
        assert (
            parse_error_without_indentation.explain_expected(
                just_indentation=just_indentation
            )
            == exp_string_without_indentation
        )

    def test_explain_expected_last_resort_and_just_indentation(self) -> None:
        parse_error = ParseError(
            1,
            1,
            "",
            {
                (RuleExpr("foo"), frozenset({None})),
                (
                    RuleExpr("bar"),
                    frozenset({AbsoluteIndentation(10, RelativeIndentation.equal)}),
                ),
                (
                    RuleExpr("baz"),
                    frozenset({AbsoluteIndentation(10, RelativeIndentation.equal)}),
                ),
            },
        )

        assert parse_error.explain_expected(
            last_resort_exprs={RuleExpr("bar")}, just_indentation=True,
        ) == ("Expected baz (with indentation = 10)")

        assert parse_error.explain_expected(
            last_resort_exprs={RuleExpr("bar"), RuleExpr("baz")}, just_indentation=True,
        ) == ("Expected foo")

    def test_explain(self) -> None:
        parse_error = ParseError(
            123, 5, "foo bar", {(RuleExpr("baz"), frozenset({None}))}
        )

        assert parse_error.explain() == (
            "At line 123 column 5:\n" "    foo bar\n" "        ^\n" "Expected baz"
        )
