import pytest  # type: ignore

from typing import Any

from peggie.parser.parser import (
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
)

from peggie.parser.transformer import ParseTreeTransformer


@pytest.mark.parametrize(
    "parse_tree, exp_out",
    [
        # Regexes should be passed through
        (Regex("foo", 123), "foo"),
        # Concatenations, star and plus should be passed through as lists
        (Concat((Regex("foo", 0), Regex("bar", 3))), ["foo", "bar"]),
        (Plus((Regex("foo", 0), Regex("bar", 3))), ["foo", "bar"]),
        (Star((Regex("foo", 0), Regex("bar", 3))), ["foo", "bar"]),
        # Concatenation, star and list should be processed recursively
        (
            Concat(
                (
                    Plus((Regex("foo", 0), Regex("bar", 3))),
                    Plus((Regex("baz", 6), Regex("qux", 9))),
                )
            ),
            [["foo", "bar"], ["baz", "qux"]],
        ),
        (
            Star(
                (
                    Plus((Regex("foo", 0), Regex("bar", 3))),
                    Plus((Regex("baz", 6), Regex("qux", 9))),
                )
            ),
            [["foo", "bar"], ["baz", "qux"]],
        ),
        (
            Plus(
                (
                    Star((Regex("foo", 0), Regex("bar", 3))),
                    Star((Regex("baz", 6), Regex("qux", 9))),
                )
            ),
            [["foo", "bar"], ["baz", "qux"]],
        ),
        # Alternations should pass their contents transformed but without extra
        # wrapping
        (Alt(Concat((Regex("foo", 0), Regex("bar", 3))), 0), ["foo", "bar"]),
        # Maybe should pass its contents transformed but otherwise untouched
        (Maybe(Concat((Regex("foo", 0), Regex("bar", 3)))), ["foo", "bar"]),
        (Maybe(None), None),
        # Rules should pass their contents transformed without extra wrapping
        (Rule("xxx", Concat((Regex("foo", 0), Regex("bar", 3)))), ["foo", "bar"]),
        # Empty, Lookahead and PositiveLookahead should be shown as None
        (Empty(), None),
        (Lookahead(), None),
        (PositiveLookahead(), None),
    ],
)
def test_default_transformation(parse_tree: ParseTree, exp_out: Any) -> None:
    t = ParseTreeTransformer()
    assert t.transform(parse_tree) == exp_out


def test_custom_transform_regex() -> None:
    class MyTransformer(ParseTreeTransformer):
        def _transform_regex(self, regex: Regex) -> Any:
            return regex

    t = MyTransformer()
    assert t.transform(Concat((Regex("foo", 0), Regex("bar", 3)))) == [
        Regex("foo", 0),
        Regex("bar", 3),
    ]


def test_custom_rule_transformers() -> None:
    class MyTransformer(ParseTreeTransformer):
        def foo(self, parse_tree: ParseTree, transformed_children: Any) -> Any:
            assert parse_tree == Concat((Regex("foo", 0), Regex("bar", 3)))
            assert transformed_children == ["foo", "bar"]
            return transformed_children + ["baz"]

    t = MyTransformer()
    assert t.transform(Rule("foo", Concat((Regex("foo", 0), Regex("bar", 3))))) == [
        "foo",
        "bar",
        "baz",
    ]


def test_custom_rule_enter() -> None:
    call_log = []

    class MyTransformer(ParseTreeTransformer):
        def foo_enter(self, parse_tree: ParseTree) -> None:
            assert parse_tree == Concat((Rule("bar", Empty()), Rule("bar", Empty())))
            call_log.append("foo_enter")

        def foo(self, parse_tree: ParseTree, transformed_children: Any) -> Any:
            call_log.append("foo")
            assert parse_tree == Concat((Rule("bar", Empty()), Rule("bar", Empty())))
            assert transformed_children == ["bar", "bar"]
            return "foo"

        def bar(self, parse_tree: ParseTree, transformed_children: Any) -> Any:
            call_log.append("bar")
            assert parse_tree == Empty()
            assert transformed_children is None
            return "bar"

    t = MyTransformer()
    assert (
        t.transform(Rule("foo", Concat((Rule("bar", Empty()), Rule("bar", Empty())))))
        == "foo"
    )
    assert call_log == ["foo_enter", "bar", "bar", "foo"]
