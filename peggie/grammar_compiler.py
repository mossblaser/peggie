from typing import Any, Mapping, Tuple, List, cast

from collections import Counter

from peggie.transformer import ParseTreeTransformer

from peggie.meta_grammar import grammar as meta_grammar

from peggie.parser import (
    Parser,
    RelativeIndentation,
    Expr,
    AltExpr,
    ConcatExpr,
    StarExpr,
    LookaheadExpr,
    RuleExpr,
    RegexExpr,
    MaybeExpr,
    PlusExpr,
    PositiveLookaheadExpr,
    Grammar,
    ParseTree,
    Alt,
)

import re


__all__ = [
    "GrammarCompileError",
    "RuleDefinedMultipleTimesError",
    "GrammarNotWellFormedError",
    "compile_grammar",
]


class GrammarCompileError(Exception):
    """Thrown during grammar compilation if the grammar is not valid."""


class RuleDefinedMultipleTimesError(GrammarCompileError):
    """Name redefined in a grammar."""


class GrammarNotWellFormedError(GrammarCompileError):
    """The grammar is not well formed."""


ESCAPE_CHARS: Mapping[str, str] = {
    "\\": "\\",
    "'": "'",
    '"': '"',
    "a": "\a",
    "b": "\b",
    "f": "\f",
    "n": "\n",
    "r": "\r",
    "t": "\t",
    "v": "\v",
}
"""
An enumeration of all valid escape sequences. All others will be passed through
as a backslash followed by the escaped character.
"""


class GrammarTransformer(ParseTreeTransformer):
    """
    Transformer which builds a :py:class:`.Grammar` from a
    :py:class:`.ParseTree` generated by parsing the grammar
    :py:data:`peggie.meta_grammar.grammar`.
    """

    def grammar(self, _pt: ParseTree, children: Any) -> Grammar:
        _spacing, definitions, _eof = children
        definitions = cast(List[Tuple[str, Expr]], definitions)

        # Check for duplicate rule names
        counts = Counter(name for name, _expr in definitions)
        for name, count in counts.items():
            if count > 1:
                raise RuleDefinedMultipleTimesError(name)

        grammar = Grammar(rules=dict(definitions), start_rule=definitions[0][0])

        # Check for well-formedness
        well_formed = grammar.is_well_formed()
        if not well_formed:
            raise GrammarNotWellFormedError(well_formed)

        return grammar

    def definition(self, _pt: ParseTree, children: Any) -> Tuple[str, Expr]:
        identifier, _arrow, expression = children
        return (cast(str, identifier), cast(Expr, expression))

    def expression(self, _pt: ParseTree, children: Any) -> Expr:
        expr, alternates = children
        if alternates:
            return AltExpr(
                (cast(Expr, expr),)
                + tuple(cast(Expr, alt_expr) for _slash, alt_expr in alternates)
            )
        else:
            return cast(Expr, expr)

    def sequence(self, _pt: ParseTree, children: List[Expr]) -> Expr:
        if len(children) == 1:
            return children[0]
        else:
            return ConcatExpr(tuple(children))

    def lookahead_prefix(self, _pt: ParseTree, children: Any) -> Expr:
        prefix, expr = children
        if prefix is None:
            return cast(Expr, expr)
        elif prefix[0] == "!":
            return LookaheadExpr(cast(Expr, expr))
        elif prefix[0] == "&":
            return PositiveLookaheadExpr(cast(Expr, expr))
        else:
            raise TypeError(prefix[0])  # Unreachable

    def arity_suffix(self, _pt: ParseTree, children: Any) -> Expr:
        expr, suffix = children
        if suffix is None:
            return cast(Expr, expr)
        elif suffix[0] == "?":
            return MaybeExpr(cast(Expr, expr))
        elif suffix[0] == "*":
            return StarExpr(cast(Expr, expr))
        elif suffix[0] == "+":
            return PlusExpr(cast(Expr, expr))
        else:
            raise TypeError(suffix[0])  # Unreachable

    def indent_rule_prefix(self, _pt: ParseTree, children: Any) -> Expr:
        indent_rule, expr = children
        if indent_rule is None:
            return cast(Expr, expr)
        else:
            return cast(Expr, expr).with_indentation(
                RelativeIndentation(indent_rule[0])
            )

    def primary(self, parse_tree: Alt, child: Any) -> Expr:
        if parse_tree.choice_index == 0:  # ( Expression )
            _open, expr, _close = child
            return cast(Expr, expr)
        elif parse_tree.choice_index in (1, 2):  # Literal, Class
            return cast(Expr, child)
        elif parse_tree.choice_index == 3:  # DOT
            return RegexExpr(re.compile(r".", re.DOTALL))
        elif parse_tree.choice_index == 4:  # Rule
            return RuleExpr(cast(str, child[0]))
        else:
            raise TypeError(parse_tree.choice_index)  # Unreachable

    def class_(self, _pt: ParseTree, children: Any) -> RegexExpr:
        _open, lookaheads_and_ranges, _close, _spacing = children
        ranges = (cast(str, r) for _la, r in lookaheads_and_ranges)
        return RegexExpr(re.compile(r"[{}]".format("".join(ranges)), re.DOTALL))

    def range(self, parse_tree: Alt, child: Any) -> str:
        if parse_tree.choice_index == 0:  # Range
            lhs, dash, rhs = cast(List[str], child)
            return re.escape(lhs) + dash + re.escape(rhs)
        elif parse_tree.choice_index == 1:  # Single value
            return re.escape(cast(str, child))
        else:
            raise TypeError(parse_tree.choice_index)  # Unreachable

    def char(self, _pt: ParseTree, children: Any) -> str:
        if children[0] is None:
            return cast(str, children[1])
        else:
            backslash, char = cast(List[str], children)
            if char in ESCAPE_CHARS:
                return ESCAPE_CHARS[char]
            else:
                return backslash + char

    def literal(self, _pt: ParseTree, children: Any) -> RegexExpr:
        regex_flag, _open, lookaheads_and_chars, _close, _spacing = children
        string = "".join(cast(str, c) for _la, c in lookaheads_and_chars)
        if regex_flag is None:
            string = re.escape(string)
        return RegexExpr(re.compile(string, re.DOTALL))

    def identifier(self, _pt: ParseTree, children: Any) -> str:
        name, _spacing = children
        return cast(str, name)


def compile_grammar(grammar_spec: str) -> Grammar:
    """
    Parse and compile a PEG grammar from a string.

    Throws a :py:exc:`GrammarCompileError` exception if the supplied grammar is
    not well formed or a :py:exc:`.ParseError` if the grammar specification
    contains a syntax error.
    """
    parser = Parser(meta_grammar)
    parse_tree = parser.parse(grammar_spec)

    transformer = GrammarTransformer()
    grammar = cast(Grammar, transformer.transform(parse_tree))

    return grammar
