from typing import Any, Mapping, MutableMapping, Tuple, List, cast

from peggie.parser.parser import (
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
    Concat,
    Star,
    Rule,
    Maybe,
    Plus,
)

import re


class GrammarCompileError(Exception):
    """Thrown during grammar compilation if the grammar is not valid."""


class RuleDefinedMultipleTimesError(GrammarCompileError):
    """Name redefined in a grammar."""


class GrammarNotWellFormedError(GrammarCompileError):
    """The grammar is not well formed."""


def assert_is_rule(parse_tree: Any) -> Rule:
    assert isinstance(parse_tree, Rule)
    return parse_tree


def compile_grammar(parse_tree: Rule) -> Grammar:
    """
    Compile a grammar from a parse tree produced using the PEG meta grammar.
    """
    assert parse_tree.name == "Grammar"

    assert isinstance(parse_tree.value, Concat)
    _spacing, definitions, _eof = parse_tree.value.values
    assert isinstance(definitions, Plus)

    start_rule = None
    rules: MutableMapping[str, Expr] = {}

    for definition in definitions.values:
        name, expression = compile_definition(assert_is_rule(definition))

        if start_rule is None:
            start_rule = name

        if name in rules:
            raise RuleDefinedMultipleTimesError(name)

        rules[name] = expression

    assert start_rule is not None

    grammar = Grammar(rules=rules, start_rule=start_rule)

    well_formed = grammar.is_well_formed()
    if not well_formed:
        raise GrammarNotWellFormedError(well_formed)

    return grammar


def compile_definition(parse_tree: Rule) -> Tuple[str, Expr]:
    assert parse_tree.name == "Definition"

    concat = parse_tree.value
    assert isinstance(concat, Concat)

    name = concat.values[0].flatten()[0]
    assert isinstance(name, str)

    expr_rule = concat.values[2]
    assert isinstance(expr_rule, Rule)
    expr = compile_expression(expr_rule)

    return (name, expr)


def compile_expression(parse_tree: Rule) -> Expr:
    assert parse_tree.name == "Expression"

    sequences: List[Expr] = []
    rules = parse_tree.value.flatten((Rule,))
    sequences.append(compile_sequence(assert_is_rule(rules.pop(0))))
    while rules:
        assert assert_is_rule(rules.pop(0)).name == "SLASH"
        sequences.append(compile_sequence(assert_is_rule(rules.pop(0))))

    if len(sequences) == 1:
        return sequences[0]
    else:
        return AltExpr(tuple(sequences))


def compile_sequence(parse_tree: Rule) -> Expr:
    assert parse_tree.name == "Sequence"

    expressions: List[Expr] = []
    assert isinstance(parse_tree.value, Star)
    for rule in parse_tree.value.values:
        expressions.append(compile_lookahead_prefix(assert_is_rule(rule)))

    if len(expressions) == 1:
        return expressions[0]
    else:
        return ConcatExpr(tuple(expressions))


def compile_lookahead_prefix(parse_tree: Rule) -> Expr:
    assert parse_tree.name == "LookaheadPrefix"
    assert isinstance(parse_tree.value, Concat)

    prefix_tree, expr_tree = parse_tree.value.values

    expr = compile_arity_suffix(assert_is_rule(expr_tree))

    # Apply lookahead condition (if required)
    prefix = prefix_tree.flatten()
    if prefix and prefix[0] == "&":
        expr = PositiveLookaheadExpr(expr)
    elif prefix and prefix[0] == "!":
        expr = LookaheadExpr(expr)
    else:
        assert prefix == []

    return expr


def compile_arity_suffix(parse_tree: Rule) -> Expr:
    assert parse_tree.name == "AritySuffix"
    assert isinstance(parse_tree.value, Concat)

    primary_tree, suffix_tree = parse_tree.value.values

    expr = compile_indent_rule_prefix(assert_is_rule(primary_tree))

    # Apply repeition condition (if required)
    suffix = suffix_tree.flatten()
    if suffix and suffix[0] == "?":
        expr = MaybeExpr(expr)
    elif suffix and suffix[0] == "*":
        expr = StarExpr(expr)
    elif suffix and suffix[0] == "+":
        expr = PlusExpr(expr)
    else:
        assert suffix == []

    return expr


def compile_indent_rule_prefix(parse_tree: Rule) -> Expr:
    assert parse_tree.name == "IndentRulePrefix"
    assert isinstance(parse_tree.value, Concat)

    indent_rule, primary = parse_tree.value.values

    expr = compile_primary(assert_is_rule(primary))

    assert isinstance(indent_rule, Maybe)
    if indent_rule.value is not None:
        expr = expr.with_indentation(
            compile_indent_rule(assert_is_rule(indent_rule.value))
        )

    return expr


def compile_indent_rule(parse_tree: Rule) -> RelativeIndentation:
    assert parse_tree.name == "IndentRule"
    return RelativeIndentation(parse_tree.flatten()[0])


def compile_primary(parse_tree: Rule) -> Expr:
    assert parse_tree.name == "Primary"
    assert isinstance(parse_tree.value, Alt)

    if parse_tree.value.choice_index == 0:  # (Expression)
        assert isinstance(parse_tree.value.value, Concat)
        _open, expr_rule, _close = parse_tree.value.value.values
        return compile_expression(assert_is_rule(expr_rule))
    elif parse_tree.value.choice_index == 1:  # Literal
        return compile_literal(assert_is_rule(parse_tree.value.value))
    elif parse_tree.value.choice_index == 2:  # Class
        return compile_class(assert_is_rule(parse_tree.value.value))
    elif parse_tree.value.choice_index == 3:  # DOT
        return RegexExpr(re.compile(r".", re.DOTALL))
    elif parse_tree.value.choice_index == 4:  # Identifier
        name = parse_tree.value.value.flatten()[0]
        assert isinstance(name, str)
        return RuleExpr(name)
    else:
        assert False


def compile_literal(parse_tree: Rule) -> Expr:
    assert parse_tree.name == "Literal"
    assert isinstance(parse_tree.value, Alt)
    assert isinstance(parse_tree.value.value, Concat)
    concat = parse_tree.value.value

    regex_flag, _open, chars, _close, _spacing = concat.values

    assert isinstance(regex_flag, Maybe)
    is_regex = regex_flag.value is not None

    string = "".join(map(compile_char, cast(List[Rule], chars.flatten((Rule,)))))

    if is_regex:
        return RegexExpr(re.compile(string, re.DOTALL))
    else:
        return RegexExpr(re.compile(re.escape(string), re.DOTALL))


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


def compile_char(parse_tree: Rule) -> str:
    assert parse_tree.name == "Char"

    parts = cast(List[str], parse_tree.flatten())

    if len(parts) == 1:  # Non-escaped char
        return parts[0]
    elif len(parts) == 2:  # Escaped char
        if parts[1] in ESCAPE_CHARS:
            return ESCAPE_CHARS[parts[1]]
        else:
            return "".join(parts)
    else:
        assert False


def compile_class(parse_tree: Rule) -> Expr:
    assert parse_tree.name == "Class"
    assert isinstance(parse_tree.value, Concat)

    _open, ranges, _close, _spacing = parse_tree.value.values

    range_specs = map(compile_range, cast(List[Rule], ranges.flatten((Rule,))))

    return RegexExpr(re.compile("[{}]".format("".join(range_specs)), re.DOTALL))


def compile_range(parse_tree: Rule) -> str:
    assert parse_tree.name == "Range"
    assert isinstance(parse_tree.value, Alt)

    def compile_and_escape_char(char: ParseTree) -> str:
        return re.escape(compile_char(assert_is_rule(char)))

    if parse_tree.value.choice_index == 0:  # Range
        assert isinstance(parse_tree.value.value, Concat)
        lhs, _dash, rhs = parse_tree.value.value.values
        return "{}-{}".format(
            compile_and_escape_char(lhs), compile_and_escape_char(rhs)
        )
    elif parse_tree.value.choice_index == 1:  # Single value
        return compile_and_escape_char(parse_tree.value.value)
    else:
        assert False
