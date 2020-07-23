"""
A Packrat Parsing Expression Grammar (PEG) Parser implementation.
"""

import re

from enum import Enum

from collections import defaultdict

from dataclasses import dataclass, replace, field

from typing import (
    FrozenSet,
    Iterable,
    List,
    Mapping,
    MutableMapping,
    NamedTuple,
    Optional,
    Pattern,
    Sequence,
    Set,
    Tuple,
    Type,
    TypeVar,
    Union,
)

from peggie.error_message_generation import (
    offset_to_line_and_column,
    extract_line,
    format_error_message,
)


__all__ = [
    "RelativeIndentation",
    "GrammarWellFormedness",
    "WellFormed",
    "UndefinedRule",
    "LeftRecursion",
    "RepeatedEmptyTerm",
    "Expr",
    "AltExpr",
    "ConcatExpr",
    "StarExpr",
    "LookaheadExpr",
    "RuleExpr",
    "RegexExpr",
    "EmptyExpr",
    "MaybeExpr",
    "PlusExpr",
    "PositiveLookaheadExpr",
    "Grammar",
    "ParseTree",
    "Alt",
    "Concat",
    "Star",
    "Lookahead",
    "Rule",
    "Regex",
    "Empty",
    "Maybe",
    "Plus",
    "PositiveLookahead",
    "GrammarError",
    "RepeatedEmptyTermError",
    "LeftRecursionError",
    "UndefinedRuleError",
    "AbsoluteIndentation",
    "ParseError",
    "Parser",
]


def string_to_indentations(string: str) -> List[int]:
    r"""
    Return the indentation level associated with each char in a string.

    The indentiation level of a character is shared by all characters in a
    line. For example, in the string "hello\n  world", all of the charachters
    in "hello\n" have an indentation level of 0 (including the newline
    character) while all of the characters in "  world" have an indentation
    level of 2 (including the leading spaces).

    .. note::

        Only ASCII horizontal tabs and spaces are treated as indentation
        characters. Both are treated as equal in the degree of indentation.
    """
    indentations = []
    for line_num, line in enumerate(string.splitlines(keepends=True)):
        indentation = len(line) - len(line.lstrip(" \t"))
        indentations.extend([indentation] * len(line))
    return indentations


class RelativeIndentation(Enum):
    """
    Describes the required relative indentation level of a pair of tokens.
    """

    any = "@*"
    equal = "@="
    greater_or_equal = "@>="
    greater = "@>"

    def check(self, a: int, b: int) -> bool:
        """
        Check if two indentation levels have the relationship described by this
        :py:class:`RelativeIndentation` value.
        """
        if self == RelativeIndentation.any:
            return True
        elif self == RelativeIndentation.equal:
            return b == a
        elif self == RelativeIndentation.greater_or_equal:
            return b >= a
        elif self == RelativeIndentation.greater:
            return b > a
        else:
            # Should be unreachable!
            raise NotImplementedError(self)


@dataclass
class GrammarWellFormedness:
    """Base class of result from a well-formedness test."""

    def __bool__(self) -> bool:
        return False


@dataclass
class WellFormed(GrammarWellFormedness):
    """The grammar is well formed."""

    def __bool__(self) -> bool:
        return True


@dataclass
class UndefinedRule(GrammarWellFormedness):
    """The grammar refers to an undefined rule."""

    name: str


@dataclass
class LeftRecursion(GrammarWellFormedness):
    """The grammar contains a left-recursive rule."""

    name: str


@dataclass
class RepeatedEmptyTerm(GrammarWellFormedness):
    """The grammar contains a repeated empty term."""

    expr: "Expr"


ExprT = TypeVar("ExprT", bound="Expr")


class Expr:
    """An expression in a PEG grammar. Abstract base class."""

    indentation: RelativeIndentation
    """
    Extension to PEG to support indentation-sensitive parsing.  Specifies a
    relative indentation requirement for the start of this expression.

    This attribute is only used for elements matched within a
    :py:class:`Concat` and :py:class:`Star`. Here, this requirement checks the
    relative indentation of each expression with respect to the first
    expression matched.
    """

    def with_indentation(self: ExprT, new_indentation: RelativeIndentation) -> ExprT:
        """Create a copy of this Expr with the specified relative indentation."""
        # NB: Relies on subclasses of this type being dataclasses
        return replace(self, indentation=new_indentation)

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        """Iterate over the sub-expressions within this expression."""
        raise NotImplementedError()

    def iter_first_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        """
        Iterate over subexpressions which may be matched first by this
        expression.
        """
        return self.iter_subexpressions(grammar)

    def iter_first_rules_and_regexes(
        self, grammar: "Grammar"
    ) -> Iterable[Union["RuleExpr", "RegexExpr"]]:
        r"""
        Iterate over the :py:class:`RuleExpr` or :py:class:`RegexExpr`\ s which
        are matched by this Expr. Does not recurse into rules.
        """
        for child in self.iter_first_subexpressions(grammar):
            yield from child.iter_first_rules_and_regexes(grammar)

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        """
        Test if this expression can match the empty string (in the context of
        the specified grammar).
        """
        raise NotImplementedError()

    def first_set(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> Set["Expr"]:
        r"""
        Return the set of :py:class:`Expr`\ s which are immediateliy matched by
        this :py:class:`Expr`. Results are recursive.
        """
        if _visited_rules is None:
            _visited_rules = set()

        first = set()
        for subexpression in self.iter_first_subexpressions(grammar):
            first.add(subexpression)
            first.update(subexpression.first_set(grammar, _visited_rules))
        return first

    def is_well_formed(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> GrammarWellFormedness:
        """
        Is this expression well-formed? That is, does it lack any repeated
        empty expressions, left recursion or undefined rules.
        """
        if _visited_rules is None:
            _visited_rules = set()

        for expr in self.iter_subexpressions(grammar):
            well_formed = expr.is_well_formed(grammar, _visited_rules)
            if not well_formed:
                return well_formed
        return WellFormed()


@dataclass(frozen=True)
class AltExpr(Expr):
    """Prioritised alternation: matches first matching expression."""

    exprs: Tuple[Expr, ...]
    indentation: RelativeIndentation = RelativeIndentation.any

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        return iter(self.exprs)

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        if _visited_rules is None:
            _visited_rules = set()
        return any(e.matches_empty(grammar, _visited_rules) for e in self.exprs)


@dataclass(frozen=True)
class ConcatExpr(Expr):
    """Concatenation of several expressions."""

    exprs: Tuple[Expr, ...]
    indentation: RelativeIndentation = RelativeIndentation.any

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        return iter(self.exprs)

    def iter_first_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        for expr in self.exprs:
            yield expr
            if not expr.matches_empty(grammar):
                break

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        if _visited_rules is None:
            _visited_rules = set()
        return all(e.matches_empty(grammar, _visited_rules) for e in self.exprs)


@dataclass(frozen=True)
class StarExpr(Expr):
    """Kleene star: matches 0-or-more repetitions of an expression."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        return iter((self.expr,))

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        return True

    def is_well_formed(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> GrammarWellFormedness:
        if self.expr.matches_empty(grammar):
            return RepeatedEmptyTerm(self)
        return super().is_well_formed(grammar, _visited_rules)


@dataclass(frozen=True)
class LookaheadExpr(Expr):
    """Negative lookahead. Matches when expression does not, without consuming input."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        return iter((self.expr,))

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        return True


@dataclass(frozen=True)
class RuleExpr(Expr):
    """Match a named rule in the grammar."""

    name: str
    indentation: RelativeIndentation = RelativeIndentation.any

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        # Special case; to avoid crash when undefined rule is used
        if self.name in grammar.rules:
            return iter((grammar.rules[self.name],))
        else:
            return iter(())

    def iter_first_rules_and_regexes(
        self, grammar: "Grammar"
    ) -> Iterable[Union["RuleExpr", "RegexExpr"]]:
        yield self

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        if _visited_rules is None:
            _visited_rules = set()

        if self.name in _visited_rules:
            return True  # Left-recursive rule encountered; halt recursion
        else:
            _visited_rules.add(self.name)
            result = grammar.rules[self.name].matches_empty(grammar, _visited_rules)
            _visited_rules.remove(self.name)
            return result

    def first_set(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> Set[Expr]:
        r"""
        Return the set of :py:class:`Expr`\ s which are immediateliy matched by
        this :py:class:`Expr`. Results are recursive.
        """
        if _visited_rules is None:
            _visited_rules = set()

        if self.name in _visited_rules:
            return set()
        else:
            _visited_rules.add(self.name)

        return super().first_set(grammar, _visited_rules)

    def is_well_formed(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> GrammarWellFormedness:
        if self.name not in grammar.rules:
            return UndefinedRule(self.name)
        if self in self.first_set(grammar):
            return LeftRecursion(self.name)

        if _visited_rules is None:
            _visited_rules = set()
        if self.name in _visited_rules:
            return WellFormed()
        _visited_rules.add(self.name)

        return super().is_well_formed(grammar, _visited_rules)


RegexExprT = TypeVar("RegexExprT", bound="RegexExpr")


@dataclass(frozen=True)
class RegexExpr(Expr):
    """
    Match a compiled :py:mod:`re` regular expression. If a string is provided,
    it will be compiled into a regular expression with the :py:data:`re.DOTALL`
    flag set.
    """

    pattern: Pattern[str]
    indentation: RelativeIndentation = RelativeIndentation.any

    def __init__(
        self,
        pattern: Union[Pattern[str], str],
        indentation: RelativeIndentation = RelativeIndentation.any,
    ) -> None:
        if isinstance(pattern, str):
            pattern = re.compile(pattern, re.DOTALL)

        object.__setattr__(self, "pattern", pattern)
        object.__setattr__(self, "indentation", indentation)

    @classmethod
    def literal(
        cls: Type[RegexExprT],
        text: str,
        indentation: RelativeIndentation = RelativeIndentation.any,
    ) -> RegexExprT:
        """Return a :py:class:`RegexExpr` matching a string literal."""
        return cls(re.escape(text), indentation)

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        return iter(())

    def iter_first_rules_and_regexes(
        self, grammar: "Grammar"
    ) -> Iterable[Union["RuleExpr", "RegexExpr"]]:
        yield self

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        return self.pattern.match("") is not None


@dataclass(frozen=True)
class EmptyExpr(Expr):
    """Match the empty string."""

    indentation: RelativeIndentation = RelativeIndentation.any

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        return iter(())

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        return True


@dataclass(frozen=True)
class MaybeExpr(Expr):
    """Match an expression, or an empty string."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        return iter((self.expr,))

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        return True


@dataclass(frozen=True)
class PlusExpr(Expr):
    """'Kleene plus': match 1-or-more repetitions of an expression."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        return iter((self.expr,))

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        return self.expr.matches_empty(grammar, _visited_rules)

    def is_well_formed(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> GrammarWellFormedness:
        if self.expr.matches_empty(grammar):
            return RepeatedEmptyTerm(self)
        return super().is_well_formed(grammar, _visited_rules)


@dataclass(frozen=True)
class PositiveLookaheadExpr(Expr):
    """Positive lookahead: match, but don't consume an expression."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any

    def iter_subexpressions(self, grammar: "Grammar") -> Iterable["Expr"]:
        return iter((self.expr,))

    def matches_empty(
        self, grammar: "Grammar", _visited_rules: Optional[Set[str]] = None
    ) -> bool:
        return True


@dataclass
class Grammar:
    """A PEG grammar description."""

    rules: Mapping[str, Expr]
    """The expression for each rule in the grammar."""

    start_rule: str = "start"
    """Name of the starting rule in :py:attr:`rules`."""

    def is_well_formed(self) -> GrammarWellFormedness:
        """
        Is this grammar well-formed? That is, is it free from missing rules,
        (direct/indirect/hidden) left recursive rules and iteration of empty
        patterns?
        """
        if self.start_rule not in self.rules:
            return UndefinedRule(self.start_rule)
        visited_rules: Set[str] = set()
        for name in self.rules:
            well_formed = RuleExpr(name).is_well_formed(self, visited_rules)
            if not well_formed:
                return well_formed
        return WellFormed()


@dataclass(frozen=True)
class ParseTree:
    """A parse tree generated by the parser. Base class."""

    def __bool__(self) -> bool:
        return True

    def iter_children(self) -> Iterable["ParseTree"]:
        """Iterate over child parse trees."""
        raise NotImplementedError()


@dataclass(frozen=True)
class Alt(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`AltExpr` is parsed."""

    value: ParseTree
    choice_index: int

    def iter_children(self) -> Iterable[ParseTree]:
        return iter((self.value,))


@dataclass(frozen=True)
class Concat(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`ConcatExpr` is parsed."""

    values: Tuple[ParseTree, ...]

    def iter_children(self) -> Iterable[ParseTree]:
        return iter(self.values)


@dataclass(frozen=True)
class Star(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`StarExpr` is parsed."""

    values: Tuple[ParseTree, ...]

    def iter_children(self) -> Iterable[ParseTree]:
        return iter(self.values)


@dataclass(frozen=True)
class Lookahead(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`LookaheadExpr` is parsed."""

    offset: int
    """The character offset where this lookahead was matched."""

    def iter_children(self) -> Iterable[ParseTree]:
        return iter(())


@dataclass(frozen=True)
class Rule(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`RuleExpr` is parsed."""

    name: str
    value: ParseTree

    def iter_children(self) -> Iterable[ParseTree]:
        return iter((self.value,))


@dataclass(frozen=True)
class Regex(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`RegexExpr` is parsed."""

    string: str
    """The matched string."""

    start: int
    """The offset at which this string appears in the parser input."""

    @property
    def end(self) -> int:
        """
        The offset just beyond the end of this string as it appears in the parser input.
        """
        return self.start + len(self.string)

    def iter_children(self) -> Iterable[ParseTree]:
        return iter(())


@dataclass(frozen=True)
class Empty(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`EmptyExpr` is parsed."""

    offset: int
    """The character offset where this empty was matched."""

    def iter_children(self) -> Iterable[ParseTree]:
        return iter(())


@dataclass(frozen=True)
class Maybe(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`MaybeExpr` is parsed."""

    value: Optional[ParseTree]
    """If the expression matched, its :py:class:`ParseTree`, otherwise None."""

    def iter_children(self) -> Iterable[ParseTree]:
        return iter((self.value,)) if self.value is not None else iter(())


@dataclass(frozen=True)
class Plus(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`PlusExpr` is parsed."""

    values: Tuple[ParseTree, ...]

    def iter_children(self) -> Iterable[ParseTree]:
        return iter(self.values)


@dataclass(frozen=True)
class PositiveLookahead(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`PositiveLookaheadExpr` is parsed."""

    offset: int
    """The character offset where this positive lookahead was matched."""

    def iter_children(self) -> Iterable[ParseTree]:
        return iter(())


@dataclass(frozen=True)
class ParseFailure:
    """Base type for parse failiure outcomes."""

    expression: Expr
    """The :py:class:`Expr` which failed to match."""

    offset: int
    """Offset at which the failure occurred."""

    def __bool__(self) -> bool:
        return False


@dataclass(frozen=True)
class UnmatchedExpression(ParseFailure):
    """Produced when an expression is not matched."""

    pass


@dataclass(frozen=True)
class UnmatchedIndentation(ParseFailure):
    """Produced when an :py:class:`RelativeIndentation` requirement is not met."""

    start_indentation: int
    """The indentation level the rule is relative to."""

    indentation: RelativeIndentation
    """The required (unmet) indentation rule."""


ParseResult = Union[ParseTree, ParseFailure]


class GrammarError(Exception):
    """Thrown when a problem is encountered with the grammar during parsing."""


class RepeatedEmptyTermError(GrammarError):
    """
    Thrown when a grammar contains a rule which repeats a term which matches
    the empty string.
    """


class LeftRecursionError(GrammarError):
    """
    Thrown when a grammar contains a direct/indirect/hidden left-recursive
    rule.
    """


class UndefinedRuleError(GrammarError):
    """
    Thrown when a grammar contains a reference to an undefined rule.
    """


class AbsoluteIndentation(NamedTuple):
    """An absolute indentation requirement."""

    start_indentation: int
    """The indentation level the rule is relative to."""

    indentation: RelativeIndentation
    """The required (unmet) :py:class:`RelativeIndentation` rule."""


@dataclass
class ParseError(Exception):
    """
    Thrown when parsing fails.

    Parameters
    ----------
    line : int
        One-indexed line number where the error occurred.
    column : int
        One-indexed column number where the error occurred.
    snippet : str
        The contents of the offending line.
    expectations : {(rule_or_regex, {AbsoluteIndentation or None, ...}), ...}
        The set of :py:class:`RuleExpr` and :py:class:`RegexExpr` expressions
        which the parser would have accepted at this point, along any required
        differences in indentation.
    expr_explanations : {rule_or_regex: str or None, ...}
        Error message customization parameter. By default expected
        :py:class:`.Rule` are shown as their rule name and :py:class:`.Regex`
        as their pattern (in quotes). This representation may be overridden by
        a string entry in this dictionary. Alternatively, an expression may be
        suppressed from the explanation by providing None.  Default = {}.
    last_resort_exprs : {rule_or_regex, ...}
        Error message customization parameter. A set of :py:class:`.Rule` or
        :py:class:`.Regex` expressions which are ordinarily suppressed from
        explanations except when these are the only matching expressions in
        which case they are included. Default = {}.
    just_indentation : bool
        Error message customization parameter. If True, when at least one
        expression with an indentation requirement is present, non-indentation
        related expectations are suppressed. Default = False.
    """

    line: int
    column: int
    snippet: str
    expectations: Set[
        Tuple[Union[RuleExpr, RegexExpr], FrozenSet[Optional[AbsoluteIndentation]]]
    ]

    expr_explanations: Mapping[Union[RuleExpr, RegexExpr], Optional[str]] = field(
        default_factory=dict
    )
    last_resort_exprs: Set[Union[RuleExpr, RegexExpr]] = field(default_factory=set)
    just_indentation: bool = field(default=False)

    def explain(
        self,
        expr_explanations: Optional[
            Mapping[Union[RuleExpr, RegexExpr], Optional[str]]
        ] = None,
        last_resort_exprs: Optional[Set[Union[RuleExpr, RegexExpr]]] = None,
        just_indentation: Optional[bool] = None,
    ) -> str:
        """
        Return a human-readable string describing the expected next values.

        Parameters
        ----------
        expr_explanations : {rule_or_regex: str or None, ...}
            See :py:attr:`expr_explanations`.
        last_resort_exprs : {rule_or_regex, ...}
            See :py:attr:`last_resort_exprs`.
        just_indentation : bool
            See :py:attr:`just_indentation`.
        """
        if expr_explanations is None:
            expr_explanations = self.expr_explanations
        if last_resort_exprs is None:
            last_resort_exprs = self.last_resort_exprs
        if just_indentation is None:
            just_indentation = self.just_indentation

        # Strip indentation specs from expressions to ensure just rule names or
        # patterns are matched
        expr_explanations = {
            expr.with_indentation(RelativeIndentation.any): exp
            for expr, exp in expr_explanations.items()
        }
        last_resort_exprs = {
            expr.with_indentation(RelativeIndentation.any) for expr in last_resort_exprs
        }

        # Accumulate explanations for all expected rules/regexes
        #
        # [(Explanation, last_resort, indentation_specified), ...]
        explanations: List[Tuple[str, bool, bool]] = []
        for rule_or_regex, unmatched_indentations in self.expectations:
            # Find explanation
            explanation: Optional[str]
            rule_or_regex_no_indent = rule_or_regex.with_indentation(
                RelativeIndentation.any
            )
            if rule_or_regex_no_indent in expr_explanations:
                explanation = expr_explanations[rule_or_regex_no_indent]
            elif isinstance(rule_or_regex, RuleExpr):
                explanation = rule_or_regex.name
            elif isinstance(rule_or_regex, RegexExpr):
                explanation = repr(rule_or_regex.pattern.pattern)
            else:
                raise TypeError(type(rule_or_regex))  # Unreachable...

            if explanation is None:
                continue

            # Add indentation explanation(s)
            required_indentations = {
                "{} {}".format(
                    unmatched_indentation.indentation.value[1:],
                    unmatched_indentation.start_indentation,
                )
                for unmatched_indentation in unmatched_indentations
                if unmatched_indentation is not None
            }
            if required_indentations:
                explanation += " ({}with indentation {})".format(
                    ("optionally " if None in unmatched_indentations else ""),
                    " or ".join(sorted(required_indentations)),
                )

            if explanation not in [e for (e, _l, _i) in explanations]:
                explanations.append(
                    (
                        explanation,
                        rule_or_regex.with_indentation(RelativeIndentation.any)
                        in last_resort_exprs,
                        bool(required_indentations),
                    )
                )

        # Hide last resort explanations unless they're all we've got
        if not all(last_resort for _e, last_resort, _i in explanations):
            explanations = [
                (e, last_resort, i)
                for e, last_resort, i in explanations
                if not last_resort
            ]

        # Hide non-indentation related explanations if any explanation relates
        # to indentation (and just_indentation set)
        if just_indentation and any(
            indentation_specified for _e, _l, indentation_specified in explanations
        ):
            explanations = [
                (e, l, indentation_specified)
                for e, l, indentation_specified in explanations
                if indentation_specified
            ]

        if explanations:
            return "Expected {}".format(
                " or ".join(sorted(explanation for explanation, _l, _i in explanations))
            )
        else:
            return "Parsing failure"

    def __str__(self) -> str:
        return format_error_message(
            self.line, self.column, self.snippet, self.explain()
        )


class Parser:
    """
    A parser.

    Strings are parsed using the :py:meth:`parse` method.

    Parameters
    ----------
    grammar : :py:class:`Grammar`
        The grammar describing the language to be parsed.
    """

    _grammar: Grammar
    """The :py:class:`Grammar` to be parsed."""

    class _Step(NamedTuple):
        """Identifies a specific expression application during parsing."""

        expression: Expr
        offset: int

    class _Result(NamedTuple):
        """Identifies the result of matching an expression."""

        parse_tree: ParseResult
        new_offset: int

    _cache: MutableMapping[_Step, _Result]
    """Packrat parsing cache."""

    _parse_failures: List[ParseFailure]
    """
    A list of parsing failures, in the order they are encountered. Failures
    relating to lookaheads are removed. This value is used during error message
    generation to find the furthest point parsing managed to reach.
    """

    _executing_rules: Set[Tuple[str, int]]
    """
    Used for an internal well-formedness sanity check. Verifies that no rule
    performs direct/indirect/hidden left recursion.
    """

    _string: str
    """The string currently being parsed."""

    _indentations: Sequence[int]
    """
    The indentation level for each char in :py:attr:`_string`, e.g. from
    :py:func:`string_to_indentations`.
    """

    _offset: int
    """The current parsing offset into :py:attr:`_string`."""

    def __init__(self, grammar: Grammar) -> None:
        self._grammar = grammar

    def _parse_empty(self, expr: EmptyExpr) -> Empty:
        return Empty(self._offset)

    def _parse_regex(self, expr: RegexExpr) -> Union[Regex, ParseFailure]:
        start_offset = self._offset
        match = expr.pattern.match(self._string[start_offset:])
        if match is None:
            return UnmatchedExpression(expr, start_offset)
        else:
            string = match.group(0)
            self._offset += len(string)
            return Regex(string, start_offset)

    def _parse_alt(self, expr: AltExpr) -> Union[Alt, ParseFailure]:
        start_offset = self._offset

        for candidate_index, candidate_expr in enumerate(expr.exprs):
            parse_tree = self._parse(candidate_expr)
            if parse_tree:
                assert isinstance(parse_tree, ParseTree)
                return Alt(parse_tree, candidate_index)
            else:
                self._offset = start_offset

        return UnmatchedExpression(expr, start_offset)

    def _check_indent(
        self, expr: Expr, block_start_offset: int, expr_start_offset: int
    ) -> Optional[UnmatchedIndentation]:
        """
        Check if the current offset is correctly indented with respect to the
        indentation level used at ``block_start_offset`` and the indentation
        rule for the provided expression. Returns an
        :py:class:`UnmatchedExpression` if the rule is not satisfied and None
        if it is.
        """
        if block_start_offset >= len(self._string) or expr_start_offset >= len(
            self._string
        ):
            # At end of string, skip the check
            return None

        start_indent = self._indentations[block_start_offset]
        current_indent = self._indentations[expr_start_offset]

        if expr.indentation.check(start_indent, current_indent):
            return None
        else:
            return UnmatchedIndentation(
                expr, expr_start_offset, start_indent, expr.indentation
            )

    def _parse_concat(self, expr: ConcatExpr) -> Union[Concat, ParseFailure]:
        start_offset = self._offset
        parse_trees = []
        for sub_expr in expr.exprs:
            # Check subexpression is matched
            old_num_parse_failures = len(self._parse_failures)
            before_offset = self._offset
            parse_tree = self._parse(sub_expr)

            # Check subexpression is correctly indented
            indentation_problem = self._check_indent(
                sub_expr, start_offset, before_offset
            )
            if indentation_problem is not None:
                # NB: In the event of an mis-indented expression, we wish
                # for the indentation failure to be the most recent
                # failure. As such we must discard any other failures
                # encountered while matching the expression above.
                del self._parse_failures[old_num_parse_failures:]

                # Special case: If a StarExpr or MaybeExpr is badly indented,
                # treat it as an empty match rather than an indentation error.
                # (If you really do want a Star or Maybe which matches something
                # to fail when indentation fails to match you can wrap it in a
                # concatenation with the empty string).
                if isinstance(sub_expr, StarExpr):
                    self._offset = before_offset
                    parse_tree = Star(())
                elif isinstance(sub_expr, MaybeExpr):
                    self._offset = before_offset
                    parse_tree = Maybe(None)
                else:
                    self._offset = start_offset
                    return indentation_problem

            # Fail if subexpression wasn't matched
            if not isinstance(parse_tree, ParseTree):
                self._offset = start_offset
                return UnmatchedExpression(expr, self._offset)

            parse_trees.append(parse_tree)

        return Concat(tuple(parse_trees))

    def _parse_star(self, expr: StarExpr) -> Star:
        start_offset = self._offset
        last_offset = self._offset
        parse_trees = []
        while True:
            # Check if next expression has appropriate indentation (and
            # therefore could be a match)
            indentation_problem = self._check_indent(
                expr.expr, start_offset, last_offset
            )
            if indentation_problem is not None:
                self._parse_failures.append(indentation_problem)
                break

            # Check subexpression is matched
            parse_tree = self._parse(expr.expr)
            if not isinstance(parse_tree, ParseTree):
                break

            # Well-formedness sanity check: must not have matched the empty
            # string
            if self._offset <= last_offset:
                raise RepeatedEmptyTermError(expr)
            last_offset = self._offset

            parse_trees.append(parse_tree)

        return Star(tuple(parse_trees))

    def _parse_lookahead(self, expr: LookaheadExpr) -> Union[Lookahead, ParseFailure]:
        old_num_parse_failures = len(self._parse_failures)
        try:
            start_offset = self._offset
            parse_tree = self._parse(expr.expr)
            self._offset = start_offset

            if parse_tree:
                return UnmatchedExpression(expr, self._offset)
            else:
                return Lookahead(self._offset)
        finally:
            # Erase all failures noted during the lookahead
            del self._parse_failures[old_num_parse_failures:]

    def _parse_rule(self, expr: RuleExpr) -> Union[Rule, ParseFailure]:
        # Sanity check: all rules referenced
        if expr.name not in self._grammar.rules:
            raise UndefinedRuleError(expr.name)

        # Well-formedness sanity check: Rules must not include
        # direct/indirect/hidden left recursion.
        rule_instantiation = (expr.name, self._offset)
        if rule_instantiation in self._executing_rules:
            raise LeftRecursionError(expr)
        self._executing_rules.add(rule_instantiation)

        try:
            parse_tree = self._parse(self._grammar.rules[expr.name])
            if parse_tree:
                assert isinstance(parse_tree, ParseTree)
                return Rule(expr.name, parse_tree)
            else:
                return UnmatchedExpression(expr, self._offset)
        finally:
            self._executing_rules.remove(rule_instantiation)

    def _parse_maybe(self, expr: MaybeExpr) -> Maybe:
        # NB: Syntactic sugar, implemented via other operators
        alt_expr = AltExpr((expr.expr, EmptyExpr()))
        # NB: Called directly to avoid caching things which aren't part of the
        # grammar.
        parse_tree = self._parse_alt(alt_expr)
        assert isinstance(parse_tree, Alt)
        if parse_tree.choice_index == 0:
            return Maybe(parse_tree.value)
        else:
            return Maybe(None)

    def _parse_plus(self, expr: PlusExpr) -> Union[Plus, ParseFailure]:
        # NB: Syntactic sugar, implemented via other operators
        parse_tree = self._parse_star(StarExpr(expr.expr))
        if len(parse_tree.values) == 0:
            return UnmatchedExpression(expr, self._offset)
        else:
            return Plus(parse_tree.values)

    def _parse_positive_lookahead(
        self, expr: PositiveLookaheadExpr
    ) -> Union[PositiveLookahead, ParseFailure]:
        # NB: Syntactic sugar, implemented via other operators
        lookahead_expr = LookaheadExpr(expr.expr)
        # NB: Called directly to avoid caching things which aren't part of the
        # grammar.
        parse_tree = self._parse_lookahead(lookahead_expr)
        if not parse_tree:
            return PositiveLookahead(self._offset)
        else:
            return UnmatchedExpression(expr, self._offset)

    def _parse_no_cache(self, expr: Expr) -> ParseResult:
        if isinstance(expr, EmptyExpr):
            return self._parse_empty(expr)
        elif isinstance(expr, RegexExpr):
            return self._parse_regex(expr)
        elif isinstance(expr, AltExpr):
            return self._parse_alt(expr)
        elif isinstance(expr, ConcatExpr):
            return self._parse_concat(expr)
        elif isinstance(expr, StarExpr):
            return self._parse_star(expr)
        elif isinstance(expr, LookaheadExpr):
            return self._parse_lookahead(expr)
        elif isinstance(expr, RuleExpr):
            return self._parse_rule(expr)
        elif isinstance(expr, MaybeExpr):
            return self._parse_maybe(expr)
        elif isinstance(expr, PlusExpr):
            return self._parse_plus(expr)
        elif isinstance(expr, PositiveLookaheadExpr):
            return self._parse_positive_lookahead(expr)
        else:
            # Should be unreachable...
            raise TypeError(type(expr))

    def _parse(self, expr: Expr) -> ParseResult:
        step = Parser._Step(expr, self._offset)
        if step not in self._cache:
            parse_result = self._parse_no_cache(expr)
            self._cache[step] = Parser._Result(parse_result, self._offset)

        parse_result, offset = self._cache[step]
        self._offset = offset
        if isinstance(parse_result, ParseFailure):
            self._parse_failures.append(parse_result)
        return parse_result

    def _get_parse_error(self) -> ParseError:
        """
        Produce a :py:exc:`ParseError` describing the current parsing faliure.
        """
        # Assume the error occurred at the position of the longest parse
        offset = max(failure.offset for failure in self._parse_failures)
        line, column = offset_to_line_and_column(self._string, offset)

        # Find all of the candidate expressions which failed at this point
        expected_exprs: Set[Expr] = set()
        expected_expr_indentations: MutableMapping[
            Expr, Set[Optional[AbsoluteIndentation]]
        ] = defaultdict(set)
        for failure in self._parse_failures:
            if failure.offset != offset:
                continue

            expected_indentation: Optional[AbsoluteIndentation] = None
            if isinstance(failure, UnmatchedIndentation):
                expected_indentation = AbsoluteIndentation(
                    failure.start_indentation, failure.indentation,
                )

            for expr in set([failure.expression]) | failure.expression.first_set(
                self._grammar
            ):
                expected_exprs.add(expr)
                expected_expr_indentations[expr].add(expected_indentation)

        # Keep only the top-most expressions
        for expr in expected_exprs.copy():
            for subexpr in expr.iter_first_subexpressions(self._grammar):
                expected_exprs.discard(subexpr)

        # Simplify into the rules/regexes which failed to match
        expectations = {
            (rule_or_regex, frozenset(expected_expr_indentations[rule_or_regex]))
            for expr in expected_exprs
            for rule_or_regex in expr.iter_first_rules_and_regexes(self._grammar)
        }

        snippet = extract_line(self._string, line)
        return ParseError(line, column, snippet, expectations)

    def parse(self, string: str) -> Rule:
        """
        Parse a string, returning the :py:class:`ParseTree` if successful or
        raising a :py:exc:`ParseError` if not.
        """
        self._string = string
        self._indentations = string_to_indentations(self._string)
        self._offset = 0
        self._cache = {}
        self._parse_failures = []
        self._executing_rules = set()
        parse_tree = self._parse(RuleExpr(self._grammar.start_rule))

        if parse_tree:
            assert isinstance(parse_tree, Rule)
            return parse_tree
        else:
            raise self._get_parse_error()
