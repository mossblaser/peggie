"""
A Packrat Parsing Expression Grammar (PEG) Parser implementation.
"""


from enum import Enum

from dataclasses import dataclass, replace

from typing import (
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
    Union,
)


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

    def with_indentation(self, new_indentation: RelativeIndentation) -> "Expr":
        """Create a copy of this Expr with the specified relative indentation."""
        # NB: Relies on subclasses of this type being dataclasses
        return replace(self, indentation=new_indentation)


@dataclass(frozen=True)
class AltExpr(Expr):
    """Prioritised alternation: matches first matching expression."""

    exprs: Tuple[Expr, ...]
    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass(frozen=True)
class ConcatExpr(Expr):
    """Concatenation of several expressions."""

    exprs: Tuple[Expr, ...]
    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass(frozen=True)
class StarExpr(Expr):
    """Kleene star: matches 0-or-more repetitions of an expression."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass(frozen=True)
class LookaheadExpr(Expr):
    """Negative lookahead. Matches when expression does not, without consuming input."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass(frozen=True)
class RuleExpr(Expr):
    """Match a named rule in the grammar."""

    name: str
    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass(frozen=True)
class RegexExpr(Expr):
    """Match a compiled :py:mod:`re` regular expression."""

    pattern: Pattern[str]
    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass(frozen=True)
class EmptyExpr(Expr):
    """Match the empty string."""

    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass(frozen=True)
class MaybeExpr(Expr):
    """Match an expression, or an empty string."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass(frozen=True)
class PlusExpr(Expr):
    """'Kleene plus': match 1-or-more repetitions of an expression."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass(frozen=True)
class PositiveLookaheadExpr(Expr):
    """Positive lookahead: match, but don't consume an expression."""

    expr: Expr
    indentation: RelativeIndentation = RelativeIndentation.any


@dataclass
class Grammar:
    """A PEG grammar description."""

    rules: Mapping[str, Expr]
    """The expression for each rule in the grammar."""

    start_rule: str = "start"
    """Name of the starting rule in :py:attr:`rules`."""


@dataclass(frozen=True)
class ParseTree:
    """A parse tree generated by the parser. Base class."""

    def iter_children(self) -> Iterable["ParseTree"]:
        """Iterate over child parse trees."""
        raise NotImplementedError()

    def flatten(
        self, keep: Tuple[Type["ParseTree"], ...] = ()
    ) -> List[Union["ParseTree", str]]:
        """
        Return a flattened version of this parse tree.

        By default, returns a flat list of strings (one for each Regex). If the
        ``keep`` argument is given, it should be a tuple of
        :py:class:`ParseTree` subclasses which should not be recursed into
        while flattening.
        """
        if isinstance(self, keep):
            return [self]
        else:
            out = []
            for child in self.iter_children():
                out.extend(child.flatten(keep))
            return out


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

    def flatten(
        self, keep: Tuple[Type[ParseTree], ...] = ()
    ) -> List[Union[ParseTree, str]]:
        if isinstance(self, keep):
            return [self]
        else:
            return [self.string]


@dataclass(frozen=True)
class Empty(ParseTree):
    """:py:class:`ParseTree` produced when a :py:class:`EmptyExpr` is parsed."""

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

    def iter_children(self) -> Iterable[ParseTree]:
        return iter(())


class GrammarError(Exception):
    """Thrown when a problem is encountered with the grammar during parsing."""


class RepeatedEmptyTermInGrammarError(GrammarError):
    """
    Thrown when a grammar contains a rule which repeats a term which matches
    the empty string.
    """


class LeftRecursiveGrammarError(GrammarError):
    """
    Thrown when a grammar contains a direct/indirect/hidden left-recursive
    rule.
    """


class UndefinedRuleError(GrammarError):
    """
    Thrown when a grammar contains a reference to an undefined rule.
    """


class ParseError(Exception):
    """Thrown when parsing fails."""

    def __init__(self, line: int, column: int, expected_patterns: List[str]) -> None:
        super().__init__()
        self.line = line
        self.column = column
        self.expected_patterns = expected_patterns

    def __str__(self) -> str:
        return "Parse error on line {}, column {}: expected {}".format(
            self.line, self.column, " or ".join(self.expected_patterns)
        )


class Parser:
    """A Packrat PEG parser."""

    _grammar: Grammar
    """The :py:class:`Grammar` to be parsed."""

    class _Step(NamedTuple):
        """Identifies a specific expression application during parsing."""

        expression: Expr
        offset: int

    class _Result(NamedTuple):
        """Identifies the result of matching an expression."""

        parse_tree: Optional[ParseTree]
        new_offset: int

    _cache: MutableMapping[_Step, _Result]
    """Packrat parsing cache."""

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

    def _parse_empty(self, expr: EmptyExpr) -> Optional[Empty]:
        return Empty()

    def _parse_regex(self, expr: RegexExpr) -> Optional[Regex]:
        start = self._offset
        match = expr.pattern.match(self._string[start:])
        if match is None:
            return None
        else:
            string = match.group(0)
            self._offset += len(string)
            return Regex(string, start)

    def _parse_alt(self, expr: AltExpr) -> Optional[Alt]:
        start_offset = self._offset

        for candidate_index, candidate_expr in enumerate(expr.exprs):
            parse_tree = self._parse(candidate_expr)
            if parse_tree is not None:
                return Alt(parse_tree, candidate_index)
            else:
                self._offset = start_offset

        return None

    def _check_indent(
        self, start_offset: int, indentation: RelativeIndentation
    ) -> bool:
        """
        Check if the current offset is correctly indented with respect to the
        indentation level used at ``start_offset`` and the indentation rule
        given in ``indentation``.
        """
        if start_offset >= len(self._string) or self._offset >= len(self._string):
            # At end of string, skip the check
            return True

        start_indent = self._indentations[start_offset]
        current_indent = self._indentations[self._offset]

        return indentation.check(start_indent, current_indent)

    def _parse_concat(self, expr: ConcatExpr) -> Optional[Concat]:
        start_offset = self._offset
        parse_trees = []
        for sub_expr in expr.exprs:
            # Check subexpression is correctly indented
            if not self._check_indent(start_offset, sub_expr.indentation):
                self._offset = start_offset
                return None

            # Check subexpression is matched
            parse_tree = self._parse(sub_expr)
            if parse_tree is None:
                self._offset = start_offset
                return None
            else:
                parse_trees.append(parse_tree)

        return Concat(tuple(parse_trees))

    def _parse_star(self, expr: StarExpr) -> Optional[Star]:
        start_offset = self._offset
        last_offset = self._offset
        parse_trees = []
        while True:
            # Check subexpression is correctly indented
            if not self._check_indent(start_offset, expr.expr.indentation):
                break

            # Check subexpression is matched
            parse_tree = self._parse(expr.expr)
            if parse_tree is None:
                break
            else:
                parse_trees.append(parse_tree)

            # Well-formedness sanity check: matched expression must not match
            # the empty string
            if self._offset <= last_offset:
                raise RepeatedEmptyTermInGrammarError(expr)
            last_offset = self._offset

        return Star(tuple(parse_trees))

    def _parse_lookahead(self, expr: LookaheadExpr) -> Optional[Lookahead]:
        start_offset = self._offset
        parse_tree = self._parse(expr.expr)
        self._offset = start_offset

        if parse_tree is not None:
            return None
        else:
            return Lookahead()

    def _parse_rule(self, expr: RuleExpr) -> Optional[Rule]:
        # Sanity check: all rules referenced
        if expr.name not in self._grammar.rules:
            raise UndefinedRuleError(expr.name)

        # Well-formedness sanity check: Rules must not include
        # direct/indirect/hidden left recursion.
        rule_instantiation = (expr.name, self._offset)
        if rule_instantiation in self._executing_rules:
            raise LeftRecursiveGrammarError(expr)
        self._executing_rules.add(rule_instantiation)

        try:
            parse_tree = self._parse(self._grammar.rules[expr.name])
            if parse_tree is not None:
                return Rule(expr.name, parse_tree)
            else:
                return None
        finally:
            self._executing_rules.remove(rule_instantiation)

    def _parse_maybe(self, expr: MaybeExpr) -> Optional[Maybe]:
        # NB: Syntactic sugar, implemented via other operators
        alt_expr = AltExpr((expr.expr, EmptyExpr()))
        # NB: Called directly to avoid caching things which aren't part of the
        # grammar.
        parse_tree = self._parse_alt(alt_expr)
        assert parse_tree is not None
        if parse_tree.choice_index == 0:
            return Maybe(parse_tree.value)
        else:
            return Maybe(None)

    def _parse_plus(self, expr: PlusExpr) -> Optional[Plus]:
        # NB: Syntactic sugar, implemented via other operators
        parse_tree = self._parse_star(StarExpr(expr.expr))
        if parse_tree is None:
            return None
        elif len(parse_tree.values) == 0:
            return None
        else:
            return Plus(parse_tree.values)

    def _parse_positive_lookahead(
        self, expr: PositiveLookaheadExpr
    ) -> Optional[PositiveLookahead]:
        # NB: Syntactic sugar, implemented via other operators
        lookahead_expr = LookaheadExpr(expr.expr)
        # NB: Called directly to avoid caching things which aren't part of the
        # grammar.
        parse_tree = self._parse_lookahead(lookahead_expr)
        if parse_tree is None:
            return PositiveLookahead()
        else:
            return None

    def _parse_no_cache(self, expr: Expr) -> Optional[ParseTree]:
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

    def _parse(self, expr: Expr) -> Optional[ParseTree]:
        step = Parser._Step(expr, self._offset)
        if step not in self._cache:
            self._cache[step] = Parser._Result(self._parse_no_cache(expr), self._offset)

        parse_tree, offset = self._cache[step]
        self._offset = offset
        return parse_tree

    def _get_last_non_matching(self) -> Tuple[int, int, List[str]]:
        """
        Enumerate the line, column and last expressions which failed to match.
        """
        last_offset = max(
            step.offset
            for step, result in self._cache.items()
            if result.parse_tree is None
        )

        lines = self._string[: last_offset + 1].splitlines(keepends=True)
        last_line = len(lines)
        last_column = len(([""] + lines)[-1])

        last_non_matching = [
            (
                step.expression.pattern.pattern
                if isinstance(step.expression, RegexExpr)
                else step.expression.name
                if isinstance(step.expression, RuleExpr)
                else ""  # Unreachable
            )
            for step, result in self._cache.items()
            if (
                step.offset == last_offset
                and result.parse_tree is None
                and isinstance(step.expression, (RegexExpr, RuleExpr))
            )
        ]
        return last_line, last_column, last_non_matching

    def parse(self, string: str) -> ParseTree:
        """
        Parse a string, returning the parse tree, if successful, and raises
        :py:exc:`ParseError` otherwise.
        """
        self._string = string
        self._indentations = string_to_indentations(self._string)
        self._offset = 0
        self._cache = {}
        self._executing_rules = set()
        parse_tree = self._parse(RuleExpr(self._grammar.start_rule))

        if parse_tree is not None:
            return parse_tree
        else:
            raise ParseError(*self._get_last_non_matching())
