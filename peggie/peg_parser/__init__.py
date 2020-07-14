"""
A Parsing Expression Grammar (PEG) Parser.
"""

from peggie.peg_parser.parser import *
from peggie.peg_parser.transformer import *
from peggie.peg_parser.grammar_compiler import *

# NB: These names are explicitly re-exported here because mypy in strict mode
# does not allow implicit re-exports. The completeness of this list is tested
# by the test suite.
__all__ = [  # noqa: F405
    # parser.*
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
    # transformer.*
    "ParseTreeTransformer",
    # grammar_compiler.*
    "GrammarCompileError",
    "RuleDefinedMultipleTimesError",
    "GrammarNotWellFormedError",
    "compile_grammar",
]
