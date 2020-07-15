"""
A compiled grammar for the grammar syntax supported by this parser. See also
``meta_grammar.peg``.
"""

import os

import re

from peggie.parser import (
    AltExpr,
    ConcatExpr,
    StarExpr,
    LookaheadExpr,
    RuleExpr,
    RegexExpr,
    MaybeExpr,
    PlusExpr,
    Grammar,
)

grammar_source = open(
    os.path.join(os.path.dirname(__file__), "meta_grammar.peg"), "r"
).read()
"""
A textual description of the meta grammar which matches PEG grammar
specifications.
"""

grammar: Grammar = Grammar(
    start_rule="grammar",
    rules={
        "grammar": ConcatExpr(
            (
                RuleExpr("spacing"),
                PlusExpr(RuleExpr("definition")),
                RuleExpr("end_of_file"),
            )
        ),
        "definition": ConcatExpr(
            (RuleExpr("identifier"), RuleExpr("LEFTARROW"), RuleExpr("expression"),)
        ),
        "expression": ConcatExpr(
            (
                RuleExpr("sequence"),
                StarExpr(ConcatExpr((RuleExpr("SLASH"), RuleExpr("sequence"),))),
            )
        ),
        "sequence": StarExpr(RuleExpr("lookahead_prefix")),
        "lookahead_prefix": ConcatExpr(
            (
                MaybeExpr(AltExpr((RuleExpr("AND"), RuleExpr("NOT"),))),
                RuleExpr("arity_suffix"),
            )
        ),
        "arity_suffix": ConcatExpr(
            (
                RuleExpr("indent_rule_prefix"),
                MaybeExpr(
                    AltExpr((RuleExpr("QUESTION"), RuleExpr("STAR"), RuleExpr("PLUS"),))
                ),
            )
        ),
        "indent_rule_prefix": ConcatExpr(
            (MaybeExpr(RuleExpr("indent_rule")), RuleExpr("primary"))
        ),
        "primary": AltExpr(
            (
                ConcatExpr(
                    (RuleExpr("OPEN"), RuleExpr("expression"), RuleExpr("CLOSE"),)
                ),
                RuleExpr("literal"),
                RuleExpr("class"),
                RuleExpr("DOT"),
                ConcatExpr(
                    (RuleExpr("identifier"), LookaheadExpr(RuleExpr("LEFTARROW")),)
                ),
            )
        ),
        "identifier": ConcatExpr(
            (
                RegexExpr(re.compile(r"[a-zA-Z_][a-zA-Z0-9_]*", re.DOTALL)),
                RuleExpr("spacing"),
            )
        ),
        "literal": AltExpr(
            (
                ConcatExpr(
                    (
                        MaybeExpr(RegexExpr(re.compile(r"r", re.DOTALL))),
                        RegexExpr(re.compile(r"\'", re.DOTALL)),
                        StarExpr(
                            ConcatExpr(
                                (
                                    LookaheadExpr(
                                        RegexExpr(re.compile(r"\'", re.DOTALL))
                                    ),
                                    RuleExpr("char"),
                                )
                            ),
                        ),
                        RegexExpr(re.compile(r"\'", re.DOTALL)),
                        RuleExpr("spacing"),
                    )
                ),
                ConcatExpr(
                    (
                        MaybeExpr(RegexExpr(re.compile(r"r", re.DOTALL))),
                        RegexExpr(re.compile(r"\"", re.DOTALL)),
                        StarExpr(
                            ConcatExpr(
                                (
                                    LookaheadExpr(
                                        RegexExpr(re.compile(r"\"", re.DOTALL))
                                    ),
                                    RuleExpr("char"),
                                )
                            ),
                        ),
                        RegexExpr(re.compile(r"\"", re.DOTALL)),
                        RuleExpr("spacing"),
                    )
                ),
            )
        ),
        "class": ConcatExpr(
            (
                RegexExpr(re.compile(r"\[", re.DOTALL)),
                PlusExpr(
                    ConcatExpr(
                        (
                            LookaheadExpr(RegexExpr(re.compile(r"\]", re.DOTALL))),
                            RuleExpr("range"),
                        )
                    ),
                ),
                RegexExpr(re.compile(r"\]", re.DOTALL)),
                RuleExpr("spacing"),
            )
        ),
        "range": AltExpr(
            (
                ConcatExpr(
                    (
                        RuleExpr("char"),
                        RegexExpr(re.compile(r"\-", re.DOTALL)),
                        RuleExpr("char"),
                    )
                ),
                RuleExpr("char"),
            )
        ),
        "char": AltExpr(
            (
                ConcatExpr(
                    (
                        RegexExpr(re.compile(r"\\", re.DOTALL)),
                        RegexExpr(re.compile(r".", re.DOTALL)),
                    )
                ),
                ConcatExpr(
                    (
                        LookaheadExpr(RegexExpr(re.compile(r"\\", re.DOTALL))),
                        RegexExpr(re.compile(r".", re.DOTALL)),
                    )
                ),
            )
        ),
        "indent_rule": ConcatExpr(
            (RegexExpr(re.compile(r"@(\*|=|>=|>)", re.DOTALL)), RuleExpr("spacing"),)
        ),
        "LEFTARROW": ConcatExpr(
            (RegexExpr(re.compile(r"\<\-", re.DOTALL)), RuleExpr("spacing"))
        ),
        "SLASH": ConcatExpr(
            (RegexExpr(re.compile(r"\/", re.DOTALL)), RuleExpr("spacing"))
        ),
        "AND": ConcatExpr(
            (RegexExpr(re.compile(r"\&", re.DOTALL)), RuleExpr("spacing"))
        ),
        "NOT": ConcatExpr(
            (RegexExpr(re.compile(r"\!", re.DOTALL)), RuleExpr("spacing"))
        ),
        "QUESTION": ConcatExpr(
            (RegexExpr(re.compile(r"\?", re.DOTALL)), RuleExpr("spacing"))
        ),
        "STAR": ConcatExpr(
            (RegexExpr(re.compile(r"\*", re.DOTALL)), RuleExpr("spacing"))
        ),
        "PLUS": ConcatExpr(
            (RegexExpr(re.compile(r"\+", re.DOTALL)), RuleExpr("spacing"))
        ),
        "OPEN": ConcatExpr(
            (RegexExpr(re.compile(r"\(", re.DOTALL)), RuleExpr("spacing"))
        ),
        "CLOSE": ConcatExpr(
            (RegexExpr(re.compile(r"\)", re.DOTALL)), RuleExpr("spacing"))
        ),
        "DOT": ConcatExpr(
            (RegexExpr(re.compile(r"\.", re.DOTALL)), RuleExpr("spacing"))
        ),
        "spacing": StarExpr(AltExpr((RuleExpr("space"), RuleExpr("comment")))),
        "comment": ConcatExpr(
            (
                RegexExpr(re.compile(r"\#", re.DOTALL)),
                StarExpr(
                    ConcatExpr(
                        (
                            LookaheadExpr(RuleExpr("end_of_line")),
                            RegexExpr(re.compile(r".", re.DOTALL)),
                        )
                    ),
                ),
                RuleExpr("end_of_line"),
            )
        ),
        "space": AltExpr(
            (RegexExpr(re.compile("[\\ \\\t]", re.DOTALL)), RuleExpr("end_of_line"))
        ),
        "end_of_line": RegexExpr(re.compile("\r\n|\n\r|\n|\r", re.DOTALL)),
        "end_of_file": LookaheadExpr(RegexExpr(re.compile(r".", re.DOTALL))),
    },
)
"""
A meta grammar which matches PEG grammar specifications.
"""
