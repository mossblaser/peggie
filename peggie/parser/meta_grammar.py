"""
A compiled grammar for the grammar syntax supported by this parser. See also
``meta_grammar.peg``.
"""

import os

import re

from peggie.parser.parser import (
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

grammar: Grammar = Grammar(
    start_rule="Grammar",
    rules={
        "Grammar": ConcatExpr(
            (
                RuleExpr("Spacing"),
                PlusExpr(RuleExpr("Definition")),
                RuleExpr("EndOfFile"),
            )
        ),
        "Definition": ConcatExpr(
            (RuleExpr("Identifier"), RuleExpr("LEFTARROW"), RuleExpr("Expression"),)
        ),
        "Expression": ConcatExpr(
            (
                RuleExpr("Sequence"),
                StarExpr(ConcatExpr((RuleExpr("SLASH"), RuleExpr("Sequence"),))),
            )
        ),
        "Sequence": StarExpr(RuleExpr("LookaheadPrefix")),
        "LookaheadPrefix": ConcatExpr(
            (
                MaybeExpr(AltExpr((RuleExpr("AND"), RuleExpr("NOT"),))),
                RuleExpr("AritySuffix"),
            )
        ),
        "AritySuffix": ConcatExpr(
            (
                RuleExpr("IndentRulePrefix"),
                MaybeExpr(
                    AltExpr((RuleExpr("QUESTION"), RuleExpr("STAR"), RuleExpr("PLUS"),))
                ),
            )
        ),
        "IndentRulePrefix": ConcatExpr(
            (MaybeExpr(RuleExpr("IndentRule")), RuleExpr("Primary"))
        ),
        "Primary": AltExpr(
            (
                ConcatExpr(
                    (RuleExpr("OPEN"), RuleExpr("Expression"), RuleExpr("CLOSE"),)
                ),
                RuleExpr("Literal"),
                RuleExpr("Class"),
                RuleExpr("DOT"),
                ConcatExpr(
                    (RuleExpr("Identifier"), LookaheadExpr(RuleExpr("LEFTARROW")),)
                ),
            )
        ),
        "Identifier": ConcatExpr(
            (
                RegexExpr(re.compile(r"[a-zA-Z_][a-zA-Z0-9_]*", re.DOTALL)),
                RuleExpr("Spacing"),
            )
        ),
        "Literal": AltExpr(
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
                                    RuleExpr("Char"),
                                )
                            ),
                        ),
                        RegexExpr(re.compile(r"\'", re.DOTALL)),
                        RuleExpr("Spacing"),
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
                                    RuleExpr("Char"),
                                )
                            ),
                        ),
                        RegexExpr(re.compile(r"\"", re.DOTALL)),
                        RuleExpr("Spacing"),
                    )
                ),
            )
        ),
        "Class": ConcatExpr(
            (
                RegexExpr(re.compile(r"\[", re.DOTALL)),
                PlusExpr(
                    ConcatExpr(
                        (
                            LookaheadExpr(RegexExpr(re.compile(r"\]", re.DOTALL))),
                            RuleExpr("Range"),
                        )
                    ),
                ),
                RegexExpr(re.compile(r"\]", re.DOTALL)),
                RuleExpr("Spacing"),
            )
        ),
        "Range": AltExpr(
            (
                ConcatExpr(
                    (
                        RuleExpr("Char"),
                        RegexExpr(re.compile(r"\-", re.DOTALL)),
                        RuleExpr("Char"),
                    )
                ),
                RuleExpr("Char"),
            )
        ),
        "Char": AltExpr(
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
        "IndentRule": ConcatExpr(
            (RegexExpr(re.compile(r"@(\*|=|>=|>)", re.DOTALL)), RuleExpr("Spacing"),)
        ),
        "LEFTARROW": ConcatExpr(
            (RegexExpr(re.compile(r"\<\-", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "SLASH": ConcatExpr(
            (RegexExpr(re.compile(r"\/", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "AND": ConcatExpr(
            (RegexExpr(re.compile(r"\&", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "NOT": ConcatExpr(
            (RegexExpr(re.compile(r"\!", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "QUESTION": ConcatExpr(
            (RegexExpr(re.compile(r"\?", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "STAR": ConcatExpr(
            (RegexExpr(re.compile(r"\*", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "PLUS": ConcatExpr(
            (RegexExpr(re.compile(r"\+", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "OPEN": ConcatExpr(
            (RegexExpr(re.compile(r"\(", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "CLOSE": ConcatExpr(
            (RegexExpr(re.compile(r"\)", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "DOT": ConcatExpr(
            (RegexExpr(re.compile(r"\.", re.DOTALL)), RuleExpr("Spacing"))
        ),
        "Spacing": StarExpr(AltExpr((RuleExpr("Space"), RuleExpr("Comment")))),
        "Comment": ConcatExpr(
            (
                RegexExpr(re.compile(r"\#", re.DOTALL)),
                StarExpr(
                    ConcatExpr(
                        (
                            LookaheadExpr(RuleExpr("EndOfLine")),
                            RegexExpr(re.compile(r".", re.DOTALL)),
                        )
                    ),
                ),
                RuleExpr("EndOfLine"),
            )
        ),
        "Space": AltExpr(
            (RegexExpr(re.compile("[\\ \\\t]", re.DOTALL)), RuleExpr("EndOfLine"))
        ),
        "EndOfLine": RegexExpr(re.compile("\r\n|\n\r|\n|\r", re.DOTALL)),
        "EndOfFile": LookaheadExpr(RegexExpr(re.compile(r".", re.DOTALL))),
    },
)
"""
A meta grammar which matches PEG grammar specifications.
"""
