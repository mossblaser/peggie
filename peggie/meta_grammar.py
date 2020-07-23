"""
A compiled grammar for the grammar syntax supported by this parser. See also
``meta_grammar.peg``.
"""

import os

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
            (RegexExpr(r"[a-zA-Z_][a-zA-Z0-9_]*"), RuleExpr("spacing"),)
        ),
        "literal": AltExpr(
            (
                ConcatExpr(
                    (
                        MaybeExpr(RegexExpr.literal("r")),
                        RegexExpr.literal("'"),
                        StarExpr(
                            ConcatExpr(
                                (
                                    LookaheadExpr(RegexExpr.literal("'")),
                                    RuleExpr("char"),
                                )
                            ),
                        ),
                        RegexExpr.literal("'"),
                        RuleExpr("spacing"),
                    )
                ),
                ConcatExpr(
                    (
                        MaybeExpr(RegexExpr.literal("r")),
                        RegexExpr.literal('"'),
                        StarExpr(
                            ConcatExpr(
                                (
                                    LookaheadExpr(RegexExpr.literal('"')),
                                    RuleExpr("char"),
                                )
                            ),
                        ),
                        RegexExpr.literal('"'),
                        RuleExpr("spacing"),
                    )
                ),
            )
        ),
        "class": ConcatExpr(
            (
                RegexExpr.literal("["),
                PlusExpr(
                    ConcatExpr(
                        (LookaheadExpr(RegexExpr.literal("]")), RuleExpr("range"),)
                    ),
                ),
                RegexExpr.literal("]"),
                RuleExpr("spacing"),
            )
        ),
        "range": AltExpr(
            (
                ConcatExpr(
                    (RuleExpr("char"), RegexExpr.literal("-"), RuleExpr("char"),)
                ),
                RuleExpr("char"),
            )
        ),
        "char": AltExpr(
            (
                ConcatExpr((RegexExpr.literal("\\"), RegexExpr(r"."),)),
                ConcatExpr((LookaheadExpr(RegexExpr.literal("\\")), RegexExpr(r"."),)),
            )
        ),
        "indent_rule": ConcatExpr((RegexExpr(r"@(\*|=|>=|>)"), RuleExpr("spacing"),)),
        "LEFTARROW": ConcatExpr((RegexExpr.literal("<-"), RuleExpr("spacing"))),
        "SLASH": ConcatExpr((RegexExpr.literal("/"), RuleExpr("spacing"))),
        "AND": ConcatExpr((RegexExpr.literal("&"), RuleExpr("spacing"))),
        "NOT": ConcatExpr((RegexExpr.literal("!"), RuleExpr("spacing"))),
        "QUESTION": ConcatExpr((RegexExpr.literal("?"), RuleExpr("spacing"))),
        "STAR": ConcatExpr((RegexExpr.literal("*"), RuleExpr("spacing"))),
        "PLUS": ConcatExpr((RegexExpr.literal("+"), RuleExpr("spacing"))),
        "OPEN": ConcatExpr((RegexExpr.literal("("), RuleExpr("spacing"))),
        "CLOSE": ConcatExpr((RegexExpr.literal(")"), RuleExpr("spacing"))),
        "DOT": ConcatExpr((RegexExpr.literal("."), RuleExpr("spacing"))),
        "spacing": StarExpr(AltExpr((RuleExpr("space"), RuleExpr("comment")))),
        "comment": ConcatExpr(
            (
                RegexExpr.literal("#"),
                StarExpr(
                    ConcatExpr(
                        (LookaheadExpr(RuleExpr("end_of_line")), RegexExpr(r"."),)
                    ),
                ),
                RuleExpr("end_of_line"),
            )
        ),
        "space": AltExpr((RegexExpr("[\\ \\\t]"), RuleExpr("end_of_line"))),
        "end_of_line": RegexExpr("\r\n|\n\r|\n|\r"),
        "end_of_file": LookaheadExpr(RegexExpr(r".")),
    },
)
"""
A meta grammar which matches PEG grammar specifications.
"""
