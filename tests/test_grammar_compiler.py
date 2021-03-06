import pytest  # type: ignore

import re

from peggie.parser import (
    Expr,
    RegexExpr,
    RuleExpr,
    RelativeIndentation,
    LookaheadExpr,
    PositiveLookaheadExpr,
    MaybeExpr,
    StarExpr,
    PlusExpr,
    AltExpr,
    ConcatExpr,
    LeftRecursion,
)

from peggie.meta_grammar import (
    grammar,
    grammar_source,
)

from peggie.grammar_compiler import (
    compile_grammar,
    RuleDefinedMultipleTimesError,
    GrammarNotWellFormedError,
)


class TestCompileGrammar:
    def test_repeated_names_rejected(self) -> None:
        with pytest.raises(RuleDefinedMultipleTimesError):
            compile_grammar("name <- .\nname <- .")

    def test_start_rule_is_first_rule(self) -> None:
        g = compile_grammar("a <- 'ay'\nb <- 'bee'\nc <- 'see'")
        assert g.start_rule == "a"
        assert set(g.rules) == set(["a", "b", "c"])

    @pytest.mark.parametrize(
        "rule, exp_expr",
        [
            # Literal
            ("'fo+'", RegexExpr(re.compile(r"fo\+", re.DOTALL))),
            ('"fo+"', RegexExpr(re.compile(r"fo\+", re.DOTALL))),
            # Regex
            ("r'fo+'", RegexExpr(re.compile(r"fo+", re.DOTALL))),
            ('r"fo+"', RegexExpr(re.compile(r"fo+", re.DOTALL))),
            # Char escaped values
            ("r'\\n'", RegexExpr(re.compile("\n", re.DOTALL))),
            ("r'\\+'", RegexExpr(re.compile(r"\+", re.DOTALL))),
            # Class
            ("[abc]", RegexExpr(re.compile(r"[abc]", re.DOTALL))),
            ("[a-zA-Z0-9_]", RegexExpr(re.compile(r"[a-zA-Z0-9_]", re.DOTALL))),
            # Class: escaped values
            ("[\\n]", RegexExpr(re.compile("[\\\n]", re.DOTALL))),
            ("[\\[\\-\\]]", RegexExpr(re.compile(r"[\\\[\\\-\\\]]", re.DOTALL))),
            # Dot
            (".", RegexExpr(re.compile(r".", re.DOTALL))),
            # Reference to rule
            ("rule", RuleExpr("rule")),
            ("r", RuleExpr("r")),
            # Different indentation requirements
            ("@*rule", RuleExpr("rule", RelativeIndentation.any)),
            ("@=rule", RuleExpr("rule", RelativeIndentation.equal)),
            ("@>rule", RuleExpr("rule", RelativeIndentation.greater)),
            ("@>=rule", RuleExpr("rule", RelativeIndentation.greater_or_equal)),
            # Lookahead
            ("!rule", LookaheadExpr(RuleExpr("rule"))),
            ("&rule", PositiveLookaheadExpr(RuleExpr("rule"))),
            # Maybe
            ("rule?", MaybeExpr(RuleExpr("rule"))),
            # Star
            ("rule*", StarExpr(RuleExpr("rule"))),
            # Plus
            ("rule+", PlusExpr(RuleExpr("rule"))),
            # Alternation
            ("a / b / c", AltExpr((RuleExpr("a"), RuleExpr("b"), RuleExpr("c")))),
            # Concatenation
            ("a b c", ConcatExpr((RuleExpr("a"), RuleExpr("b"), RuleExpr("c")))),
            # Precidence rules
            (
                "a b+ / c !@=d?",
                AltExpr(
                    (
                        ConcatExpr((RuleExpr("a"), PlusExpr(RuleExpr("b")))),
                        ConcatExpr(
                            (
                                RuleExpr("c"),
                                LookaheadExpr(
                                    MaybeExpr(RuleExpr("d", RelativeIndentation.equal))
                                ),
                            )
                        ),
                    )
                ),
            ),
            # Unnecessary grouping (optimised out)
            ("(rule)", RuleExpr("rule")),
            ("((a) b)", ConcatExpr((RuleExpr("a"), RuleExpr("b")))),
            ("((a) / (b))", AltExpr((RuleExpr("a"), RuleExpr("b")))),
            ("(a)+", PlusExpr(RuleExpr("a"))),
            ("!((a)+)", LookaheadExpr(PlusExpr(RuleExpr("a")))),
            # Grouping
            (
                "(a / b) c",
                ConcatExpr((AltExpr((RuleExpr("a"), RuleExpr("b"))), RuleExpr("c"))),
            ),
        ],
    )
    def test_expression(self, rule: str, exp_expr: Expr) -> None:
        g = compile_grammar(
            "start <- {}\nrule<-.\na<-.\nb<-.\nc<-.\nd<-.\nr<-.".format(rule)
        )
        assert g.rules["start"] == exp_expr

    def test_well_formedness_enforced(self) -> None:
        with pytest.raises(GrammarNotWellFormedError) as exc_info:
            compile_grammar("start <- start")
        exc_info.value.args == (LeftRecursion("start"),)


def test_self_hosting() -> None:
    # Check that the PEG grammar can parse its own description and that it
    # results in the same compiled grammar representation.

    # import pprint
    # open("/tmp/a", "w").write(pprint.pformat(grammar.rules))
    # open("/tmp/b", "w").write(pprint.pformat(compile_grammar(grammar_source).rules))

    assert compile_grammar(grammar_source) == grammar
