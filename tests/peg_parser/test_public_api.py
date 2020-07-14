from peggie.peg_parser import __all__ as peg_parser_all

from peggie.peg_parser.parser import __all__ as parser_all
from peggie.peg_parser.transformer import __all__ as transformer_all
from peggie.peg_parser.grammar_compiler import __all__ as grammar_compiler_all


def test_all_is_complete() -> None:
    assert sorted(peg_parser_all) == sorted(
        parser_all + transformer_all + grammar_compiler_all
    )
