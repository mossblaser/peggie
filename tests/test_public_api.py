from peggie import __all__ as peggie_all

from peggie.parser import __all__ as parser_all
from peggie.transformer import __all__ as transformer_all
from peggie.grammar_compiler import __all__ as grammar_compiler_all


def test_all_is_complete() -> None:
    assert sorted(peggie_all) == sorted(
        parser_all + transformer_all + grammar_compiler_all
    )
