"""A base for building parse tree transformers."""

from typing import Any

from peggie.parser.parser import (
    ParseTree,
    Regex,
    Alt,
    Maybe,
    Empty,
    Lookahead,
    PositiveLookahead,
    Rule,
)


class ParseTreeTransformer:
    """
    By default, this transformer will produce a representation containing a
    hierarchy of lists containing :py:class:`Regex` or string objects and None.

    Transformations may be customised by defining methods with the name of
    the rule to be transformed. These will be called with the
    :py:class:`ParseTree` of the matched rule along with the transformed
    value of the body of the rule. Methods should return the newly
    transformed body. If no matching method is defined, the
    :py:meth:`_default` method will be called.

    Methods named ``<rule_name>_enter`` will be called (if defined) before
    the children of a rule are transformed.

    The default transformation for Regex values is to return the matched
    string. This can be changed by overriding :py:meth:`_transform_regex`.
    """

    def transform(self, tree: ParseTree) -> Any:
        """
        Transform the provided parse tree with this transformer.
        """
        if isinstance(tree, Regex):
            return self._transform_regex(tree)
        elif isinstance(tree, Alt):
            return self.transform(tree.value)
        elif isinstance(tree, Maybe):
            if tree.value is not None:
                return self.transform(tree.value)
            else:
                return None
        elif isinstance(tree, (Empty, Lookahead, PositiveLookahead)):
            return None
        elif isinstance(tree, Rule):
            enter_fn = getattr(self, "{}_enter".format(tree.name), None)
            if enter_fn is not None:
                enter_fn(tree.value)

            processed_children = self.transform(tree.value)

            process_fn = getattr(self, tree.name, self._default)
            return process_fn(tree.value, processed_children)
        else:
            return [self.transform(child) for child in tree.iter_children()]

    def _transform_regex(self, regex: Regex) -> Any:
        """
        The default transformation for Regex.

        This default implementation returns the matched string but this method
        may be overridden to return custom values instead.
        """
        return regex.string

    def _default(self, tree: ParseTree, transformed_children: Any) -> Any:
        """The default transformation for rules."""
        return transformed_children
