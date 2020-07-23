r"""
Peggie is a simple parsing library which may be used to parse
text-based inputs given a suitable grammar.

Specifically, this parser implements a variant of the Parsing Expression
Grammars (PEG) [PEG]_ formalism with extensions to support indentation
sensitive parsing derived from the proposal by Adams and Ağacan [PEGIndent]_.
The parser itself uses the Packrat [Packrat]_ algorithm and is implemented in
pure Python. This parser distinguishes itself from other Python-based parsing
libraries in supporting indentation sensitivity directly rather than as a
(typically only semi-supported) bolt-on to the lexing process.

Basic usage
===========

A grammar should be defined in the typical PEG style (full details will be
given later). For example, a simple grammar for matching nested lists of
numbers such as ``[1, 20, 300]`` or ``[[1, 2, []], [3, [4]]]``::

    >>> grammar_source = r'''
    ...     start          <- space value space end_of_file
    ...     value          <- list_of_values / number
    ...     list_of_values <- "[" space (value space ("," space value space)*)? "]"
    ...     number         <- r"[0-9]+"
    ...     space          <- r"\s*"
    ...     end_of_file    <- !.
    ... '''

The first rule in the grammar is treated as the start rule. Quoted strings
match literals, and quoted strings proceeded by an ``r`` are treated as
Python-style regular expressions (see :py:mod:`re`). A dot (``.``) matches any
single character.  The ``?``, ``*`` and ``+`` operators match zero-or-one,
zero-or-more and one-or-more instances of a pattern respectively.  The ``!``
operator is a negative lookahead operator which only matches when the pattern
after it does not -- in this case matching only when no next value is present,
i.e. the end of the file.

Next, the grammar must be compiled using :py:func:`.compile_grammar`::

    >>> from peggie import compile_grammar
    >>> grammar = compile_grammar(grammar_source)

The compiled grammar may then be used by a :py:class:`.Parser` object to parse
strings according to the grammar. For example::

    >>> from peggie import Parser
    >>> parser = Parser(grammar)
    >>> parse_tree = parser.parse("[1, 2, [3, [4]], []]")

Parse trees may be conveniently transformed into a more useful representation
(or indeed evaluated into some final result) using a
:py:class:`.ParseTreeTransformer`. A :py:class:`.ParseTreeTransformer` subclass
should be constructed which defines a transformation to be carried out for each
rule in the grammar. For example, we can write a transformer which assembles a
Python list from the parse tree like so::

    >>> from peggie import ParseTreeTransformer

    >>> class ListTransformer(ParseTreeTransformer):
    ...     def number(self, parse_tree, result):
    ...         return int(result)
    ...
    ...     def list_of_values(self, parse_tree, result):
    ...         _open, _sp, body, _close = result
    ...         if body is None:
    ...             return []
    ...         else:
    ...             first, _sp, rest = body
    ...             out = [first]
    ...             for _comma, _sp1, value, _sp2 in rest:
    ...                 out.append(value)
    ...             return out
    ...
    ...     def start(self, parse_tree, result):
    ...         _sp1, value, _sp2, _eof = result
    ...         return value

    >>> transformer = ListTransformer()
    >>> transformer.transform(parse_tree)
    [1, 2, [3, [4]], []]

When a :py:class:`ParseTreeTransformer` subclass'
:py:meth:`.ParseTreeTransformer.transform` method is called, the parse tree is
traversed in a bottom-up fashion, and for each rule application encountered
attempts to call the method named after the rule. These methods are passed two
arguments: the :py:class:`.ParseTree` corresponding with the matched subtree
and the transformed result of transforming the children of the parse-tree at
this point. The method should return the transformed value.

When no matching method is found, the default implementation transforms the
parse tree into simple Python types automatically. For example, regular
expression matches are transformed into strings, ``?`` into a value or None and
concatenations, ``*`` and ``+`` into lists.


Grammars
========

The PEG grammar syntax accepted by this module is defined using its own syntax
below:

..
    NB: The 'include' below is relative to the /docs/source directory, an
    unfortunate wrinkle in Sphinx/RST...

.. include:: ../../peggie/meta_grammar.peg
    :literal:

The syntax is based on Ford's [PEG]_ grammars. In summary:

* ``name <- pattern`` defines a rule called ``name`` matching the provided
  pattern.
* ``rule_name`` is a pattern matching whatever pattern is defined for the
  provided rule.
* ``"foo"`` is a pattern matching the string ``foo``.
* ``[a-z]`` is a pattern matching the characters from ``a`` to ``z``.
* ``.`` is a pattern which matches any single character.
* ``pattern+`` is a pattern matching one or more instances of the provided
  pattern.
* ``pattern*`` is a pattern matching zero or more instances of the provided
  pattern.
* ``pattern?`` is a pattern matching zero or one instances of the provided
  pattern.
* ``pattern_a pattern_b`` is a pattern matching a string matched by
  ``pattern_a`` followed by a string matched by ``pattern_b``.
* ``patern_a / pattern_b`` is a pattern which matches ``pattern_a`` if that
  mattern matches, or ``pattern_b`` if ``pattern_a`` does not match and
  ``pattern_b`` does. (Note that there is no ambiguity if both ``pattern_a``
  and ``pattern_b`` match).
* ``!pattern`` is a pattern which matches, without consuming any input, only
  when the provided pattern does not match.
* ``&pattern`` is a pattern which matches, without consuming any input, only
  when the provided pattern matches.

Comments are started with a hash (``#``) and run to the end of the line.


Regular expressions
-------------------

In addition, Python-style regular expressions may also be included in ``r``
prefixed strings, eg. ``r"foo+"``. This is partly a convenience to enable
slightly more concise descriptions of some patterns and also a potential
performance boost since longer patterns may be matched more quickly by Python's
:py:mod:`re` matcher.

.. note::

    The :py:data:`re.DOTALL` flag is implicitly set for all patterns meaning
    ``.`` in a regular expression will also match newlines, which is not the
    default Python behaviour.

Indentation sensitivity
-----------------------

This parser also extends Ford's grammars to support indentation-sensitive
languages using an extension loosely based on by that of Adams and Ağacan
[PEGIndent]_.


Indentation prefixes
````````````````````

By default, all patterns are indentation insensitive but may be made
indentation sensitive by prefixing them with ``@=``, ``@>=`` or ``@>``. These
add the additional requirement that the pattern must be on a line whose
indentation is equal, greater-or-equal or greater (respectively) than the
current reference indentation.

Within a concatenation, the indentation level at the first character to be
matched is used as the reference indentation. For example, the grammar::

    start       <- space foo_bar_baz end_of_file

    foo_bar_baz <- "foo"
                   newline
                   @="bar"
                   newline
                   @="baz"
                   newline

    space       <- r"[ \t]*"
    newline     <- r"[ \t]*\n[ \t]*"
    end_of_file <- !.

Will match any string containing ``foo``, ``bar`` and ``baz`` on separate
lines, but all indented to the same level.

.. note::

    An indentation constraint may be applied to a pattern which matches any
    part of the line, not just the start. However, the indentation level for
    the line as a whole is always checked, not the number of characters into
    the line that particular pattern appears. For example, the following would
    match exactly the same strings as the example above::

        foo_bar_baz <- "foo"
                      newline
                      @="bar"
                      newline
                      "baz"
                      @=newline  # NB: @= is applied to the newline here!

Replacing ``@=`` with ``@>=`` will match ``foo``, ``bar`` and ``baz`` so long
as ``bar`` and ``baz`` are indented at least as much as ``foo``, but may be
indented more so. However, ``bar`` and ``baz`` may be intended arbitrarily with
respect to eachother because indentation is only checked with respect to the
first character in the concatenation. So, for example::

    foo
        bar
            baz

And::

    foo
    bar
        baz

And::

    foo
        bar
    baz

Will all match but the following will *not*::

            foo
       bar
            baz

Simillarly, replacing ``@=`` with ``@>`` requires that ``bar`` and ``baz`` have
greater indentation than ``foo`` but, again, the relative indentation of
``bar`` and ``baz`` to each other is not constrained.

.. note::

    We could have placed ``@=`` in front of the first pattern in
    ``foo_bar_baz`` but since the first pattern has, by definition, the same
    indentation as the start of the cancatnation, this has no effect.

    If we had placed ``@>`` in front of the first pattern, the pattern will
    never match because we are requiring that this pattern has a greater
    indentation than itself.

.. note::

    Any pattern without an indentation prefix has no indentation constraint.
    Where it is useful for reasons of readability, however, this can be made
    explicit with the ``@*`` prefix.

When used with ``*`` or ``+``, the indentation constraints apply with respect
to the indentation of the start of the first match. For example the grammar::

    start       <- space foos end_of_file

    foos <- @=("foo" newline)+

    space       <- r"[ \t]*"
    newline     <- r"[ \t]*\n[ \t]*"
    end_of_file <- !.

Matches any number of lines containing ``foo`` indented to the same level.
Likewise replacing ``@=`` with ``@>=`` will match any number of lines
containing ``foo`` where all lines must be indented at least as much as the
first (but with no other relative constraint on indentation).

When indentation prefixes are used in other contexts (for example in ``@= "foo"
?``), they have no effect.

.. note::

    The location of the perentheses used in this example are important. For
    example::

        foos <- (@="foo" newline)+

    Will match any number of lines containing foo with *any* indentation
    because the indentation prefix applies to ``"foo"`` within the
    concatenation ``(@="foo" newline)``.


Indented block syntax example
`````````````````````````````

For a simple example of indentation prefixes being used to specify a
python-style intended block syntax, see the following::

    start          <- space stmt space end_of_file

    stmt           <- call_stmt / block_stmt
    call_stmt      <- r"[a-z]+\(\)" newline
    block_stmt     <- "block:" newline
                      @>(
                        @=stmt+
                      )

    space          <- r"[ \t]*"
    newline        <- r"[ \t]*\n[ \t]*"
    end_of_file    <- !.

This grammar accepts strings of the form::

    block:
        foo()
        bar()
        block:
            baz()
        block:
            qux()
        quo()

Breaking down the ``block_stmt``:

* ``"block:" newline``: A block starts with ``block:`` and a newline...
* ``@>( ... )`` ...followed by something indented more than ``block:`` was...
* ``@=stmt+`` ...specifically one or more ``stmt``\ s, all indented to the same
  level as the first ``stmt`` matched.


Indentation prefixes and empty matches
``````````````````````````````````````

When an indentation requirement is applied to a pattern which can match the
empty string, the indentation level is still checked and the empty string will
not be matched if the indentation level at the current parse position is
incorrect. For example, in the grammar::

    start       <- space foo_bar_baz end_of_file

    foo_bar_baz <- "foo" newline
                   @="" "bar" newline
                   @="" "baz" newline

    space       <- r"[ \t]*"
    newline     <- r"[ \t]*\n[ \t]*"
    end_of_file <- !.

Here we placed the indentation restriction on ``""`` (which matches the empty
string), this empty string must be matched at the same indentation level as the
start of the concatenation. Consequently this grammar matches the same strings
as our earlier example (i.e. with ``foo``, ``bar`` and ``baz`` having the same
indentation).

There is, however, an exception to this rule. When a ``?`` or ``*`` pattern
within a concatenation fails to match the indentation requirement, the pattern
is deemed to have matched the empty string rather than failing to match. To see
why this special case is useful, consider the following grammar::

    start          <- space stmt space end_of_file

    stmt           <- call_stmt / try_catch_stmt
    call_stmt      <- r"[a-z]+\(\)" newline
    try_catch_stmt <- "try" stmt_block
                      @=("catch" stmt_block)?

    stmt_block     <- ":" newline
                      @>(
                        @=stmt+
                      )

    space          <- r"[ \t]*"
    newline        <- r"[ \t]*\n[ \t]*"
    end_of_file    <- !.

This grammar describes a language matching strings like::

    foo()

And::

    try:
        foo()
        bar()
        baz()

And::

    try:
        foo()
        bar()
    catch:
        baz()
        qux()

In this case, the 'catch' part of the ``try_catch_stmt`` rule uses ``@=`` to
ensure that 'catch' has the same indentation as the opening 'try'. However,
this also helps us disambiguate the following case::

    try:
        foo()
        try:
            bar()
    catch:
        baz()

Specifically, the 'catch' here belongs to the outer try/catch block, not the
inner one. In this instance while parsing the inner ``try_catch_stmt`` rule, we
fail to match the ``catch:`` line because it has the wrong indentation.
However, as a special case because this is enclosed in a ``?`` pattern, rather
than throwing a parse failure we act as if the ``?`` matched the empty string,
leaving the ``catch:`` to be parsed by the outer ``try_catch_stmt`` rule.

.. note::

    If, for some reason, you wished to override this special case behaviour
    (and always fail to parse on indentation failure, this can be forced by
    turning the ``?`` pattern into something else, for example a concatenation
    with the empty string like::

        try_catch_stmt <- "try" stmt_block
                          @=(("catch" stmt_block)? "")  # Will not match empty on
                                                        # indentation failure!


Well-formedness
---------------

Grammars must adhere to the following well-formedness rules:

* At least one rule (the start rule) must be defined.
* Left recursion, whether direct, indirect or hidden, is not allowed.
* The ``*`` and ``+`` repetition operators may not be used on patterns which
  match the empty string.
* All rules used in patterns must have exactly one definition.

If a grammar does not meet these criteria, a :py:exc:`.GrammarCompileError`
will be thrown during grammar compilation.


Unprocessed input
-----------------

In common with other PEG parsers, the parser will not throw an error if the
whole input is not parsed. To ensure the whole input is matched by the grammar,
use the ``!.`` idiom which matches only at the end of the input.


Error messages
==============

Upon parse failures a :py:exc:`.ParseError` exception will be thrown.

When cast
to a :py:class:`str`, these exceptions produce a descriptive parse failure. For
example, given the following grammar::

    >>> grammar = compile_grammar('''
    ...     summation   <- value space+ ("+" space+ summation)? end_of_file
    ...     value       <- r"[0-9]+"
    ...     space       <- [ \t]
    ...     end_of_file <- !.
    ... ''')

This matches simple strings such as ``123`` or ``1 + 2 + 3``. If a non-matching
string is provided, however, an error such as ``1 + 2 - 3`` the following will
be produced::

    >>> from peggie import ParseError

    >>> try:
    ...     Parser(grammar).parse("1 + 2 - 3")
    ... except ParseError as e:
    ...     print(str(e))
    At line 1 column 7:
        1 + 2 - 3
              ^
    Expected '\\+' or end_of_file or space

Notice that the error suggests rules (``end_of_file`` and ``space``) and
strings (``\\+``) which would have allowed parsing to continue.

.. note::

    All string literals in a grammar are internally converted to Python regular
    expressions and therefore are subject to :py:func:`re.escape`. As an
    unfortunate result, the strings shown in these error messages also contain
    extra escaping.

Likewise, if we try to parse an empty string::

    >>> try:
    ...     Parser(grammar).parse("")
    ... except ParseError as e:
    ...     print(str(e))
    At line 1 column 1:
    <BLANKLINE>
        ^
    Expected summation

Note that the error refers to the highest-level rule which applies, i.e.
``summation`` rather than ``value`` in this case. In complex grammars, this is
usually what you'd want.

These default messages simply use the bare rule names and regular expression
strings from the grammar to describe what is expected. This is not always the
most user-friendly option. As a result, alternative names for these elements
using the :py:attr:`.ParseError.expr_explanations` :py:class:`dict` attribute.
For example::

    >>> from peggie import RuleExpr, RegexExpr

    >>> expr_explanations = {
    ...     RuleExpr("end_of_file"): "<end of file>",
    ...     RuleExpr("space"): "<space>",
    ...     RuleExpr("value"): "<number>",
    ...     RuleExpr("summation"): "<number>",
    ...     RegexExpr.literal("+"): "'+'",
    ... }

    >>> try:
    ...     Parser(grammar).parse("1 + 2 - 3")
    ... except ParseError as e:
    ...     e.expr_explanations = expr_explanations
    ...     print(str(e))
    At line 1 column 7:
        1 + 2 - 3
              ^
    Expected '+' or <end of file> or <space>

    >>> try:
    ...     Parser(grammar).parse("")
    ... except ParseError as e:
    ...     e.expr_explanations = expr_explanations
    ...     print(str(e))
    At line 1 column 1:
    <BLANKLINE>
        ^
    Expected <number>

As a further refinement, we can filter out more unhelpful suggestions when
others are available. For instance in the first example above the suggestion of
``<space>``, while valid, is not especially useful in this case. We can
suppress such suggestions using the :py:attr:`.ParseError.last_resort_exprs`
:py:class:`set` attribute::

    >>> last_resort_exprs = {
    ...     RuleExpr("space"),
    ... }

    >>> try:
    ...     Parser(grammar).parse("1 + 2 - 3")
    ... except ParseError as e:
    ...     e.expr_explanations = expr_explanations
    ...     e.last_resort_exprs = last_resort_exprs
    ...     print(str(e))
    At line 1 column 7:
        1 + 2 - 3
              ^
    Expected '+' or <end of file>

If our suppressed expression is the only suggestion, however, it will still be
included. For example::

    >>> try:
    ...     Parser(grammar).parse("1 +2")
    ... except ParseError as e:
    ...     e.expr_explanations = expr_explanations
    ...     e.last_resort_exprs = last_resort_exprs
    ...     print(str(e))
    At line 1 column 4:
        1 +2
           ^
    Expected <space>


Parser API Reference
====================

The following sections provide reference documentation for the parser's API.


Parser
------

Parsing is performed by the :py:class:`.Parser` class:

.. autoclass:: Parser
    :members: parse

When a non-matching string is provided, a :py:exc:`.ParseError` will be thrown:

.. autoexception:: ParseError
    :members:
    :undoc-members:

Indentation mismatches are recorded in the :py:attr:`.ParseError.expectations`
attribute in :py:class:`.AbsoluteIndentation` objects:

.. autoclass:: AbsoluteIndentation
    :members:

If the :py:class:`Parser` is supplied with a :py:class:`.Grammar` which is not
well-formed, the following exceptions may be thrown at runtime during parsing:

.. autoexception:: GrammarError

.. autoexception:: RepeatedEmptyTermError
    :show-inheritance:

.. autoexception:: LeftRecursionError
    :show-inheritance:

.. autoexception:: UndefinedRuleError
    :show-inheritance:


Grammars
--------

Grammars are defined by a :py:class:`.Grammar` object:

.. autoclass:: Grammar
    :members:
    :exclude-members: is_well_formed

In the typical case, :py:class:`Grammar` compiled from a textual PEG grammar
description using :py:func:`.compile_grammar`:

.. autofunction:: compile_grammar

When a grammar is compiled, it is automatically checked for well-formedness and
the following exceptions are produced if the grammar fails these checks:

.. autoexception:: GrammarCompileError

.. autoexception:: RuleDefinedMultipleTimesError
    :show-inheritance:

.. autoexception:: GrammarNotWellFormedError
    :show-inheritance:

Grammars can also be constructed 'by hand' from :py:class:`.Expr` objects.
These objects represent expressions in a grammar (e.g. regular expressions or
concatenations of other expressions).

.. autoclass:: Expr

.. autoclass:: EmptyExpr

.. autoclass:: RegexExpr
    :members: literal

.. autoclass:: RuleExpr

.. autoclass:: AltExpr

.. autoclass:: ConcatExpr

.. autoclass:: MaybeExpr

.. autoclass:: StarExpr

.. autoclass:: PlusExpr

.. autoclass:: LookaheadExpr

.. autoclass:: PositiveLookaheadExpr

Indentation requirements are indicated using the following enumerated type:

.. autoclass:: RelativeIndentation
    :members: any, equal, greater_or_equal, greater
    :undoc-members:

When a :py:class:`Grammar` is constructed by hand it is not automatically
checked for well-formedness. This check can be performed manually using
:py:meth:`.Grammar.is_well_formed`.

.. automethod:: Grammar.is_well_formed

The well-formedness check result will come in the form of one of the following:

.. autoclass:: GrammarWellFormedness

.. autoclass:: WellFormed
    :show-inheritance:
    :members:
    :undoc-members:

.. autoclass:: UndefinedRule
    :show-inheritance:
    :members:
    :undoc-members:

.. autoclass:: LeftRecursion
    :show-inheritance:
    :members:
    :undoc-members:

.. autoclass:: RepeatedEmptyTerm
    :show-inheritance:
    :members:
    :undoc-members:


Parse trees
-----------

The result of parsing a string is a :py:class:`ParseTree`. This tree's
structure mimics that of the grammar and consists of a hierarchy of the
following classes:

.. autoclass:: ParseTree
    :members:

.. autoclass:: Empty
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children

.. autoclass:: Regex
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children

.. autoclass:: Rule
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children

.. autoclass:: Alt
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children

.. autoclass:: Concat
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children

.. autoclass:: Maybe
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children

.. autoclass:: Star
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children

.. autoclass:: Plus
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children

.. autoclass:: Lookahead
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children

.. autoclass:: PositiveLookahead
    :show-inheritance:
    :members:
    :undoc-members:
    :exclude-members: iter_children


Transformers
------------

To assist in the transformation of a parse tree into a useful data structure,
the :py:class:`.ParseTreeTransformer` base class is provided:

.. autoclass:: ParseTreeTransformer
    :members:
    :private-members:

"""


from peggie.version import __version__

from peggie.parser import *
from peggie.transformer import *
from peggie.grammar_compiler import *

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
