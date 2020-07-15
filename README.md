Peggie
======

Peggie is a simple [PEG](https://bford.info/pub/lang/peg.pdf) parser written in
Python, with extensions to add native support for indentation sensitivity.


Installation
------------

Peggie can be installed from PIP using:

    $ pip install peggie

Peggie requires Python 3.7 or later, but is known to run under Python 3.6 if
the `dataclasses` backport package is installed:

    $ # For Python 3.6 only.
    $ pip install dataclasses


Documentation
-------------

The [Peggie documentation can be found on
ReadTheDocs](http://peggie.rtfd.org/).


Quick example
-------------

*The following example is taken from the Peggy documentation. Refer to the
documentation for a more thorough introduction.*

Grammars may be specified using (a slightly extended form) of the PEG syntax.
For example, this grammar can parse nested lists of numbers like `[1, 2, [3,
[4]], []]`:

    >>> grammar_source = r'''
    ...     start          <- space value space end_of_file
    ...     value          <- list_of_values / number
    ...     list_of_values <- "[" space (value space ("," space value space)*)? "]"
    ...     number         <- r"[0-9]+"
    ...     space          <- r"\s*"
    ...     end_of_file    <- !.
    ... '''

The grammar is then compiled and a parser constructed:

    >>> from peggie import compile_grammar, Parser
    >>> grammar = compile_grammar(grammar_source)
    >>> parser = Parser(grammar)

At this point the parser may be used to produce a parse-tree from an input
string:

    >>> parse_tree = parser.parse("[1, 2, [3, [4]], []]")

Parse trees can be transformed into a more useful form using the supplied
`ParseTreeTransformer` facility:

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


Development
-----------

Testing dependencies may be installed using:

    $ pip install -r requirements-test.txt

Tests are then run using:

    $ py.test

Though typechecking (using [MyPy](https://mypy.readthedocs.io/)) is performed
as part of the test suite, it may be run manually using:

    $ ./run_mypy.sh

Documentation build dependencies can be obtained using:

    $ pip install -r requirements-docs.txt

And the documentation built using:

    $ make -C docs html latexpdf


Disclaimers
-----------

*Given enough time, any project will eventually grow a parser implementation...*

Peggie is pretty minimal and fairly slow and essentially written for sport.  It
was written partly as an excercise to learn about PEG parsing and also play
with MyPy. Nevertheless, while it should be fairly stable, if you're building
something important you should probably look to one of the many better
supported options.
