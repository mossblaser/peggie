# Configuration file for the Sphinx documentation builder.

import os
import sys


# -- Path setup --------------------------------------------------------------

# To find the peggie module
sys.path.insert(0, os.path.abspath("../.."))

# -- Project information -----------------------------------------------------

project = "Peggie"
copyright = "2020, Jonathan Heathcote"
author = "Jonathan Heathcote"

from peggie import __version__ as version

release = version

# -- General configuration ---------------------------------------------------

master_doc = "index"

extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.autosummary",
    "sphinx.ext.intersphinx",
    "sphinx.ext.doctest",
    "sphinx.ext.mathjax",
    "numpydoc",
]

# -- Options for numpydoc/autodoc --------------------------------------------

# Fixes autosummary errors
numpydoc_show_class_members = False

autodoc_member_order = "bysource"

add_module_names = False

autodoc_typehints = "none"

# -- Options for intersphinx -------------------------------------------------

intersphinx_mapping = {
    "python": ("http://docs.python.org/3", None),
}


# -- Options for HTML output -------------------------------------------------

html_theme = "nature"

html_static_path = ["_static"]


# -- Options for PDF output --------------------------------------------------

# A small document so no need for chapters etc.
latex_theme = "howto"

# Show page numbers in references
latex_show_pagerefs = True

# Show hyperlink URLs in footnotes
latex_show_urls = "footnote"

# Divide the document into chapters, then sections
latex_toplevel_sectioning = "section"

# Don't include a module index (the main index should be sufficient)
latex_domain_indices = False

latex_elements = {
    "papersize": "a4paper",
    # Make index entries smaller since some are quite long
    "printindex": r"\footnotesize\raggedright\printindex",
    # Override ToC depth to include sections
    "preamble": r"\setcounter{tocdepth}{1}",
}
