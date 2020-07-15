import os

from setuptools import setup, find_packages

version_file = os.path.join(os.path.dirname(__file__), "peggie", "version.py",)
with open(version_file, "r") as f:
    exec(f.read())

setup(
    name="peggie",
    version=__version__,  # noqa: F821
    packages=find_packages(),
    include_package_data=True,
    url="https://github.com/mossblaser/peggie",
    author="BBC R&D",
    description="A simple PEG parser with native indentation sensitivity.",
    license="GPLv2",
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: GNU General Public License v2 (GPLv2)",
        "Operating System :: POSIX :: Linux",
        "Operating System :: Microsoft :: Windows",
        "Operating System :: MacOS",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
    ],
    keywords="PEG parser",
    entry_points={"console_scripts": []},
)
