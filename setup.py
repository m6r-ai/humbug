"""
Build script for Cython extension modules.

Usage:
    python setup.py build_ext --inplace

This compiles the Cython VM extensions and places the resulting .so files
directly into src/menai/ alongside the pure-Python sources.  PyInstaller will
pick them up automatically from there.

This file is intentionally separate from pyproject.toml / hatchling, which
handles the main application packaging.  The Cython build is a development
and release step, not part of the pip-installable package metadata.
"""

from setuptools import Extension, setup
from Cython.Build import cythonize

import os
import sys

# src/menai is both the package root and where the shim header lives.
_MENAI_SRC = os.path.join("src", "menai")

extensions = [
    Extension(
        name="menai.menai_value_fast",
        sources=["src/menai/menai_value_fast.pyx"],
        extra_compile_args=["-O2"],
    ),
    Extension(
        name="menai.menai_vm",
        sources=["src/menai/menai_vm.pyx"],
        extra_compile_args=["-O2"],
    ),
    Extension(
        name="menai.menai_vm_c",
        sources=["src/menai/menai_vm_c.c"],
        include_dirs=[_MENAI_SRC],
        extra_compile_args=["-O3", "-std=c11"],
        py_limited_api=False,
    ),
]

setup(
    name="menai-fast",
    ext_modules=cythonize(
        extensions,
        compiler_directives={
            "language_level": "3",
            "boundscheck": False,
            "wraparound": False,
        },
        build_dir="build/cython",
    ),
    package_dir={"": "src"},
)
