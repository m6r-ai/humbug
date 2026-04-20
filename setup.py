"""
Build script for C extension module.

Usage:
    python setup.py build_ext --inplace

Compiles the Menai C value types and C VM into a single shared library.

This file is intentionally separate from pyproject.toml/hatchling, which
handles the main application packaging.  The C extension build is a
development and release step, not part of the pip-installable package metadata.
"""

import os

import sys

from setuptools import Extension, setup

_MENAI_SRC = os.path.join("src", "menai")

extensions = [
    Extension(
        name="menai.menai_vm_c",
        sources=[
            "src/menai/menai_vm_c.c",
            "src/menai/menai_vm_string.c",
            "src/menai/menai_vm_hashtable.c",
            "src/menai/menai_vm_dict.c",
            "src/menai/menai_vm_function.c",
            "src/menai/menai_vm_list.c",
            "src/menai/menai_vm_set.c",
            "src/menai/menai_vm_struct.c",
            "src/menai/menai_vm_symbol.c",
            "src/menai/menai_vm_complex.c",
            "src/menai/menai_vm_float.c",
            "src/menai/menai_vm_integer.c",
            "src/menai/menai_vm_boolean.c",
            "src/menai/menai_vm_none.c",
            "src/menai/menai_vm_value.c",
        ],
        include_dirs=[_MENAI_SRC],
        extra_compile_args=(
            ["/O2", "/std:c11"] if sys.platform == "win32"
            else ["-O2", "-std=c11"]
        ),
    ),
]

setup(
    name="menai-fast",
    ext_modules=extensions,
    package_dir={"": "src"},
)
