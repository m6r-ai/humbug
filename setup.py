"""
Build script for C extension module.

Usage:
    python setup.py build_ext --inplace

Compiles the Menai C value types and C VM into a single shared library.
menai.menai_value_c is imported from menai_vm_c.so at runtime.

This file is intentionally separate from pyproject.toml / hatchling, which
handles the main application packaging.  The C extension build is a
development and release step, not part of the pip-installable package metadata.
"""

import os

from setuptools import Extension, setup

_MENAI_SRC = os.path.join("src", "menai")

extensions = [
    Extension(
        name="menai.menai_vm_c",
        sources=[
            "src/menai/menai_value_c.c",
            "src/menai/menai_vm_c.c",
            "src/menai/menai_vm_string.c",
        ],
        include_dirs=[_MENAI_SRC],
        extra_compile_args=["-O2", "-std=c11"],
    ),
]

setup(
    name="menai-fast",
    ext_modules=extensions,
    package_dir={"": "src"},
)
