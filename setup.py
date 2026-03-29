"""
Build script for C extension modules.

Usage:
    python setup.py build_ext --inplace

This compiles the C value type and C VM extensions and places the resulting
.so files directly into src/menai/ alongside the pure-Python sources.
PyInstaller will pick them up automatically from there.

This file is intentionally separate from pyproject.toml / hatchling, which
handles the main application packaging.  The C extension build is a
development and release step, not part of the pip-installable package metadata.
"""

from setuptools import Extension, setup

import os

# src/menai is both the package root and where the shared headers live.
_MENAI_SRC = os.path.join("src", "menai")

extensions = [
    Extension(
        name="menai.menai_value_c",
        sources=["src/menai/menai_value_c.c"],
        include_dirs=[_MENAI_SRC],
        extra_compile_args=["-O2", "-std=c11"],
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
    ext_modules=extensions,
    package_dir={"": "src"},
)
