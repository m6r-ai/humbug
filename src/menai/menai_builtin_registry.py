"""
Unified builtin function registry for Menai.

This module provides builtin function metadata and first-class function objects
for all builtins, used by the VM to populate the global environment.
"""

from typing import Dict, Tuple

from menai.menai_bytecode import BUILTIN_OPCODE_MAP


class MenaiBuiltinRegistry:
    """
    Central registry for all builtin functions.

    Provides arity metadata (consumed by the semantic analyser) and
    MenaiFunction objects (consumed by the VM to populate globals).
    Fixed-arity builtins are represented as bytecode stubs; variadic
    builtins whose function objects are supplied by the Menai prelude
    are skipped here so the prelude lambdas take effect instead.
    """

    # Arity table for all builtin functions.
    #
    # Each entry is (min_args, max_args) where max_args is None for truly
    # variadic functions (no upper bound).  Functions with fixed arity
    # have min_args == max_args.
    #
    # This table covers ONLY builtins that are backed by a VM opcode in
    # BUILTIN_OPCODE_MAP.  Pure-Menai prelude functions (map, filter, fold,
    # zip, unzip, find, any?, all?, etc.) are NOT in this table and must NOT
    # be added — the registry asserts every entry has a BUILTIN_OPCODE_MAP
    # entry, so adding a prelude-only name will cause an assertion failure.
    BUILTIN_FUNCTION_ARITIES: Dict[str, Tuple[int, int | None]] = {
        'function?': (1, 1),
        'function=?': (2, 2),
        'function!=?': (2, 2),
        'function-min-arity': (1, 1),
        'function-variadic?': (1, 1),
        'function-accepts?': (2, 2),
        'symbol?': (1, 1),
        'symbol=?': (2, 2),
        'symbol!=?': (2, 2),
        'symbol->string': (1, 1),
        'none?': (1, 1),
        'boolean?': (1, 1),
        'boolean=?': (2, None),
        'boolean!=?': (2, None),
        'boolean-not': (1, 1),
        'integer?': (1, 1),
        'integer=?': (2, None),
        'integer!=?': (2, None),
        'integer<?': (2, None),
        'integer>?': (2, None),
        'integer<=?': (2, None),
        'integer>=?': (2, None),
        'integer+': (0, None),
        'integer-': (2, None),
        'integer*': (0, None),
        'integer/': (2, None),
        'integer%': (2, 2),
        'integer-neg': (1, 1),
        'integer-expn': (2, 2),
        'integer-abs': (1, 1),
        'integer-bit-or': (2, None),
        'integer-bit-and': (2, None),
        'integer-bit-xor': (2, None),
        'integer-bit-not': (1, 1),
        'integer-bit-shift-left': (2, 2),
        'integer-bit-shift-right': (2, 2),
        'integer-min': (1, None),
        'integer-max': (1, None),
        'integer->complex': (1, 2),
        'integer->string': (1, 2),
        'integer->float': (1, 1),
        'float?': (1, 1),
        'float=?': (2, None),
        'float!=?': (2, None),
        'float<?': (2, None),
        'float>?': (2, None),
        'float<=?': (2, None),
        'float>=?': (2, None),
        'float+': (0, None),
        'float-': (2, None),
        'float*': (0, None),
        'float/': (2, None),
        'float//': (2, 2),
        'float%': (2, 2),
        'float-neg': (1, 1),
        'float-exp': (1, 1),
        'float-expn': (2, None),
        'float-log': (1, 1),
        'float-log10': (1, 1),
        'float-log2': (1, 1),
        'float-logn': (2, 2),
        'float-sin': (1, 1),
        'float-cos': (1, 1),
        'float-tan': (1, 1),
        'float-sqrt': (1, 1),
        'float-abs': (1, 1),
        'float->integer': (1, 1),
        'float->complex': (1, 2),
        'float->string': (1, 1),
        'float-floor': (1, 1),
        'float-ceil': (1, 1),
        'float-round': (1, 1),
        'float-min': (1, None),
        'float-max': (1, None),
        'complex?': (1, 1),
        'complex=?': (2, None),
        'complex!=?': (2, None),
        'complex+': (0, None),
        'complex-': (2, None),
        'complex*': (0, None),
        'complex/': (2, None),
        'complex-neg': (1, 1),
        'complex-real': (1, 1),
        'complex-imag': (1, 1),
        'complex-exp': (1, 1),
        'complex-expn': (2, None),
        'complex-log': (1, 1),
        'complex-log10': (1, 1),
        'complex-logn': (2, 2),
        'complex-sin': (1, 1),
        'complex-cos': (1, 1),
        'complex-tan': (1, 1),
        'complex-sqrt': (1, 1),
        'complex-abs': (1, 1),
        'complex->string': (1, 1),
        'string?': (1, 1),
        'string=?': (2, None),
        'string!=?': (2, None),
        'string<?': (2, None),
        'string>?': (2, None),
        'string<=?': (2, None),
        'string>=?': (2, None),
        'string->list': (1, 2),
        'string-concat': (0, None),
        'string-length': (1, 1),
        'string-upcase': (1, 1),
        'string-downcase': (1, 1),
        'string-trim': (1, 1),
        'string-trim-left': (1, 1),
        'string-trim-right': (1, 1),
        'string-replace': (3, 3),
        'string-index': (2, 2),
        'string-prefix?': (2, 2),
        'string-suffix?': (2, 2),
        'string-ref': (2, 2),
        'string-slice': (2, 3),
        'string->number': (1, 1),
        'string->integer': (1, 2),
        'string->integer-codepoint': (1, 1),
        'list': (0, None),
        'list?': (1, 1),
        'list=?': (2, None),
        'list!=?': (2, None),
        'list-prepend': (2, 2),
        'list-append': (2, 2),
        'list-concat': (0, None),
        'list-reverse': (1, 1),
        'list-first': (1, 1),
        'list-rest': (1, 1),
        'list-length': (1, 1),
        'list-last': (1, 1),
        'list-member?': (2, 2),
        'list-null?': (1, 1),
        'list-index': (2, 2),
        'list-slice': (2, 3),
        'list-remove': (2, 2),
        'list-ref': (2, 2),
        'list->string': (1, 2),
        'dict?': (1, 1),
        'dict=?': (2, None),
        'dict!=?': (2, None),
        'dict-get': (2, 3),
        'dict-set': (3, 3),
        'dict-remove': (2, 2),
        'dict-has?': (2, 2),
        'dict-keys': (1, 1),
        'dict-values': (1, 1),
        'dict-merge': (2, 2),
        'dict-length': (1, 1),
        'range': (2, 3),
    }

    @staticmethod
    def get_function_arity(name: str) -> Tuple[int, int | None] | None:
        """Return (min_args, max_args) for a known builtin function, or None if unknown.

        max_args is None for truly variadic functions (no upper bound).
        Used by the semantic analyser for call-site arity validation.
        """
        return MenaiBuiltinRegistry.BUILTIN_FUNCTION_ARITIES.get(name)

    @staticmethod
    def is_primitive_name(name: str) -> bool:
        """Return True if name is a valid primitive function name.

        A primitive name has a direct $-prefixed form that the desugarer and
        semantic analyser can use.  The $ prefix is the source-level syntax
        for bypassing the variadic wrapper and calling the primitive directly.
        """
        return name in BUILTIN_OPCODE_MAP

    @staticmethod
    def get_primitive_arity(name: str) -> int | None:
        """Return the exact argument count of the primitive form of this builtin, or None.

        Returns None if this name has no primitive form (i.e. it is a pure-Menai
        stdlib function with no direct primitive backing).
        Used by the desugarer to decide whether (f a b) can be rewritten to ($f a b).
        """
        entry = BUILTIN_OPCODE_MAP.get(name)
        if entry is None:
            return None

        _, arity = entry
        return arity
