"""
Unified builtin function registry for AIFPL.

This module provides builtin function metadata and first-class function objects
for all builtins, used by the VM to populate the global environment.
"""

from typing import Dict, Optional, Tuple

from aifpl.aifpl_bytecode import BUILTIN_OPCODE_MAP, CodeObject, Instruction, Opcode
from aifpl.aifpl_value import AIFPLFunction


class AIFPLBuiltinRegistry:
    """
    Central registry for all builtin functions.

    Provides arity metadata (consumed by the semantic analyser) and
    AIFPLFunction objects (consumed by the VM to populate globals).
    Fixed-arity builtins are represented as bytecode stubs; variadic
    builtins whose function objects are supplied by the AIFPL prelude
    are skipped here so the prelude lambdas take effect instead.
    """

    # Authoritative arity table for all builtins.
    #
    # Each entry is (min_args, max_args) where max_args is None for truly
    # variadic functions (no upper bound).  Functions with fixed arity
    # have min_args == max_args.
    #
    # This is the single source of truth consumed by the semantic analyzer
    # for early arity checking, and by create_builtin_function_objects() for
    # building AIFPLFunction metadata.
    ARITY_TABLE: Dict[str, Tuple[int, Optional[int]]] = {
        'function?': (1, 1),
        'function-min-arity': (1, 1),
        'function-variadic?': (1, 1),
        'function-accepts?': (2, 2),
        'function=?': (2, 2),
        'function!=?': (2, 2),
        'symbol?': (1, 1),
        'symbol=?': (2, 2),
        'symbol!=?': (2, 2),
        'symbol->string': (1, 1),
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
        'integer-': (1, None),
        'integer*': (0, None),
        'integer/': (2, None),
        'integer%': (2, 2),
        'integer-neg': (1, 1),
        'integer-expt': (2, 2),
        'integer-abs': (1, 1),
        'bit-or': (2, None),
        'bit-and': (2, None),
        'bit-xor': (2, None),
        'bit-not': (1, 1),
        'bit-shift-left': (2, 2),
        'bit-shift-right': (2, 2),
        'integer-min': (1, None),
        'integer-max': (1, None),
        'integer->string': (1, 1),
        'bin': (1, 1),
        'hex': (1, 1),
        'oct': (1, 1),
        'integer->float': (1, 1),
        'float?': (1, 1),
        'float=?': (2, None),
        'float!=?': (2, None),
        'float<?': (2, None),
        'float>?': (2, None),
        'float<=?': (2, None),
        'float>=?': (2, None),
        'float+': (0, None),
        'float-': (1, None),
        'float*': (0, None),
        'float/': (1, None),
        'float//': (2, 2),
        'float%': (2, 2),
        'float-neg': (1, 1),
        'float-expt': (2, None),
        'float-sin': (1, 1),
        'float-cos': (1, 1),
        'float-tan': (1, 1),
        'float-log': (1, 1),
        'float-log10': (1, 1),
        'float-exp': (1, 1),
        'float-sqrt': (1, 1),
        'float-abs': (1, 1),
        'float->integer': (1, 1),
        'float->string': (1, 1),
        'float-floor': (1, 1),
        'float-ceil': (1, 1),
        'float-round': (1, 1),
        'float-min': (1, None),
        'float-max': (1, None),
        'complex': (2, 2),
        'complex?': (1, 1),
        'complex=?': (2, None),
        'complex!=?': (2, None),
        'complex+': (0, None),
        'complex-': (1, None),
        'complex*': (0, None),
        'complex/': (1, None),
        'complex-neg': (1, 1),
        'complex-real': (1, 1),
        'complex-imag': (1, 1),
        'complex-expt': (2, None),
        'complex-sin': (1, 1),
        'complex-cos': (1, 1),
        'complex-tan': (1, 1),
        'complex-log': (1, 1),
        'complex-log10': (1, 1),
        'complex-exp': (1, 1),
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
        'string-replace': (3, 3),
        'string-contains?': (2, 2),
        'string-prefix?': (2, 2),
        'string-suffix?': (2, 2),
        'string-ref': (2, 2),
        'string-slice': (2, 3),
        'string->number': (1, 1),
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
        'list-position': (2, 2),
        'list-slice': (2, 3),
        'list-remove': (2, 2),
        'list-ref': (2, 2),
        'list->string': (1, 2),
        'alist?': (1, 1),
        'alist=?': (2, None),
        'alist!=?': (2, None),
        'alist': (0, None),
        'alist-get': (2, 3),
        'alist-set': (3, 3),
        'alist-remove': (2, 2),
        'alist-has?': (2, 2),
        'alist-keys': (1, 1),
        'alist-values': (1, 1),
        'alist-merge': (2, 2),
        'alist-length': (1, 1),
        'range': (2, 3),
    }

    def create_builtin_function_objects(self) -> Dict[str, AIFPLFunction]:
        """
        Create AIFPLFunction objects for all builtins.

        This is used to populate the global environment with first-class function objects.

        Fixed-arity builtins that appear in BUILTIN_OPCODE_MAP are represented as
        AIFPLFunction objects with a bytecode stub — a minimal CodeObject whose body
        is the single opcode followed by RETURN.  The stub has no locals and no
        constants; it relies entirely on the arguments already being on the stack
        when CALL enters the frame.

        Names provided by the AIFPL prelude are skipped entirely — the prelude's
        compiled lambda objects take effect in the global environment instead.

        Returns:
            Dictionary mapping function names to AIFPLFunction objects
        """
        # Names provided by the AIFPL prelude — the registry skips these so the
        # prelude's compiled function objects take effect in the global environment.
        # These names are still kept in ARITY_TABLE so that 2-arg calls inside
        # their own prelude stub bodies resolve to opcodes correctly via
        # BUILTIN_OPCODE_MAP in the codegen.
        prelude_names = {
            'boolean=?',
            'boolean!=?',
            'integer=?',
            'integer!=?',
            'integer<?',
            'integer>?',
            'integer<=?',
            'integer>=?',
            'integer+',
            'integer-',
            'integer*',
            'integer/',
            'bit-or',
            'bit-and',
            'bit-xor',
            'integer-min',
            'integer-max',
            'float=?',
            'float!=?',
            'float<?',
            'float>?',
            'float<=?',
            'float>=?',
            'float+',
            'float-',
            'float*',
            'float/',
            'float-expt',
            'float-min',
            'float-max',
            'complex=?',
            'complex!=?',
            'complex+',
            'complex-',
            'complex*',
            'complex/',
            'complex-expt',
            'string=?',
            'string!=?',
            'string<?',
            'string>?',
            'string<=?',
            'string>=?',
            'string-concat',
            'string-slice',
            'string->list',
            'list',
            'list=?',
            'list!=?',
            'list-concat',
            'list-slice',
            'list->string',
            'alist',
            'alist=?',
            'alist!=?',
            'alist-get',
            'range',
        }

        builtins = {}
        for name, (min_args, max_args) in self.ARITY_TABLE.items():
            if name in prelude_names:
                # Prelude supplies the function object for this name
                continue

            is_fixed_arity = (max_args is not None and min_args == max_args)

            assert name in BUILTIN_OPCODE_MAP, f"Builtin '{name}' is missing from BUILTIN_OPCODE_MAP"
            assert is_fixed_arity, f"Builtin '{name}' is variadic but missing from prelude_names set"

            # Truly fixed-arity builtin: generate a bytecode stub
            opcode, arity = BUILTIN_OPCODE_MAP[name]
            instructions = [
                Instruction(opcode),
                Instruction(Opcode.RETURN),
            ]
            stub = CodeObject(
                instructions=instructions,
                constants=[],
                names=[],
                code_objects=[],
                param_count=arity,
                local_count=arity,
                name=f'<builtin:{name}>',
            )
            parameters = tuple(f'arg{i}' for i in range(arity))
            builtins[name] = AIFPLFunction(
                parameters=parameters,
                name=name,
                bytecode=stub,
                is_variadic=False
            )

        return builtins
