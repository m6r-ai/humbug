#!/usr/bin/env python3
"""
Probe devirtualization behaviour on devirt_example.menai.

Prints the IR immediately after lambda lifting so we can see exactly
what structure the devirtualizer receives.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from menai.menai_compiler import MenaiCompiler
from menai.menai_ir_devirtualizer import MenaiIRDevirtualizer
from menai.menai_ir import (
    MenaiIRLet, MenaiIRLetrec, MenaiIRLambda, MenaiIRCall, MenaiIRReturn,
    MenaiIRVariable, MenaiIRConstant, MenaiIRIf, MenaiIRExpr
)


def print_ir(ir: MenaiIRExpr, indent: int = 0) -> None:
    prefix = "  " * indent
    if isinstance(ir, MenaiIRLet):
        print(f"{prefix}LET:")
        for name, val, *_ in ir.bindings:
            print(f"{prefix}  ({name} =")
            print_ir(val, indent + 2)
            print(f"{prefix}  )")
        print(f"{prefix}  BODY:")
        print_ir(ir.body_plan, indent + 2)

    elif isinstance(ir, MenaiIRLetrec):
        print(f"{prefix}LETREC:")
        for name, val, *_ in ir.bindings:
            print(f"{prefix}  ({name} =")
            print_ir(val, indent + 2)
            print(f"{prefix}  )")
        print(f"{prefix}  BODY:")
        print_ir(ir.body_plan, indent + 2)

    elif isinstance(ir, MenaiIRLambda):
        wrapper = " [WRAPPER]" if ir.is_wrapper else ""
        sfv = f" sfv={ir.sibling_free_vars}" if ir.sibling_free_vars else ""
        ofv = f" ofv={ir.outer_free_vars}" if ir.outer_free_vars else ""
        print(f"{prefix}LAMBDA{wrapper} {ir.binding_name} params={ir.params} pc={ir.param_count}{sfv}{ofv}")
        print(f"{prefix}  BODY:")
        print_ir(ir.body_plan, indent + 2)

    elif isinstance(ir, MenaiIRReturn):
        print(f"{prefix}RETURN:")
        print_ir(ir.value_plan, indent + 1)

    elif isinstance(ir, MenaiIRCall):
        tc = " TAIL" if ir.is_tail_call else ""
        print(f"{prefix}CALL{tc}:")
        print(f"{prefix}  func:")
        print_ir(ir.func_plan, indent + 2)
        for i, a in enumerate(ir.arg_plans):
            print(f"{prefix}  arg[{i}]:")
            print_ir(a, indent + 2)

    elif isinstance(ir, MenaiIRVariable):
        print(f"{prefix}VAR {ir.var_type}:{ir.name}")

    elif isinstance(ir, MenaiIRConstant):
        print(f"{prefix}CONST {ir.value}")

    elif isinstance(ir, MenaiIRIf):
        print(f"{prefix}IF:")
        print_ir(ir.condition_plan, indent + 1)
        print(f"{prefix}  THEN:")
        print_ir(ir.then_plan, indent + 2)
        print(f"{prefix}  ELSE:")
        print_ir(ir.else_plan, indent + 2)

    else:
        print(f"{prefix}{type(ir).__name__}")


source_path = Path(__file__).parent / "devirt_example.menai"
source = source_path.read_text()

# Replicate the compiler pipeline up to just after lambda lifting
compiler = MenaiCompiler(optimize=True)
resolved_ast = compiler.compile_to_resolved_ast(source, "devirt_example.menai")
desugared_ast = compiler.desugarer.desugar(resolved_ast)
for ast_pass in compiler.ast_passes:
    desugared_ast = ast_pass.optimize(desugared_ast)
_free_var_info = compiler.free_var_analyzer.analyze(desugared_ast)
ir = compiler.ir_builder.build(desugared_ast)
ir_lifted = compiler.lambda_lifter.lift(ir)

print("=== IR after lambda lifting (before devirtualization) ===")
print()
print_ir(ir_lifted)
