#!/usr/bin/env python3
"""
AIFPL Disassembler - Compile and disassemble AIFPL modules with detailed annotations.

This tool compiles AIFPL source files and generates annotated bytecode disassembly
showing:
- Constants table
- Variable assignments and function names
- Annotated instructions with what they do
- Source line numbers for each function
- Nested function hierarchy

Usage:
    python aifpl_disassemble.py <file.aifpl>
    python aifpl_disassemble.py <file.aifpl> --output disasm.txt
    python aifpl_disassemble.py <file.aifpl> --trace  # Also show function call trace
"""

import argparse
from pathlib import Path
import sys
import traceback
from typing import List, Dict

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from aifpl import AIFPL
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_value import AIFPLValue
from aifpl.aifpl_bytecode import Opcode, CodeObject, Instruction
from aifpl.aifpl_builtins import AIFPLBuiltinRegistry


def format_constant(const: object) -> str:
    """Format a constant for display."""
    if isinstance(const, str):
        if len(const) > 50:
            return f'"{const[:47]}..."'

        return f'"{const}"'

    if isinstance(const, AIFPLValue):
        val_str = str(const)
        if len(val_str) > 50:
            return f'{val_str[:47]}...'

        return val_str

    return str(const)


def annotate_instruction(instr: Instruction, code: CodeObject, builtin_names: List[str]) -> str:
    """Add annotation to instruction showing what it does."""
    opcode = instr.opcode
    arg1 = instr.arg1
    arg2 = instr.arg2

    annotation = ""

    if opcode == Opcode.LOAD_CONST:
        if arg1 < len(code.constants):
            const = code.constants[arg1]
            const_str = format_constant(const)
            if len(const_str) > 40:
                const_str = const_str[:37] + "..."
            annotation = f"  ; Load constant: {const_str}"

    elif opcode == Opcode.LOAD_VAR:
        annotation = f"  ; Load var[{arg2}]"

    elif opcode == Opcode.STORE_VAR:
        annotation = f"  ; Store to var[{arg2}]"

    elif opcode == Opcode.MAKE_CLOSURE:
        if arg1 < len(code.code_objects):
            nested = code.code_objects[arg1]
            name = nested.name or f"<lambda-{arg1}>"
            line_info = f" at line {nested.source_line}" if (nested.source_line and nested.source_line > 0) else ""
            capture_word = "capture" if arg2 == 1 else "captures"
            annotation = f"  ; Create closure for {name}{line_info} with {arg2} {capture_word}"

    elif opcode == Opcode.CALL_FUNCTION:
        arg_word = "arg" if arg1 == 1 else "args"
        annotation = f"  ; Call function with {arg1} {arg_word} (function on stack)"

    elif opcode == Opcode.TAIL_CALL_FUNCTION:
        arg_word = "arg" if arg1 == 1 else "args"
        annotation = f"  ; Tail call function with {arg1} {arg_word}"

    elif opcode == Opcode.CALL_BUILTIN:
        arg_word = "arg" if arg2 == 1 else "args"
        builtin_name = builtin_names[arg1] if arg1 < len(builtin_names) else f"<unknown-{arg1}>"
        # Show both name and index for clarity
        annotation = f"  ; Call builtin '{builtin_name}' (#{arg1}) with {arg2} {arg_word}"

    elif opcode == Opcode.JUMP:
        annotation = f"  ; Jump to instruction {arg1}"

    elif opcode == Opcode.JUMP_IF_FALSE:
        annotation = f"  ; Jump to {arg1} if top of stack is false"

    elif opcode == Opcode.JUMP_IF_TRUE:
        annotation = f"  ; Jump to {arg1} if top of stack is true"

    elif opcode == Opcode.RETURN:
        annotation = "  ; Return from function"

    elif opcode == Opcode.LOAD_TRUE:
        annotation = "  ; Load boolean true"

    elif opcode == Opcode.LOAD_FALSE:
        annotation = "  ; Load boolean false"

    elif opcode == Opcode.LOAD_EMPTY_LIST:
        annotation = "  ; Load empty list"

    elif opcode == Opcode.LOAD_NAME:
        if arg1 < len(code.names):
            name = code.names[arg1]
            annotation = f"  ; Load name: {name}"

    elif opcode == Opcode.LOAD_PARENT_VAR:
        annotation = f"  ; Load parent var[{arg2}] from frame {arg1} levels up"

    elif opcode == Opcode.RAISE_ERROR:
        if arg1 < len(code.constants):
            msg = code.constants[arg1]
            annotation = f"  ; Raise error: {format_constant(msg)[:40]}"

    return annotation


def disassemble_with_nested(code: CodeObject, builtin_names: List[str], depth: int = 0, name: str | None = None) -> List[str]:
    """Recursively disassemble code object and all nested code objects."""
    indent = "  " * depth
    display_name = name or code.name or "<top-level>"

    # Add source line info to display name if available
    if code.source_line and code.source_line > 0:
        display_name = f"{display_name} [line {code.source_line}]"

    output = []
    output.append(f"\n{indent}{'='*70}")
    output.append(f"{indent}Function: {display_name}")
    output.append(f"{indent}Instructions: {len(code.instructions)}")
    output.append(f"{indent}Locals: {code.local_count}")
    output.append(f"{indent}Constants: {len(code.constants)}")
    output.append(f"{indent}{'='*70}")

    # Show constants table
    if code.constants:
        output.append(f"{indent}")
        output.append(f"{indent}Constants Table:")
        output.append(f"{indent}{'-'*70}")
        for i, const in enumerate(code.constants):
            const_str = format_constant(const)
            output.append(f"{indent}  [{i:3}] {const_str}")

        output.append(f"{indent}{'-'*70}")
        output.append(f"{indent}")

    # Show annotated disassembly
    output.append(f"{indent}Instructions:")
    output.append(f"{indent}{'-'*70}")
    for i, instr in enumerate(code.instructions):
        annotation = annotate_instruction(instr, code, builtin_names)
        instr_str = f"{i:6}: {instr.opcode.name:20} {instr.arg1:3} {instr.arg2:3}"
        if annotation:
            output.append(f"{indent}{instr_str}{annotation}")
        else:
            output.append(f"{indent}{instr_str}")

    output.append(f"{indent}{'-'*70}")
    output.append(f"{indent}")

    # Recursively disassemble nested code objects
    for i, nested_code in enumerate(code.code_objects):
        nested_name = nested_code.name or f"<nested-{i}>"
        nested_output = disassemble_with_nested(nested_code, builtin_names, depth + 1, nested_name)
        output.extend(nested_output)

    return output


def analyze_function_flow(code: CodeObject) -> Dict[int, str]:
    """Track which functions are stored in which variables."""
    var_map = {}

    for i, instr in enumerate(code.instructions):
        if instr.opcode == Opcode.STORE_VAR and i > 0:
            prev_instr = code.instructions[i - 1]
            if prev_instr.opcode == Opcode.MAKE_CLOSURE:
                closure_idx = prev_instr.arg1
                var_idx = instr.arg2
                if closure_idx < len(code.code_objects):
                    nested_code = code.code_objects[closure_idx]
                    func_name = nested_code.name or f"<closure-{closure_idx}>"
                    line_info = f" [line {nested_code.source_line}]" \
                        if (nested_code.source_line and nested_code.source_line > 0) else ""
                    var_map[var_idx] = f"{func_name}{line_info}"

        if (instr.opcode == Opcode.STORE_VAR and i >= 3):
            if (code.instructions[i-1].opcode == Opcode.CALL_BUILTIN and
                code.instructions[i-2].opcode == Opcode.LOAD_CONST):
                const_idx = code.instructions[i-2].arg1
                var_idx = instr.arg2
                if const_idx < len(code.constants):
                    key = code.constants[const_idx]
                    if hasattr(key, 'value'):
                        key_str = key.value

                    else:
                        key_str = str(key)

                    var_map[var_idx] = f"<from-alist: {key_str}>"

    return var_map


def trace_calls(code: CodeObject, var_map: Dict[int, str]) -> List[str]:
    """Trace function calls."""
    traces = []

    for i, instr in enumerate(code.instructions):
        if instr.opcode == Opcode.CALL_FUNCTION:
            arg_count = instr.arg1
            func_load_idx = i - arg_count - 1
            if func_load_idx >= 0:
                func_instr = code.instructions[func_load_idx]

                func_desc = "???"
                if func_instr.opcode == Opcode.LOAD_VAR:
                    var_idx = func_instr.arg2
                    if var_idx in var_map:
                        func_desc = var_map[var_idx]

                    else:
                        func_desc = f"var[{var_idx}]"

                traces.append(f"Instr {i:3}: CALL_FUNCTION({arg_count} args) -> {func_desc}")

        elif instr.opcode == Opcode.TAIL_CALL_FUNCTION:
            arg_count = instr.arg1
            func_load_idx = i - arg_count - 1
            if func_load_idx >= 0:
                func_instr = code.instructions[func_load_idx]

                func_desc = "???"
                if func_instr.opcode == Opcode.LOAD_VAR:
                    var_idx = func_instr.arg2
                    if var_idx in var_map:
                        func_desc = var_map[var_idx]

                    else:
                        func_desc = f"var[{var_idx}]"

                traces.append(f"Instr {i:3}: TAIL_CALL({arg_count} args) -> {func_desc}")

    return traces


def generate_trace(code: CodeObject, builtin_names: List[str], depth: int = 0, name: str | None =None) -> List[str]:
    """Generate function call trace."""
    indent = "  " * depth
    display_name = name or code.name or "<top-level>"

    if code.source_line and code.source_line > 0:
        display_name = f"{display_name} [line {code.source_line}]"

    output = []
    output.append(f"\n{indent}{'='*70}")
    output.append(f"{indent}Function: {display_name}")
    output.append(f"{indent}Instructions: {len(code.instructions)}")
    output.append(f"{indent}{'='*70}")

    var_map = analyze_function_flow(code)

    if var_map:
        output.append(f"{indent}")
        output.append(f"{indent}Variable Assignments:")
        output.append(f"{indent}{'-'*70}")
        for var_idx in sorted(var_map.keys()):
            output.append(f"{indent}  var[{var_idx:2}] = {var_map[var_idx]}")

        output.append(f"{indent}{'-'*70}")

    traces = trace_calls(code, var_map)

    if traces:
        output.append(f"{indent}")
        output.append(f"{indent}Function Calls:")
        output.append(f"{indent}{'-'*70}")
        for trace in traces:
            output.append(f"{indent}{trace}")

        output.append(f"{indent}{'-'*70}")

    for i, nested_code in enumerate(code.code_objects):
        nested_name = nested_code.name or f"<closure-{i}>"
        nested_output = generate_trace(nested_code, builtin_names, depth + 1, nested_name)
        output.extend(nested_output)

    return output


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Disassemble AIFPL bytecode with detailed annotations",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument('file', help='AIFPL source file to disassemble')
    parser.add_argument('--output', '-o', help='Output file (default: stdout)')
    parser.add_argument('--trace', '-t', action='store_true',
                       help='Also generate function call trace')
    parser.add_argument('--optimize', action='store_true',
                       help='Compile with optimizations enabled')

    args = parser.parse_args()

    # Read source file
    source_path = Path(args.file)
    if not source_path.exists():
        print(f"Error: File not found: {args.file}", file=sys.stderr)
        return 1

    with open(source_path, 'r', encoding='utf-8') as f:
        source = f.read()

    # Compile
    print(f"Compiling: {args.file}", file=sys.stderr)
    file_dir = str(source_path.parent.absolute())
    aifpl = AIFPL(module_path=[file_dir])

    try:
        compiler = AIFPLCompiler(
            module_loader=aifpl,
            optimize=args.optimize
        )
        code = compiler.compile(source)

    except Exception as e:
        print(f"Error compiling: {e}", file=sys.stderr)
        traceback.print_exc()
        return 1

    # Get builtin names for annotation
    builtin_names = AIFPLBuiltinRegistry.BUILTIN_TABLE

    # Generate disassembly
    output_lines = disassemble_with_nested(code, builtin_names, name=args.file)

    # Add trace if requested
    if args.trace:
        output_lines.append("\n\n")
        output_lines.append("="*80)
        output_lines.append("FUNCTION CALL TRACE")
        output_lines.append("="*80)
        trace_lines = generate_trace(code, builtin_names, name=args.file)
        output_lines.extend(trace_lines)

    # Output
    output_text = '\n'.join(output_lines)

    if args.output:
        output_path = Path(args.output)
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(output_text)

        print(f"✓ Disassembly written to: {output_path}", file=sys.stderr)
        print(f"  Total lines: {len(output_lines)}", file=sys.stderr)
        print(f"  Total code objects: {len(code.code_objects) + 1}", file=sys.stderr)

    else:
        print(output_text)

    return 0


if __name__ == '__main__':
    sys.exit(main())
