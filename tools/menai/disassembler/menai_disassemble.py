#!/usr/bin/env python3
"""
Menai Disassembler - Compile and disassemble Menai modules with detailed annotations.

This tool compiles Menai source files and generates annotated bytecode disassembly
showing:
- Constants table
- Variable assignments and function names
- Annotated instructions with what they do
- Source line numbers for each function
- Nested function hierarchy

Usage:
    python menai_disassemble.py <file.menai>
    python menai_disassemble.py <file.menai> --output disasm.txt
    python menai_disassemble.py <file.menai> --trace  # Also show function call trace
"""

import argparse
from pathlib import Path
import sys
import traceback
from typing import List, Dict

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from menai import Menai
from menai.menai_compiler import MenaiCompiler
from menai.menai_value import MenaiValue
from menai.menai_bytecode import Opcode, CodeObject, Instruction, reg_name


_ANSI_CYAN = "\033[36m"
_ANSI_YELLOW = "\033[33m"
_ANSI_GREEN = "\033[32m"
_ANSI_GREY = "\033[90m"
_ANSI_RESET = "\033[0m"


def _cyan(text: str, color: bool) -> str:
    return f"{_ANSI_CYAN}{text}{_ANSI_RESET}" if color else text


def _yellow(text: str, color: bool) -> str:
    return f"{_ANSI_YELLOW}{text}{_ANSI_RESET}" if color else text


def _grey(text: str, color: bool) -> str:
    return f"{_ANSI_GREY}{text}{_ANSI_RESET}" if color else text


def _green(text: str, color: bool) -> str:
    return f"{_ANSI_GREEN}{text}{_ANSI_RESET}" if color else text


def format_constant(const: object) -> str:
    """Format a constant for display."""
    if isinstance(const, str):
        if len(const) > 50:
            return f'"{const[:47]}..."'

        return f'"{const}"'

    if isinstance(const, MenaiValue):
        val_str = str(const)
        if len(val_str) > 50:
            return f'{val_str[:47]}...'

        return val_str

    return str(const)


def clean_name(name: str) -> str:
    """Strip the '(N param[s])' suffix the bytecode builder appends to closure names."""
    if '(' in name:
        return name[:name.index('(')].strip()
    return name


def annotate_instruction(instr: Instruction, code: CodeObject) -> str:
    """Add annotation to instruction showing what it does."""
    opcode = instr.opcode
    src0 = instr.src0

    annotation = ""

    if opcode == Opcode.LOAD_CONST:
        if src0 < len(code.constants):
            const = code.constants[src0]
            const_str = format_constant(const)
            if len(const_str) > 40:
                const_str = const_str[:37] + "..."

            annotation = f"  ; {const_str}"

    elif opcode == Opcode.LOAD_NONE:
        annotation = "  ; #none"

    elif opcode in (Opcode.LOAD_TRUE, Opcode.LOAD_FALSE):
        val = "#t" if opcode == Opcode.LOAD_TRUE else "#f"
        annotation = f"  ; {val}"

    elif opcode == Opcode.LOAD_EMPTY_LIST:
        annotation = "  ; []"

    elif opcode == Opcode.LOAD_NAME:
        if src0 < len(code.names):
            annotation = f"  ; '{code.names[src0]}'"

    elif opcode == Opcode.MAKE_CLOSURE:
        if instr.src0 < len(code.code_objects):
            nested = code.code_objects[instr.src0]
            name = nested.name or f"<lambda-{src0}>"
            loc_parts = []
            if nested.source_file:
                loc_parts.append(nested.source_file)

            if nested.source_line and nested.source_line > 0:
                loc_parts.append(f"line {nested.source_line}")

            line_info = f" at {':'.join(loc_parts)}" if loc_parts else ""
            annotation = f"  ; closure for '{clean_name(name)}'{line_info}"

    elif opcode == Opcode.PATCH_CLOSURE:
        # src0 = closure register, src1 = capture index, src2 = value register.
        # Scan backwards for the MAKE_CLOSURE that produced each register so we
        # can name the closure and the free-var being filled.
        closure_name = None
        free_var_name = None
        for scan_instr in code.instructions:
            if scan_instr.opcode == Opcode.MAKE_CLOSURE and scan_instr.dest == instr.src0:
                nested = code.code_objects[scan_instr.src0]
                closure_name = clean_name(nested.name) if nested.name else reg_name(instr.src0, code)
                if instr.src1 < len(nested.free_vars):
                    free_var_name = nested.free_vars[instr.src1]

                break

        # Name the value being patched in: use the closure's own name if the
        # value register also holds a known closure, otherwise use reg_name.
        value_closure_name = None
        for scan_instr in code.instructions:
            if scan_instr.opcode == Opcode.MAKE_CLOSURE and scan_instr.dest == instr.src2:
                value_closure_name = clean_name(code.code_objects[scan_instr.src0].name)
                break

        lhs_closure = closure_name or reg_name(instr.src0, code)
        lhs_capture = f"'{free_var_name}'" if free_var_name else f"capture[{instr.src1}]"
        rhs_sym = value_closure_name
        rhs_reg = reg_name(instr.src2, code)
        rhs = f"'{rhs_sym}'" if rhs_sym else rhs_reg
        annotation = f"  ; '{lhs_closure}'.{lhs_capture} = {rhs}"

    elif opcode == Opcode.EMIT_TRACE:
        annotation = f"  ; Emit {reg_name(instr.src0, code)} to trace watcher"

    elif opcode == Opcode.RAISE_ERROR:
        if src0 < len(code.constants):
            msg = code.constants[src0]
            annotation = f"  ; Raise error: {format_constant(msg)[:40]}"

    return annotation


def format_instruction(instr: Instruction, index: int, code: CodeObject) -> str:
    """Format an instruction with symbolic register names derived from code."""
    instr_str = f"{index:4}: {instr.format(code)}"
    # Pad to fixed width so annotations align; 48 chars covers the longest opcodes
    return instr_str.ljust(48)


def disassemble_with_nested(code: CodeObject, depth: int = 0, name: str | None = None, color: bool = False) -> List[str]:
    """Recursively disassemble code object and all nested code objects."""
    indent = "  " * depth
    display_name = name or code.name or "<top-level>"

    # Add source line info to display name if available
    loc_parts = []
    if code.source_file:
        loc_parts.append(code.source_file)

    if code.source_line and code.source_line > 0:
        loc_parts.append(f"line {code.source_line}")

    if loc_parts:
        display_name = f"{display_name} [{':'.join(loc_parts)}]"

    output = []
    output.append(f"{indent}{'-'*70}")                                    # plain: function opener
    output.append(f"{indent}{_yellow('Function: ' + display_name, color)}")
    output.append(_grey(f"{indent}{'-'*70}", color))

    # Show code objects table
    if code.code_objects:
        output.append(f"{indent}{_green('Code Objects: ' + str(len(code.code_objects)), color)}")
        output.append(_grey(f"{indent}{'-'*70}", color))
        for i, nested in enumerate(code.code_objects):
            nested_name = clean_name(nested.name) if nested.name else f"<lambda-{i}>"
            loc_parts = []
            if nested.source_file:
                loc_parts.append(nested.source_file)

            if nested.source_line and nested.source_line > 0:
                loc_parts.append(f"line {nested.source_line}")

            loc_str = f" [{':'.join(loc_parts)}]" if loc_parts else ""
            coid = f"x{i}"
            output.append(f"{indent}{_cyan(f'{coid:>6}: {nested_name}{loc_str}', color)}")

        output.append(_grey(f"{indent}{'-'*70}", color))

    # Show constants table
    if code.constants:
        output.append(f"{indent}{_green('Constants: ' + str(len(code.constants)), color)}")
        output.append(_grey(f"{indent}{'-'*70}", color))
        for i, const in enumerate(code.constants):
            const_str = format_constant(const)
            cid = f"k{i}"
            output.append(f"{indent}{_cyan(f'{cid:>6}: {const_str}', color)}")

        output.append(_grey(f"{indent}{'-'*70}", color))

    # Show register map for params and captures (only when present)
    param_count = code.param_count
    if param_count:
        output.append(f"{indent}{_green('Inputs: ' + str(code.param_count), color)}")
        output.append(_grey(f"{indent}{'-'*70}", color))
        for i, pname in enumerate(code.param_names):
            rid = f"i{i}"
            output.append(f"{indent}{_cyan(f"{rid:>6}: '{pname}'", color)}")

        output.append(_grey(f"{indent}{'-'*70}", color))

    # Show register map for params and captures (only when present)
    capture_count = len(code.free_vars)
    if capture_count:
        output.append(f"{indent}{_green('Captured: ' + str(len(code.free_vars)), color)}")
        output.append(_grey(f"{indent}{'-'*70}", color))
        for i, fname in enumerate(code.free_vars):
            rid = f"c{i}"
            output.append(f"{indent}{_cyan(f"{rid:>6}: '{fname}'", color)}")

        output.append(_grey(f"{indent}{'-'*70}", color))

    locals_count = code.local_count - param_count - capture_count
    if locals_count:
        output.append(f"{indent}{_green('Locals: ' + str(locals_count), color)}")
        output.append(_grey(f"{indent}{'-'*70}", color))

    # Show annotated disassembly
    output.append(f"{indent}{_green('Instructions: ' + str(len(code.instructions)), color)}")
    output.append(_grey(f"{indent}{'-'*70}", color))

    # Pre-pass: collect all jump target indices.
    # JUMP target is in src0; JUMP_IF_FALSE/TRUE target is in src1.
    jump_targets = {
        instr.src1 if instr.opcode in (Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE)
        else instr.src0
        for instr in code.instructions
        if instr.opcode in (Opcode.JUMP, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE)
    }
    control_flow_opcodes = {
        Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE, Opcode.CALL, Opcode.APPLY
    }

    for i, instr in enumerate(code.instructions):
        is_target = i in jump_targets
        if is_target and i > 0:
            output.append(f"{indent}")

        annotation = annotate_instruction(instr, code)
        instr_str = format_instruction(instr, i, code)

        # For jump target lines, prepend "► " so the marker sits flush at the
        # indent boundary and all subsequent columns remain aligned with
        # non-target lines.
        target_marker = "► " if is_target else "  "

        if annotation:
            output.append(f"{indent}{target_marker}{instr_str}{_green(annotation, color)}")

        else:
            output.append(f"{indent}{target_marker}{instr_str}")

        # Blank line after a control flow opcode, unless the next instruction is
        # already a jump target (which will insert its own blank line above).
        if instr.opcode in control_flow_opcodes and (i + 1) not in jump_targets:
            output.append(f"{indent}")

    output.append(f"{indent}{'-'*70}")                                    # plain: function closer
    output.append(f"{indent}")

    # Recursively disassemble nested code objects
    for i, nested_code in enumerate(code.code_objects):
        nested_name = clean_name(nested_code.name) if nested_code.name else f"<nested-{i}>"
        nested_output = disassemble_with_nested(nested_code, depth + 1, nested_name, color)
        output.extend(nested_output)

    return output


def analyze_function_flow(code: CodeObject) -> Dict[int, str]:
    """Track which functions are stored in which variables."""
    var_map = {}

    for instr in code.instructions:
        if instr.opcode == Opcode.MAKE_CLOSURE:
            closure_idx = instr.src0
            var_idx = instr.dest
            if closure_idx < len(code.code_objects):
                nested_code = code.code_objects[closure_idx]
                func_name = clean_name(nested_code.name) if nested_code.name else f"<closure-{closure_idx}>"
                loc_parts = []
                if nested_code.source_file:
                    loc_parts.append(nested_code.source_file)

                if nested_code.source_line and nested_code.source_line > 0:
                    loc_parts.append(f"line {nested_code.source_line}")

                line_info = f" [{':'.join(loc_parts)}]" if loc_parts else ""
                var_map[var_idx] = f"{func_name}{line_info}"

    return var_map


def trace_calls(code: CodeObject, var_map: Dict[int, str]) -> List[str]:
    """Trace function calls."""
    traces = []

    for i, instr in enumerate(code.instructions):
        if instr.opcode == Opcode.CALL:
            arg_count = instr.src1
            func_reg = instr.src0
            func_desc = var_map.get(func_reg, f"r{func_reg}")
            traces.append(f"Instr {i:3}: CALL r{func_reg} ({arg_count} args) -> {func_desc}")

        elif instr.opcode == Opcode.TAIL_CALL:
            arg_count = instr.src1
            func_reg = instr.src0
            func_desc = var_map.get(func_reg, f"r{func_reg}")
            traces.append(f"Instr {i:3}: TAIL_CALL r{func_reg} ({arg_count} args) -> {func_desc}")

    return traces


def generate_trace(code: CodeObject, depth: int = 0, name: str | None = None) -> List[str]:
    """Generate function call trace."""
    indent = "  " * depth
    display_name = name or code.name or "<top-level>"

    loc_parts = []
    if code.source_file:
        loc_parts.append(code.source_file)

    if code.source_line and code.source_line > 0:
        loc_parts.append(f"line {code.source_line}")

    if loc_parts:
        display_name = f"{display_name} [{':'.join(loc_parts)}]"

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
        nested_output = generate_trace(nested_code, depth + 1, nested_name)
        output.extend(nested_output)

    return output


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Disassemble Menai bytecode with detailed annotations",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument('file', help='Menai source file to disassemble')
    parser.add_argument('--output', '-o', help='Output file (default: stdout)')
    parser.add_argument('--trace', '-t', action='store_true', help='Also generate function call trace')
    parser.add_argument('--no-color', action='store_true', help='Disable ANSI colour output')
    parser.add_argument('--color', '-c', action='store_true', help='Force ANSI colour output')

    args = parser.parse_args()
    color = (not args.no_color and not args.output and sys.stdout.isatty()) or args.color

    # Read source file
    source_path = Path(args.file)
    if not source_path.exists():
        print(f"Error: File not found: {args.file}", file=sys.stderr)
        return 1

    with open(source_path, 'r', encoding='utf-8') as f:
        source = f.read()

    # Compile
    print(f"Compiling: {args.file}", file=sys.stderr)

    # Build a deduplicated module search path:
    #   1. The file's own directory (for bare imports like "calendar" when
    #      the source file lives alongside its modules)
    #   2. The current working directory (so that import paths written
    #      relative to the project root, e.g. "tools/planner/calendar",
    #      resolve correctly when the tool is run from the root)
    file_dir = str(source_path.parent.absolute())
    cwd = str(Path.cwd())
    seen = set()
    module_path = [d for d in [file_dir, cwd] if not (d in seen or seen.add(d))]
    menai = Menai(module_path=module_path)

    try:
        compiler = MenaiCompiler(module_loader=menai)
        code = compiler.compile(source)

    except Exception as e:
        print(f"Error compiling: {e}", file=sys.stderr)
        traceback.print_exc()
        return 1

    # Generate disassembly
    output_lines = disassemble_with_nested(code, name=args.file, color=color)

    # Add trace if requested
    if args.trace:
        output_lines.append("\n\n")
        output_lines.append("="*80)
        output_lines.append("FUNCTION CALL TRACE")
        output_lines.append("="*80)
        trace_lines = generate_trace(code, name=args.file)
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
