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
from menai.menai_bytecode import Opcode, CodeObject, Instruction


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


def describe_local(index: int, code: CodeObject) -> str:
    """
    Return a human-readable description of a local variable slot.

    The frame layout is:
      [0 .. param_count-1]                      parameters
      [param_count .. param_count+len(free_vars)-1]  captured variables
      [param_count+len(free_vars) .. local_count-1]  let/letrec locals

    Parameter names come from code.param_names (populated by the codegen).
    Captured variable names come from code.free_vars.
    Body locals have no stored name so are shown as local[N].
    """
    param_count = code.param_count
    capture_count = len(code.free_vars)

    if index < param_count:
        # Parameter slot — use stored name if available
        if index < len(code.param_names):
            return f"param '{code.param_names[index]}'"
        return f"param[{index}]"
    captured_index = index - param_count
    if captured_index < capture_count:
        return f"captured '{code.free_vars[captured_index]}'"

    local_index = index - param_count - capture_count
    return f"local[{local_index}]"


def annotate_instruction(instr: Instruction, code: CodeObject) -> str:
    """Add annotation to instruction showing what it does."""
    opcode = instr.opcode
    src0 = instr.src0
    src1 = instr.src1

    annotation = ""

    if opcode == Opcode.LOAD_CONST:
        if src0 < len(code.constants):
            const = code.constants[src0]
            const_str = format_constant(const)
            if len(const_str) > 40:
                const_str = const_str[:37] + "..."
            annotation = f"  ; Load constant: {const_str}"

    elif opcode == Opcode.LOAD_VAR:
        annotation = f"  ; Load {describe_local(src0, code)}"

    elif opcode == Opcode.STORE_VAR:
        annotation = f"  ; Store to {describe_local(src0, code)}"

    elif opcode == Opcode.ENTER:
        n = src0
        if n > 0 and code.param_names:
            names = ', '.join(f"'{name}'" for name in code.param_names[:n])
            annotation = f"  ; Store {n} params into locals: {names}"

        else:
            annotation = f"  ; Store {n} params into locals 0..{n - 1}"

    elif opcode == Opcode.MAKE_CLOSURE:
        if src0 < len(code.code_objects):
            nested = code.code_objects[src0]
            name = nested.name or f"<lambda-{src0}>"
            loc_parts = []
            if nested.source_file:
                loc_parts.append(nested.source_file)

            if nested.source_line and nested.source_line > 0:
                loc_parts.append(f"line {nested.source_line}")

            line_info = f" at {':'.join(loc_parts)}" if loc_parts else ""
            capture_word = "capture" if src1 == 1 else "captures"
            annotation = f"  ; Create closure for {name}{line_info} with {src1} {capture_word}"

    elif opcode == Opcode.PATCH_CLOSURE:
        # src0 = local slot holding the closure to patch
        # src1 = which captured-values slot to fill (index into free_vars)
        # The value to store was pushed onto the stack by the preceding LOAD_VAR.
        closure_slot = describe_local(src0, code)
        # Find which nested code object lives in that local slot by scanning
        # for a MAKE_CLOSURE followed (eventually) by STORE_VAR src0.
        free_var_name = None
        for j, scan_instr in enumerate(code.instructions):
            if (scan_instr.opcode == Opcode.STORE_VAR and scan_instr.src0 == src0
                    and j > 0 and code.instructions[j - 1].opcode == Opcode.MAKE_CLOSURE):
                nested = code.code_objects[code.instructions[j - 1].src0]
                if src1 < len(nested.free_vars):
                    free_var_name = nested.free_vars[src1]

                break

        slot_desc = f"'{free_var_name}'" if free_var_name else f"slot {src1}"
        annotation = f"  ; Patch {closure_slot}: set capture {slot_desc} from stack"

    elif opcode == Opcode.CALL:
        arg_word = "arg" if src0 == 1 else "args"
        annotation = f"  ; Call function with {src0} {arg_word} (function on stack)"

    elif opcode == Opcode.TAIL_CALL:
        arg_word = "arg" if src0 == 1 else "args"
        annotation = f"  ; Tail call function with {src0} {arg_word}"

    elif opcode == Opcode.RETURN:
        annotation = "  ; Return from function"

    elif opcode == Opcode.LOAD_TRUE:
        annotation = "  ; Load boolean true"

    elif opcode == Opcode.LOAD_FALSE:
        annotation = "  ; Load boolean false"

    elif opcode == Opcode.LOAD_EMPTY_LIST:
        annotation = "  ; Load empty list"

    elif opcode == Opcode.LOAD_NAME:
        if src0 < len(code.names):
            name = code.names[src0]
            annotation = f"  ; Load name: {name}"

    elif opcode == Opcode.RAISE_ERROR:
        if src0 < len(code.constants):
            msg = code.constants[src0]
            annotation = f"  ; Raise error: {format_constant(msg)[:40]}"

    return annotation


def format_instruction(instr: Instruction, index: int) -> str:
    """Format instruction with appropriate arguments based on opcode."""
    n = instr.arg_count()
    if n == 0:
        instr_str = f"{index:4}: {instr.opcode.name}"
    elif n == 2:
        instr_str = f"{index:4}: {instr.opcode.name} {instr.src0} {instr.src1}"
    else:
        instr_str = f"{index:4}: {instr.opcode.name} {instr.src0}"

    # Pad to fixed width so annotations align
    return instr_str.ljust(40)


def disassemble_with_nested(code: CodeObject, depth: int = 0, name: str | None = None) -> List[str]:
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
    output.append(f"\n{indent}{'='*70}")
    output.append(f"{indent}Function: {display_name}")
    output.append(f"{indent}Instructions: {len(code.instructions)}")
    output.append(f"{indent}Locals: {code.local_count}")
    output.append(f"{indent}Constants: {len(code.constants)}")
    output.append(f"{indent}Code Objects: {len(code.code_objects)}")
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

    # Show code objects table
    if code.code_objects:
        output.append(f"{indent}Code Objects Table:")
        output.append(f"{indent}{'-'*70}")
        for i, nested in enumerate(code.code_objects):
            nested_name = nested.name or f"<lambda-{i}>"
            loc_parts = []
            if nested.source_file:
                loc_parts.append(nested.source_file)

            if nested.source_line and nested.source_line > 0:
                loc_parts.append(f"line {nested.source_line}")

            loc_str = f" [{':'.join(loc_parts)}]" if loc_parts else ""
            output.append(f"{indent}  [{i:3}] {nested_name}{loc_str}")

        output.append(f"{indent}{'-'*70}")
        output.append(f"{indent}")

    # Show annotated disassembly
    output.append(f"{indent}Instructions:")
    output.append(f"{indent}{'-'*70}")

    # Pre-pass: collect all jump target indices.
    jump_opcodes = {Opcode.JUMP, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE}
    jump_targets = {
        instr.src0
        for instr in code.instructions
        if instr.opcode in jump_opcodes
    }
    conditional_jump_opcodes = {Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE}

    for i, instr in enumerate(code.instructions):
        is_target = i in jump_targets
        if is_target and i > 0:
            output.append(f"{indent}")

        annotation = annotate_instruction(instr, code)
        instr_str = format_instruction(instr, i)

        # For jump target lines, replace the last two characters of the indent
        # with "► " so the marker sits flush at the indent boundary and all
        # subsequent columns remain aligned with non-target lines.
        target_marker = "► " if is_target else "  "

        if annotation:
            output.append(f"{indent}{target_marker}{instr_str}{annotation}")
        else:
            output.append(f"{indent}{target_marker}{instr_str}")

        # Blank line after a conditional jump, unless the next instruction is
        # already a jump target (which will insert its own blank line above).
        if instr.opcode in conditional_jump_opcodes and (i + 1) not in jump_targets:
            output.append(f"{indent}")

    output.append(f"{indent}{'-'*70}")
    output.append(f"{indent}")

    # Recursively disassemble nested code objects
    for i, nested_code in enumerate(code.code_objects):
        nested_name = nested_code.name or f"<nested-{i}>"
        nested_output = disassemble_with_nested(nested_code, depth + 1, nested_name)
        output.extend(nested_output)

    return output


def analyze_function_flow(code: CodeObject) -> Dict[int, str]:
    """Track which functions are stored in which variables."""
    var_map = {}

    for i, instr in enumerate(code.instructions):
        if instr.opcode == Opcode.STORE_VAR and i > 0:
            prev_instr = code.instructions[i - 1]
            if prev_instr.opcode == Opcode.MAKE_CLOSURE:
                closure_idx = prev_instr.src0
                var_idx = instr.src1
                if closure_idx < len(code.code_objects):
                    nested_code = code.code_objects[closure_idx]
                    func_name = nested_code.name or f"<closure-{closure_idx}>"
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
            arg_count = instr.src0
            func_load_idx = i - arg_count - 1
            if func_load_idx >= 0:
                func_instr = code.instructions[func_load_idx]

                func_desc = "???"
                if func_instr.opcode == Opcode.LOAD_VAR:
                    var_idx = func_instr.src1
                    if var_idx in var_map:
                        func_desc = var_map[var_idx]

                    else:
                        func_desc = f"var[{var_idx}]"

                traces.append(f"Instr {i:3}: CALL({arg_count} args) -> {func_desc}")

        elif instr.opcode == Opcode.TAIL_CALL:
            arg_count = instr.src0
            func_load_idx = i - arg_count - 1
            if func_load_idx >= 0:
                func_instr = code.instructions[func_load_idx]

                func_desc = "???"
                if func_instr.opcode == Opcode.LOAD_VAR:
                    var_idx = func_instr.src1
                    if var_idx in var_map:
                        func_desc = var_map[var_idx]

                    else:
                        func_desc = f"var[{var_idx}]"

                traces.append(f"Instr {i:3}: TAIL_CALL({arg_count} args) -> {func_desc}")

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
        compiler = MenaiCompiler(
            module_loader=menai,
            optimize=args.optimize
        )
        code = compiler.compile(source)

    except Exception as e:
        print(f"Error compiling: {e}", file=sys.stderr)
        traceback.print_exc()
        return 1

    # Generate disassembly
    output_lines = disassemble_with_nested(code, name=args.file)

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
