#!/usr/bin/env python3
"""
AIFPL Bytecode Analyzer - Compare bytecode generation with/without optimizations.

This tool helps quantify the impact of compiler optimizations by:
1. Compiling code with and without optimizations
2. Comparing bytecode size, instruction counts, and complexity
3. Generating detailed reports and visualizations
4. Running benchmarks to measure actual performance impact

Usage:
    python bytecode_analyzer.py --file example.aifpl
    python bytecode_analyzer.py --expr "(+ 1 2)"
    python bytecode_analyzer.py --test-suite suite.aifpl  # Multiple expressions
"""

import sys
import re
from pathlib import Path
from typing import Dict, List, Tuple
from dataclasses import dataclass, field
import argparse
import json
from collections import Counter

# Add parent directory to path to import aifpl
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from aifpl import AIFPL
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_bytecode import CodeObject


def split_test_file(content: str) -> List[Tuple[str, str]]:
    """
    Split test file into (name, code) tuples.

    Sections are delimited by lines like:
    ; ============================================================================
    ; 1. TEST NAME
    ; ============================================================================

    Returns list of (test_name, code) tuples.
    """
    lines = content.split('\n')

    sections = []
    current_name = None
    current_code = []
    in_section = False

    for i, line in enumerate(lines):
        # Check for section delimiter
        if line.strip().startswith('; ===='):
            # Next non-delimiter line should be the name
            if i + 1 < len(lines):
                next_line = lines[i + 1].strip()
                if next_line.startswith(';') and not next_line.startswith('; ===='):
                    # Save previous section
                    if current_name and current_code:
                        code = '\n'.join(current_code).strip()
                        if code and not code.startswith(';'):
                            sections.append((current_name, code))

                    # Start new section
                    current_name = next_line.lstrip(';').strip()
                    # Remove numbering like "1. "
                    current_name = re.sub(r'^\d+\.\s*', '', current_name)
                    current_code = []
                    in_section = True

        # Skip delimiter lines and section name lines
        elif line.strip().startswith('; ===='):
            continue
        elif in_section and line.strip().startswith(';') and current_name and line.strip().lstrip(';').strip() == current_name:
            continue

        # Collect code lines (non-comment, non-empty)
        elif in_section:
            # Skip pure comment lines
            if line.strip() and not line.strip().startswith(';'):
                current_code.append(line)
            elif not line.strip():
                # Keep blank lines within code
                if current_code:
                    current_code.append(line)

    # Add last section
    if current_name and current_code:
        code = '\n'.join(current_code).strip()
        if code and not code.startswith(';'):
            sections.append((current_name, code))

    return sections


@dataclass
class BytecodeStats:
    """Statistics about compiled bytecode."""
    total_instructions: int = 0
    instruction_counts: Dict[str, int] = field(default_factory=dict)
    constant_count: int = 0
    name_count: int = 0
    code_object_count: int = 0
    max_locals: int = 0

    # Derived metrics
    arithmetic_ops: int = 0
    load_ops: int = 0
    store_ops: int = 0
    control_flow_ops: int = 0
    function_ops: int = 0

    # Complexity metrics
    cyclomatic_complexity: int = 1  # Start at 1 for single path

    def compute_derived_metrics(self):
        """Compute derived metrics from instruction counts."""
        arithmetic_opcodes = {'CALL_BUILTIN'}  # Most arithmetic goes through builtins
        load_opcodes = {'LOAD_CONST', 'LOAD_VAR', 'LOAD_NAME', 'LOAD_TRUE', 'LOAD_FALSE', 'LOAD_EMPTY_LIST'}
        store_opcodes = {'STORE_VAR'}
        control_opcodes = {'JUMP', 'JUMP_IF_FALSE', 'JUMP_IF_TRUE'}
        function_opcodes = {'MAKE_CLOSURE', 'CALL', 'CALL_BUILTIN', 'RETURN'}

        self.arithmetic_ops = sum(count for op, count in self.instruction_counts.items() if op in arithmetic_opcodes)
        self.load_ops = sum(count for op, count in self.instruction_counts.items() if op in load_opcodes)
        self.store_ops = sum(count for op, count in self.instruction_counts.items() if op in store_opcodes)
        self.control_flow_ops = sum(count for op, count in self.instruction_counts.items() if op in control_opcodes)
        self.function_ops = sum(count for op, count in self.instruction_counts.items() if op in function_opcodes)

        # Cyclomatic complexity: number of decision points + 1
        self.cyclomatic_complexity = 1 + self.instruction_counts.get('JUMP_IF_FALSE', 0) + \
                                        self.instruction_counts.get('JUMP_IF_TRUE', 0)


@dataclass
class ComparisonResult:
    """Result of comparing two bytecode versions."""
    unoptimized: BytecodeStats
    optimized: BytecodeStats

    # Improvements
    instruction_reduction: int = 0
    instruction_reduction_pct: float = 0.0
    constant_reduction: int = 0
    load_reduction: int = 0
    arithmetic_reduction: int = 0

    def compute_improvements(self):
        """Compute improvement metrics."""
        self.instruction_reduction = self.unoptimized.total_instructions - self.optimized.total_instructions
        if self.unoptimized.total_instructions > 0:
            self.instruction_reduction_pct = (self.instruction_reduction / self.unoptimized.total_instructions) * 100

        self.constant_reduction = self.unoptimized.constant_count - self.optimized.constant_count
        self.load_reduction = self.unoptimized.load_ops - self.optimized.load_ops
        self.arithmetic_reduction = self.unoptimized.arithmetic_ops - self.optimized.arithmetic_ops


class BytecodeAnalyzer:
    """Analyzes and compares AIFPL bytecode."""

    def __init__(self, module_path: List[str] | None = None):
        """
        Initialize the analyzer with module support.
        
        Args:
            module_path: List of directories to search for modules (default: ["."])
        """
        # Create AIFPL instance for module loading
        if module_path is None:
            module_path = ["."]

        self.aifpl = AIFPL(module_path=module_path)

    def compile_code(self, source: str, optimize: bool = False) -> CodeObject:
        """Compile AIFPL source code to bytecode."""
        compiler = AIFPLCompiler(
            optimize=optimize,
            module_loader=self.aifpl
        )
        return compiler.compile(source)

    def analyze_code_object(self, code: CodeObject) -> BytecodeStats:
        """Analyze a code object and extract statistics."""
        stats = BytecodeStats()

        # Count instructions
        stats.total_instructions = len(code.instructions)
        instruction_counter = Counter(instr.opcode.name for instr in code.instructions)
        stats.instruction_counts = dict(instruction_counter)

        # Count resources
        stats.constant_count = len(code.constants)
        stats.name_count = len(code.names)
        stats.code_object_count = len(code.code_objects)
        stats.max_locals = code.local_count

        # Recursively analyze nested code objects
        for nested_code in code.code_objects:
            nested_stats = self.analyze_code_object(nested_code)
            stats.total_instructions += nested_stats.total_instructions
            stats.code_object_count += nested_stats.code_object_count
            # Merge instruction counts
            for op, count in nested_stats.instruction_counts.items():
                stats.instruction_counts[op] = stats.instruction_counts.get(op, 0) + count

        # Compute derived metrics
        stats.compute_derived_metrics()

        return stats

    def compare(self, source: str) -> ComparisonResult:
        """Compare bytecode with and without optimizations."""
        # Compile without optimization
        unopt_code = self.compile_code(source, optimize=False)
        unopt_stats = self.analyze_code_object(unopt_code)

        # Compile with optimization
        opt_code = self.compile_code(source, optimize=True)
        opt_stats = self.analyze_code_object(opt_code)

        # Compare
        result = ComparisonResult(unoptimized=unopt_stats, optimized=opt_stats)
        result.compute_improvements()

        return result

    def format_stats(self, stats: BytecodeStats, label: str = "") -> str:
        """Format bytecode statistics as a readable string."""
        lines = []
        if label:
            lines.append(f"\n{'='*60}")
            lines.append(f"{label:^60}")
            lines.append(f"{'='*60}")

        lines.append("\nOverall Statistics:")
        lines.append(f"  Total Instructions:      {stats.total_instructions:>6}")
        lines.append(f"  Constants:               {stats.constant_count:>6}")
        lines.append(f"  Names:                   {stats.name_count:>6}")
        lines.append(f"  Code Objects:            {stats.code_object_count:>6}")
        lines.append(f"  Max Locals:              {stats.max_locals:>6}")
        lines.append(f"  Cyclomatic Complexity:   {stats.cyclomatic_complexity:>6}")

        lines.append("\nInstruction Breakdown:")
        lines.append(f"  Load Operations:         {stats.load_ops:>6}")
        lines.append(f"  Store Operations:        {stats.store_ops:>6}")
        lines.append(f"  Arithmetic Operations:   {stats.arithmetic_ops:>6}")
        lines.append(f"  Control Flow:            {stats.control_flow_ops:>6}")
        lines.append(f"  Function Operations:     {stats.function_ops:>6}")

        if stats.instruction_counts:
            lines.append("\nInstruction Frequency:")
            for opcode, count in sorted(stats.instruction_counts.items(), key=lambda x: -x[1]):
                lines.append(f"  {opcode:<24} {count:>6}")

        return "\n".join(lines)

    def format_comparison(self, result: ComparisonResult) -> str:
        """Format comparison result as a readable string."""
        lines = []

        lines.append("\n" + "="*60)
        lines.append("BYTECODE COMPARISON REPORT".center(60))
        lines.append("="*60)

        # Summary
        lines.append(f"\n{'SUMMARY':^60}")
        lines.append("-"*60)
        lines.append(f"  Instructions Eliminated: {result.instruction_reduction:>6} ({result.instruction_reduction_pct:>5.1f}%)")
        lines.append(f"  Constants Eliminated:    {result.constant_reduction:>6}")
        lines.append(f"  Load Ops Eliminated:     {result.load_reduction:>6}")
        lines.append(f"  Arithmetic Ops Eliminated: {result.arithmetic_reduction:>4}")

        # Detailed stats
        lines.append(self.format_stats(result.unoptimized, "UNOPTIMIZED BYTECODE"))
        lines.append(self.format_stats(result.optimized, "OPTIMIZED BYTECODE"))

        # Improvement analysis
        lines.append(f"\n{'IMPROVEMENT ANALYSIS':^60}")
        lines.append("-"*60)

        if result.instruction_reduction > 0:
            lines.append(f"  ✓ Reduced instruction count by {result.instruction_reduction_pct:.1f}%")
        else:
            lines.append("  ✗ No instruction reduction")

        if result.constant_reduction > 0:
            lines.append(f"  ✓ Eliminated {result.constant_reduction} constants")

        if result.load_reduction > 0:
            lines.append(f"  ✓ Eliminated {result.load_reduction} load operations")

        if result.arithmetic_reduction > 0:
            lines.append(f"  ✓ Eliminated {result.arithmetic_reduction} arithmetic operations")

        # Complexity comparison
        complexity_reduction = result.unoptimized.cyclomatic_complexity - result.optimized.cyclomatic_complexity
        if complexity_reduction > 0:
            lines.append(f"  ✓ Reduced cyclomatic complexity by {complexity_reduction}")

        lines.append("\n" + "="*60)

        return "\n".join(lines)

    def generate_json_report(self, result: ComparisonResult) -> str:
        """Generate JSON report for programmatic analysis."""
        report = {
            "unoptimized": {
                "total_instructions": result.unoptimized.total_instructions,
                "constants": result.unoptimized.constant_count,
                "names": result.unoptimized.name_count,
                "code_objects": result.unoptimized.code_object_count,
                "max_locals": result.unoptimized.max_locals,
                "cyclomatic_complexity": result.unoptimized.cyclomatic_complexity,
                "instruction_counts": result.unoptimized.instruction_counts,
            },
            "optimized": {
                "total_instructions": result.optimized.total_instructions,
                "constants": result.optimized.constant_count,
                "names": result.optimized.name_count,
                "code_objects": result.optimized.code_object_count,
                "max_locals": result.optimized.max_locals,
                "cyclomatic_complexity": result.optimized.cyclomatic_complexity,
                "instruction_counts": result.optimized.instruction_counts,
            },
            "improvements": {
                "instruction_reduction": result.instruction_reduction,
                "instruction_reduction_pct": result.instruction_reduction_pct,
                "constant_reduction": result.constant_reduction,
                "load_reduction": result.load_reduction,
                "arithmetic_reduction": result.arithmetic_reduction,
            }
        }
        return json.dumps(report, indent=2)


def main():
    """Main entry point for bytecode analyzer."""
    parser = argparse.ArgumentParser(
        description="Analyze AIFPL bytecode generation and optimization impact"
    )

    parser.add_argument('--expr', '-e', type=str,
                       help='AIFPL expression to analyze')
    parser.add_argument('--file', '-f', type=str,
                       help='AIFPL source file (single expression)')
    parser.add_argument('--test-suite', '-t', type=str,
                       help='Test suite file (multiple expressions with section headers)')
    parser.add_argument('--batch', '-b', nargs='+',
                       help='Multiple files to analyze (generates summary)')
    parser.add_argument('--json', '-j', action='store_true',
                       help='Output results as JSON')
    parser.add_argument('--disassemble', '-d', action='store_true',
                       help='Show disassembled bytecode')

    args = parser.parse_args()

    # Determine module path based on file location
    module_path = ["."]
    if args.file:
        file_dir = str(Path(args.file).parent.absolute())
        module_path = [file_dir]

    analyzer = BytecodeAnalyzer(module_path=module_path)

    # Single expression
    if args.expr:
        result = analyzer.compare(args.expr)
        if args.json:
            print(analyzer.generate_json_report(result))
        else:
            print(analyzer.format_comparison(result))
            if args.disassemble:
                print("\n" + "="*60)
                print("DISASSEMBLY (Unoptimized)".center(60))
                print("="*60)
                code = analyzer.compile_code(args.expr, optimize=False)
                print(code.disassemble())
                print("\n" + "="*60)
                print("DISASSEMBLY (Optimized)".center(60))
                print("="*60)
                code = analyzer.compile_code(args.expr, optimize=True)
                print(code.disassemble())

    # Single file
    elif args.file:
        with open(args.file, 'r', encoding='utf-8') as f:
            source = f.read()

        result = analyzer.compare(source)
        if args.json:
            print(analyzer.generate_json_report(result))

        else:
            print(f"\nAnalyzing: {args.file}")
            print(analyzer.format_comparison(result))
            if args.disassemble:
                print("\n" + "="*60)
                print("DISASSEMBLY (Unoptimized)".center(60))
                print("="*60)
                code = analyzer.compile_code(source, optimize=False)
                print(code.disassemble())
                print("\n" + "="*60)
                print("DISASSEMBLY (Optimized)".center(60))
                print("="*60)
                code = analyzer.compile_code(source, optimize=True)
                print(code.disassemble())

    # Test suite (multiple expressions)
    elif args.test_suite:
        with open(args.test_suite, 'r', encoding='utf-8') as f:
            content = f.read()

        sections = split_test_file(content)

        if not sections:
            print(f"Error: No test sections found in {args.test_suite}", file=sys.stderr)
            return 1

        print(f"\nFound {len(sections)} test cases in {args.test_suite}\n")

        all_results = []
        for i, (name, code) in enumerate(sections, 1):
            try:
                print(f"[{i}/{len(sections)}] {name}... ", end='', flush=True)
                result = analyzer.compare(code)
                all_results.append((name, result))
                print(f"{result.instruction_reduction_pct:>5.1f}% reduction")
            except Exception as e:
                print(f"ERROR: {e}")
                continue

        # Summary
        if all_results:
            print("\n" + "="*60)
            print("SUMMARY".center(60))
            print("="*60)

            total_unopt = sum(r.unoptimized.total_instructions for _, r in all_results)
            total_opt = sum(r.optimized.total_instructions for _, r in all_results)
            overall_reduction = total_unopt - total_opt
            overall_pct = (overall_reduction / total_unopt * 100) if total_unopt > 0 else 0

            print(f"\nTest Cases: {len(all_results)}")
            print(f"Total Instructions (Before): {total_unopt}")
            print(f"Total Instructions (After):  {total_opt}")
            print(f"Overall Reduction: {overall_reduction} ({overall_pct:.1f}%)")
            print("\n" + "="*60)

    # Batch analysis
    elif args.batch:
        results = []
        for file_path in args.batch:
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    source = f.read()

                result = analyzer.compare(source)
                results.append((file_path, result))

            except Exception as e:
                print(f"Error analyzing {file_path}: {e}", file=sys.stderr)

        # Generate summary
        if args.json:
            summary = {
                "files": [
                    {
                        "path": path,
                        "improvements": {
                            "instruction_reduction": result.instruction_reduction,
                            "instruction_reduction_pct": result.instruction_reduction_pct,
                        }
                    }
                    for path, result in results
                ]
            }
            print(json.dumps(summary, indent=2))
        else:
            print("\n" + "="*60)
            print("BATCH ANALYSIS SUMMARY".center(60))
            print("="*60)
            print(f"\nAnalyzed {len(results)} files:\n")

            total_unopt_instructions = 0
            total_opt_instructions = 0

            for path, result in results:
                total_unopt_instructions += result.unoptimized.total_instructions
                total_opt_instructions += result.optimized.total_instructions

                print(f"  {Path(path).name:<40}")
                print(f"    Instructions: {result.unoptimized.total_instructions:>4} → {result.optimized.total_instructions:>4} "
                      f"({result.instruction_reduction_pct:>5.1f}% reduction)")

            overall_reduction = total_unopt_instructions - total_opt_instructions
            overall_pct = (overall_reduction / total_unopt_instructions * 100) if total_unopt_instructions > 0 else 0

            print(f"\n{'Overall:':<42}")
            print(f"  Instructions: {total_unopt_instructions:>4} → {total_opt_instructions:>4} "
                  f"({overall_pct:>5.1f}% reduction)")
            print("="*60)

    else:
        parser.print_help()
        return 1

    return 0


if __name__ == '__main__':
    sys.exit(main())
