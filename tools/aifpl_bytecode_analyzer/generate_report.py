#!/usr/bin/env python3
"""
Generate comprehensive optimization impact report.

This script runs both bytecode analysis and performance benchmarks,
then correlates the results to show the relationship between bytecode
improvements and runtime performance gains.

Usage:
    python generate_report.py --test-file test_cases.aifpl
    python generate_report.py --comprehensive  # Run full suite
"""

import sys
import os
import subprocess
import json
import time
from pathlib import Path
from typing import Dict, List, Tuple
import argparse
from dataclasses import dataclass

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from aifpl.aifpl import AIFPL


@dataclass
class TestResult:
    """Result of a single test case."""
    name: str
    source: str

    # Bytecode metrics
    instructions_before: int
    instructions_after: int
    instruction_reduction_pct: float

    # Performance metrics (optional)
    time_before_ms: float = 0.0
    time_after_ms: float = 0.0
    speedup_pct: float = 0.0


class ReportGenerator:
    """Generate comprehensive optimization reports."""

    def __init__(self):
        self.aifpl = AIFPL()

    def parse_test_file(self, file_path: str) -> List[Tuple[str, str]]:
        """Parse test file into individual test cases."""
        with open(file_path, 'r') as f:
            content = f.read()

        # Split by section headers (lines starting with ; ====)
        sections = []
        current_section = []
        current_name = "Test"

        for line in content.split('\n'):
            if line.startswith('; ====='):
                # Save previous section
                if current_section:
                    code = '\n'.join(current_section).strip()
                    if code:
                        sections.append((current_name, code))
                current_section = []
            elif line.startswith(';') and not line.startswith('; ====='):
                # Extract test name from comment
                if 'SECTION' not in line and len(line) > 2:
                    current_name = line[2:].strip()
            elif line.strip() and not line.startswith(';'):
                current_section.append(line)

        # Add last section
        if current_section:
            code = '\n'.join(current_section).strip()
            if code:
                sections.append((current_name, code))

        return sections

    def benchmark_code(self, source: str, iterations: int = 1000) -> float:
        """Benchmark code execution time in milliseconds."""
        try:
            # Warmup
            for _ in range(10):
                self.aifpl.evaluate(source)

            # Measure
            start = time.perf_counter()
            for _ in range(iterations):
                self.aifpl.evaluate(source)
            end = time.perf_counter()

            # Return average time in milliseconds
            return ((end - start) / iterations) * 1000
        except Exception as e:
            print(f"Error benchmarking: {e}", file=sys.stderr)
            return 0.0

    def analyze_test_case(self, name: str, source: str, 
                         run_benchmarks: bool = False) -> TestResult:
        """Analyze a single test case."""
        from bytecode_analyzer import BytecodeAnalyzer

        analyzer = BytecodeAnalyzer()
        result = analyzer.compare(source)

        test_result = TestResult(
            name=name,
            source=source,
            instructions_before=result.unoptimized.total_instructions,
            instructions_after=result.optimized.total_instructions,
            instruction_reduction_pct=result.instruction_reduction_pct
        )

        if run_benchmarks:
            print(f"  Benchmarking {name}...", end='', flush=True)
            test_result.time_before_ms = self.benchmark_code(source)
            # TODO: When optimization flag is added, benchmark with optimization
            test_result.time_after_ms = test_result.time_before_ms  # Placeholder
            if test_result.time_before_ms > 0:
                time_saved = test_result.time_before_ms - test_result.time_after_ms
                test_result.speedup_pct = (time_saved / test_result.time_before_ms) * 100
            print(" done")

        return test_result

    def generate_markdown_report(self, results: List[TestResult]) -> str:
        """Generate a markdown report."""
        lines = []

        lines.append("# AIFPL Optimization Impact Report")
        lines.append("")
        lines.append(f"Generated: {time.strftime('%Y-%m-%d %H:%M:%S')}")
        lines.append("")

        # Summary statistics
        lines.append("## Summary")
        lines.append("")

        total_instructions_before = sum(r.instructions_before for r in results)
        total_instructions_after = sum(r.instructions_after for r in results)
        avg_reduction = sum(r.instruction_reduction_pct for r in results) / len(results)

        lines.append(f"- **Test Cases:** {len(results)}")
        lines.append(f"- **Total Instructions (Before):** {total_instructions_before}")
        lines.append(f"- **Total Instructions (After):** {total_instructions_after}")
        lines.append(f"- **Total Reduction:** {total_instructions_before - total_instructions_after} "
                    f"({((total_instructions_before - total_instructions_after) / total_instructions_before * 100):.1f}%)")
        lines.append(f"- **Average Reduction:** {avg_reduction:.1f}%")
        lines.append("")

        # Performance summary (if available)
        if any(r.time_before_ms > 0 for r in results):
            avg_speedup = sum(r.speedup_pct for r in results if r.speedup_pct > 0) / \
                         len([r for r in results if r.speedup_pct > 0])
            lines.append(f"- **Average Speedup:** {avg_speedup:.1f}%")
            lines.append("")

        # Detailed results
        lines.append("## Detailed Results")
        lines.append("")
        lines.append("| Test Case | Instructions Before | Instructions After | Reduction | Speedup |")
        lines.append("|-----------|--------------------:|-------------------:|----------:|--------:|")

        for result in results:
            speedup = f"{result.speedup_pct:.1f}%" if result.speedup_pct > 0 else "N/A"
            lines.append(f"| {result.name:<30} | {result.instructions_before:>6} | "
                        f"{result.instructions_after:>6} | "
                        f"{result.instruction_reduction_pct:>5.1f}% | {speedup:>6} |")

        lines.append("")

        # Visualization
        lines.append("## Instruction Reduction Visualization")
        lines.append("")
        lines.append("```")
        for result in results:
            bar_length = int(result.instruction_reduction_pct / 2)  # Scale to 50 chars max
            bar = "█" * bar_length
            lines.append(f"{result.name[:30]:<30} {bar} {result.instruction_reduction_pct:.1f}%")
        lines.append("```")
        lines.append("")

        # Correlation analysis (if benchmarks available)
        if any(r.time_before_ms > 0 for r in results):
            lines.append("## Bytecode vs Performance Correlation")
            lines.append("")
            lines.append("Comparing instruction reduction to performance improvement:")
            lines.append("")
            lines.append("| Test Case | Instruction Reduction | Speedup | Efficiency |")
            lines.append("|-----------|----------------------:|--------:|-----------:|")

            for result in results:
                if result.speedup_pct > 0:
                    efficiency = result.speedup_pct / result.instruction_reduction_pct \
                                if result.instruction_reduction_pct > 0 else 0
                    lines.append(f"| {result.name:<30} | {result.instruction_reduction_pct:>5.1f}% | "
                               f"{result.speedup_pct:>5.1f}% | {efficiency:>8.2f} |")

            lines.append("")
            lines.append("*Efficiency = Speedup % / Instruction Reduction %*")
            lines.append("")

        # Conclusions
        lines.append("## Conclusions")
        lines.append("")

        if avg_reduction > 20:
            lines.append(f"✓ **Excellent impact**: {avg_reduction:.1f}% average instruction reduction")
        elif avg_reduction > 10:
            lines.append(f"✓ **Good impact**: {avg_reduction:.1f}% average instruction reduction")
        else:
            lines.append(f"⚠ **Moderate impact**: {avg_reduction:.1f}% average instruction reduction")

        lines.append("")

        # Best performers
        best = max(results, key=lambda r: r.instruction_reduction_pct)
        lines.append(f"**Best optimization:** {best.name} ({best.instruction_reduction_pct:.1f}% reduction)")

        worst = min(results, key=lambda r: r.instruction_reduction_pct)
        lines.append(f"**Least optimization:** {worst.name} ({worst.instruction_reduction_pct:.1f}% reduction)")
        lines.append("")

        return "\n".join(lines)

    def generate_html_report(self, results: List[TestResult]) -> str:
        """Generate an HTML report with charts."""
        # Simple HTML report - could be enhanced with JavaScript charts
        html = """
<!DOCTYPE html>
<html>
<head>
    <title>AIFPL Optimization Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        h1 { color: #333; }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #4CAF50; color: white; }
        tr:nth-child(even) { background-color: #f2f2f2; }
        .bar { background-color: #4CAF50; height: 20px; margin: 2px 0; }
        .summary { background-color: #e7f3e7; padding: 15px; border-radius: 5px; }
    </style>
</head>
<body>
    <h1>AIFPL Optimization Impact Report</h1>
    <p>Generated: {timestamp}</p>

    <div class="summary">
        <h2>Summary</h2>
        <p><strong>Test Cases:</strong> {num_tests}</p>
        <p><strong>Average Instruction Reduction:</strong> {avg_reduction:.1f}%</p>
    </div>

    <h2>Detailed Results</h2>
    <table>
        <tr>
            <th>Test Case</th>
            <th>Instructions Before</th>
            <th>Instructions After</th>
            <th>Reduction %</th>
            <th>Visualization</th>
        </tr>
        {rows}
    </table>
</body>
</html>
"""

        avg_reduction = sum(r.instruction_reduction_pct for r in results) / len(results)

        rows = []
        for result in results:
            bar_width = int(result.instruction_reduction_pct * 3)  # Scale for display
            bar = f'<div class="bar" style="width: {bar_width}px;"></div>'
            rows.append(f"""
        <tr>
            <td>{result.name}</td>
            <td>{result.instructions_before}</td>
            <td>{result.instructions_after}</td>
            <td>{result.instruction_reduction_pct:.1f}%</td>
            <td>{bar}</td>
        </tr>
            """)

        return html.format(
            timestamp=time.strftime('%Y-%m-%d %H:%M:%S'),
            num_tests=len(results),
            avg_reduction=avg_reduction,
            rows='\n'.join(rows)
        )


def main():
    parser = argparse.ArgumentParser(description="Generate optimization impact report")
    parser.add_argument('--test-file', '-f', type=str, default='test_cases.aifpl',
                       help='Test file to analyze')
    parser.add_argument('--benchmark', '-b', action='store_true',
                       help='Include performance benchmarks (slower)')
    parser.add_argument('--output', '-o', type=str, default='report.md',
                       help='Output file (markdown)')
    parser.add_argument('--html', action='store_true',
                       help='Generate HTML report instead of markdown')

    args = parser.parse_args()

    generator = ReportGenerator()

    print(f"Analyzing test cases from {args.test_file}...")
    test_cases = generator.parse_test_file(args.test_file)
    print(f"Found {len(test_cases)} test cases\n")

    results = []
    for i, (name, source) in enumerate(test_cases, 1):
        print(f"[{i}/{len(test_cases)}] Analyzing: {name}")
        result = generator.analyze_test_case(name, source, run_benchmarks=args.benchmark)
        results.append(result)
        print(f"  Instructions: {result.instructions_before} → {result.instructions_after} "
              f"({result.instruction_reduction_pct:.1f}% reduction)")

    print(f"\nGenerating report...")

    if args.html:
        report = generator.generate_html_report(results)
        output_file = args.output.replace('.md', '.html')
    else:
        report = generator.generate_markdown_report(results)
        output_file = args.output

    with open(output_file, 'w') as f:
        f.write(report)

    print(f"Report saved to: {output_file}")
    print("\nSummary:")
    avg_reduction = sum(r.instruction_reduction_pct for r in results) / len(results)
    print(f"  Average instruction reduction: {avg_reduction:.1f}%")
    print(f"  Best case: {max(r.instruction_reduction_pct for r in results):.1f}%")
    print(f"  Worst case: {min(r.instruction_reduction_pct for r in results):.1f}%")


if __name__ == '__main__':
    main()
