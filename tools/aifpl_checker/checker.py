"""AIFPL Parenthesis Balance Checker - validates paren balance in AIFPL files."""

import sys
import argparse
from pathlib import Path
from dataclasses import dataclass
from typing import List, Tuple, Optional

from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_token import AIFPLTokenType
from aifpl.aifpl_error import AIFPLTokenError


class Colors:
    """ANSI color codes for terminal output."""

    # Reset
    RESET = '\033[0m'

    # Basic colors
    BLACK = '\033[30m'
    RED = '\033[31m'
    GREEN = '\033[32m'
    YELLOW = '\033[33m'
    BLUE = '\033[34m'
    MAGENTA = '\033[35m'
    CYAN = '\033[36m'
    WHITE = '\033[37m'

    # Bright colors
    BRIGHT_BLACK = '\033[90m'
    BRIGHT_RED = '\033[91m'
    BRIGHT_GREEN = '\033[92m'
    BRIGHT_YELLOW = '\033[93m'
    BRIGHT_BLUE = '\033[94m'
    BRIGHT_MAGENTA = '\033[95m'
    BRIGHT_CYAN = '\033[96m'
    BRIGHT_WHITE = '\033[97m'

    # Styles
    BOLD = '\033[1m'
    DIM = '\033[2m'

    @staticmethod
    def colorize(text: str, color: str) -> str:
        """Wrap text in color codes."""
        return f"{color}{text}{Colors.RESET}"

    @staticmethod
    def get_form_color(form_type: str) -> str:
        """Get color for a specific form type."""
        return getattr(Colors, f'FORM_{form_type.upper()}', Colors.WHITE)


@dataclass
class FormStackFrame:
    """Tracks an opened special form for annotation."""
    open_line: int
    depth_when_opened: int  # Depth after the opening paren
    form_type: str  # 'lambda', 'let', 'letrec', 'if', 'match'
    name: Optional[str] = None  # Optional identifier (e.g., function name)


@dataclass
class ParenPosition:
    """Position of a parenthesis for color-coding."""
    line: int
    column: int  # 1-indexed
    is_open: bool
    depth: int  # Depth after this paren


@dataclass
class CloseAnnotation:
    """Annotation for a closing parenthesis."""
    form_type: str  # 'lambda', 'let', etc.
    depth: int  # Depth of the paren being closed


@dataclass
class LineInfo:
    """Information about a line in the source file."""
    close_annotations: List[CloseAnnotation]  # Annotations for closing parens
    paren_positions: List[ParenPosition]  # Positions of parens on this line
    line_num: int
    depth: int  # Depth at end of line
    content: str
    has_open: bool = False
    has_close: bool = False
    start_depth: int = 0  # Depth at start of line (for line number coloring)


@dataclass
class ParenError:
    """Information about a parenthesis balance error."""
    line_num: int
    depth: int
    error_type: str  # "negative_depth" or "unclosed"
    message: str


class ParenChecker:
    """Checks parenthesis balance in AIFPL files."""

    # Special forms to track (Tier 1)
    TRACKED_FORMS = {'lambda', 'let', 'letrec', 'if', 'match'}

    # Color palette for depth-based paren coloring (cycles through these)
    PAREN_COLORS = [Colors.CYAN, Colors.YELLOW, Colors.GREEN, Colors.MAGENTA, Colors.BLUE, Colors.RED]

    def __init__(self, filepath: str):
        """
        Initialize checker with file path.

        Args:
            filepath: Path to AIFPL file to check
        """
        self.filepath = Path(filepath)
        self.lines: List[str] = []
        self.line_info: List[LineInfo] = []
        self.errors: List[ParenError] = []
        self.total_opens = 0
        self.total_closes = 0
        self.max_depth = 0
        self.form_stack: List[FormStackFrame] = []  # Stack for tracking special forms

    def get_paren_color(self, depth: int) -> str:
        """Get color for a paren at given depth (cycles through palette)."""
        return self.PAREN_COLORS[depth % len(self.PAREN_COLORS)]

    def colorize_parens(self, code: str, paren_positions: List[ParenPosition]) -> str:
        """
        Colorize parentheses in code based on depth.

        Args:
            code: Original code line
            paren_positions: List of paren positions on this line

        Returns:
            Code with colored parens
        """
        if not paren_positions:
            return code

        # Sort by column (reverse) so we can insert from right to left
        sorted_positions = sorted(paren_positions, key=lambda p: p.column, reverse=True)

        result = code
        for paren_pos in sorted_positions:
            col_idx = paren_pos.column - 1  # Convert to 0-indexed
            if col_idx < 0 or col_idx >= len(result):
                continue

            paren_char = result[col_idx]
            if paren_char not in '()':
                continue

            # Use depth before the paren for opens, depth after for closes
            depth_for_color = paren_pos.depth - 1 if paren_pos.is_open else paren_pos.depth
            color = self.get_paren_color(depth_for_color)
            colored_paren = Colors.colorize(paren_char, color)
            result = result[:col_idx] + colored_paren + result[col_idx + 1:]

        return result

    def load_file(self) -> str:
        """
        Load file contents.

        Returns:
            File contents as string

        Raises:
            FileNotFoundError: If file doesn't exist
            IOError: If file can't be read
        """
        if not self.filepath.exists():
            raise FileNotFoundError(f"File not found: {self.filepath}")

        with open(self.filepath, 'r', encoding='utf-8') as f:
            content = f.read()

        self.lines = content.split('\n')
        return content

    def check_balance(self) -> bool:
        """
        Check if parentheses are balanced.

        Returns:
            True if balanced, False otherwise
        """
        try:
            content = self.load_file()

        except (FileNotFoundError, IOError) as e:
            print(f"Error reading file: {e}", file=sys.stderr)
            return False

        # Lex the file
        lexer = AIFPLLexer()
        try:
            tokens = lexer.lex(content)

        except AIFPLTokenError as e:
            print(f"Tokenization error: {e}", file=sys.stderr)
            return False

        # Initialize line tracking
        self.line_info = [
            LineInfo(line_num=i + 1, depth=0, content=line, close_annotations=[], paren_positions=[])
            for i, line in enumerate(self.lines)
        ]

        # Track depth through tokens
        current_depth = 0
        last_line = 0
        for i, token in enumerate(tokens):
            line_idx = token.line - 1  # Convert to 0-indexed

            # Track start depth for this line (depth before first token on line)
            if token.line != last_line:
                if line_idx < len(self.line_info):
                    self.line_info[line_idx].start_depth = current_depth
                last_line = token.line

            if token.type == AIFPLTokenType.LPAREN:
                self.total_opens += 1
                current_depth += 1
                if line_idx < len(self.line_info):
                    self.line_info[line_idx].has_open = True

                # Check if next token is a tracked special form
                if i + 1 < len(tokens):
                    next_token = tokens[i + 1]
                    if (next_token.type == AIFPLTokenType.SYMBOL and
                        next_token.value in self.TRACKED_FORMS):
                        # Push form onto stack
                        self.form_stack.append(FormStackFrame(
                            open_line=token.line,
                            depth_when_opened=current_depth,
                            form_type=next_token.value
                        ))

                # Track paren position
                if line_idx < len(self.line_info):
                    self.line_info[line_idx].paren_positions.append(ParenPosition(
                        line=token.line, column=token.column, is_open=True, depth=current_depth
                    ))

            elif token.type == AIFPLTokenType.RPAREN:
                self.total_closes += 1
                current_depth -= 1
                if line_idx < len(self.line_info):
                    self.line_info[line_idx].has_close = True

                # Pop form from stack and annotate
                # Only pop if this closing paren matches the depth of a tracked form
                if self.form_stack and current_depth + 1 == self.form_stack[-1].depth_when_opened:
                    frame = self.form_stack.pop()
                    if line_idx < len(self.line_info):
                        if self.line_info[line_idx].close_annotations is None:
                            self.line_info[line_idx].close_annotations = []
                        self.line_info[line_idx].close_annotations.append(CloseAnnotation(
                            form_type=frame.form_type,
                            depth=current_depth  # Depth after closing (which was the depth when opened - 1)
                        ))

                # Track paren position
                if line_idx < len(self.line_info):
                    self.line_info[line_idx].paren_positions.append(ParenPosition(
                        line=token.line, column=token.column, is_open=False, depth=current_depth
                    ))

                # Check for negative depth
                if current_depth < 0:
                    self.errors.append(ParenError(
                        line_num=token.line,
                        depth=current_depth,
                        error_type="negative_depth",
                        message="Extra closing parenthesis (depth went negative)"
                    ))

            # Update depth for this line
            if line_idx < len(self.line_info):
                self.line_info[line_idx].depth = current_depth

            # Track max depth
            if current_depth > self.max_depth:
                self.max_depth = current_depth

        # Check final depth
        if current_depth > 0:
            self.errors.append(ParenError(
                line_num=len(self.lines),
                depth=current_depth,
                error_type="unclosed",
                message=f"Missing {current_depth} closing parenthes{'is' if current_depth == 1 else 'es'}"
            ))

        # Fill in depth for lines without tokens (empty lines, comment-only lines)
        # by propagating depth from previous lines
        for i, line in enumerate(self.line_info):
            # If this line has no tokens (depth and start_depth are both 0 from init),
            # inherit from previous line
            if i > 0 and line.depth == 0 and line.start_depth == 0 and not line.has_open and not line.has_close:
                prev_line = self.line_info[i - 1]
                line.start_depth = prev_line.depth
                line.depth = prev_line.depth

            # Special case: first line with no tokens stays at depth 0
            # (already initialized correctly)

        return len(self.errors) == 0

    def get_error_context_lines(self, context_size: int = 5) -> List[int]:
        """
        Get line numbers around errors for focused display.

        Args:
            context_size: Number of lines before/after error to include

        Returns:
            List of line numbers to display
        """
        if not self.errors:
            return []

        line_set: set[int] = set()
        for error in self.errors:
            start = max(1, error.line_num - context_size)
            end = min(len(self.lines), error.line_num + context_size)
            line_set.update(range(start, end + 1))

        return sorted(line_set)

    def format_depth_chart(
        self,
        line_range: Optional[Tuple[int, int]] = None,
        show_all: bool = False,
        annotate: bool = False,
        color: bool = False
    ) -> str:
        """
        Format depth chart for display.

        Args:
            line_range: Optional (start, end) line numbers (1-indexed, inclusive)
            show_all: Show all lines regardless of errors
            annotate: Show form type annotations for closing parens
            color: Use ANSI colors for output

        Returns:
            Formatted depth chart string
        """
        if not self.line_info:
            return "No line information available"

        lines_to_show: List[int] = []

        # Determine which lines to show
        if line_range:
            start, end = line_range
            lines_to_show = list(range(start, min(end + 1, len(self.line_info) + 1)))

        elif show_all:
            lines_to_show = list(range(1, len(self.line_info) + 1))

        else:
            # Auto-focus on errors
            error_lines = self.get_error_context_lines()
            if error_lines:
                lines_to_show = error_lines

            else:
                # No errors, show nothing (summary only)
                return ""

        # Build chart
        lines = []
        lines.append("Line | Depth | Code")
        lines.append("-----|-------|" + "-" * 50)

        for line_num in lines_to_show:
            if line_num > len(self.line_info):
                break

            info = self.line_info[line_num - 1]
            code = info.content

            # Colorize parens if color is enabled
            if color and info.paren_positions:
                code = self.colorize_parens(code, info.paren_positions)

            # Check if this line has an error
            error_marker = ""
            for error in self.errors:
                if error.line_num == line_num:
                    error_text = f"  <-- ERROR: {error.message}"
                    error_marker = Colors.colorize(error_text, Colors.BRIGHT_RED) if color else error_text
                    break

            # Add annotations if enabled and present
            annotation = ""
            if annotate and info.close_annotations:
                if color:
                    # Color each annotation based on the depth of the paren being closed
                    colored_annotations = []
                    for ann in info.close_annotations:
                        ann_text = f"closes {ann.form_type}"
                        # Use the color of the paren being closed (based on its depth)
                        paren_color = self.get_paren_color(ann.depth)
                        colored_annotations.append(Colors.colorize(ann_text, paren_color))
                    annotation = "  " + Colors.colorize("<--", Colors.DIM) + " " + ", ".join(colored_annotations)

                else:
                    ann_texts = [f"closes {ann.form_type}" for ann in info.close_annotations]
                    annotation = "  <-- " + ", ".join(ann_texts)

            # Format line number with color matching the depth at start of line
            if color:
                # Color line number based on the depth at the start of the line
                # This shows which paren context we're in
                if info.start_depth > 0:
                    line_color = self.get_paren_color(info.start_depth - 1)

                else:
                    line_color = Colors.DIM
                line_num_str = Colors.colorize(f"{line_num:4d}", line_color)

            else:
                line_num_str = f"{line_num:4d}"

            line_display = f"{line_num_str} | {info.start_depth:5d} | {code}{annotation}{error_marker}"
            lines.append(line_display)

        return "\n".join(lines)

    def format_summary(self) -> str:
        """
        Format summary of balance check.

        Returns:
            Formatted summary string
        """
        if len(self.errors) == 0:
            return (
                f"✓ Parentheses balanced in {self.filepath.name}\n"
                f"  Total: {self.total_opens} opens, {self.total_closes} closes\n"
                f"  Maximum depth: {self.max_depth}"
            )

        else:
            imbalance = self.total_opens - self.total_closes
            if imbalance > 0:
                detail = f"Missing {imbalance} closing parenthes{'is' if imbalance == 1 else 'es'}"

            elif imbalance < 0:
                detail = f"Extra {-imbalance} closing parenthes{'is' if imbalance == -1 else 'es'}"

            else:
                detail = "Depth errors (parens in wrong order)"

            return (
                f"✗ Parentheses UNBALANCED in {self.filepath.name}\n"
                f"  Total: {self.total_opens} opens, {self.total_closes} closes\n"
                f"  {detail}"
            )

    def print_report(
        self,
        line_range: Optional[Tuple[int, int]] = None,
        summary_only: bool = False,
        annotate: bool = False,
        color: bool = False
    ) -> None:
        """
        Print full report to stdout.

        Args:
            line_range: Optional (start, end) line numbers
            summary_only: Only show summary, no depth chart
            annotate: Show form type annotations for closing parens
            color: Use ANSI colors for output
        """
        print(self.format_summary())

        try:
            # Show depth chart unless summary_only is True
            if not summary_only:
                print()
                # Pass show_all=True to show all lines (new default behavior)
                depth_chart = self.format_depth_chart(line_range, show_all=True, annotate=annotate, color=color)
                if depth_chart:
                    print(depth_chart)

            # Show error details
            if self.errors:
                print()
                for error in self.errors:
                    if error.error_type == "negative_depth":
                        print(f"Unmatched closing parenthesis at line {error.line_num}")
                    elif error.error_type == "unclosed":
                        print("Unclosed expressions at end of file")

        finally:
            # Always reset colors at the end
            if color:
                print(Colors.RESET, end='')


def parse_line_range(range_str: str) -> Tuple[int, int]:
    """
    Parse line range string.

    Args:
        range_str: Range string like "100-200", "100-", or "-200"

    Returns:
        Tuple of (start, end) line numbers

    Raises:
        ValueError: If range string is invalid
    """
    if '-' not in range_str:
        raise ValueError("Line range must contain '-' (e.g., '100-200', '100-', '-200')")

    parts = range_str.split('-', 1)

    if not parts[0]:  # "-200" format
        start = 1
        end = int(parts[1])

    elif not parts[1]:  # "100-" format
        start = int(parts[0])
        end = 999999999  # Will be clamped to file length

    else:  # "100-200" format
        start = int(parts[0])
        end = int(parts[1])

    if start < 1:
        raise ValueError("Start line must be >= 1")

    if end < start:
        raise ValueError("End line must be >= start line")

    return (start, end)


def main() -> int:
    """Main entry point for CLI."""
    parser = argparse.ArgumentParser(
        description="Check parenthesis balance in AIFPL files",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s file.aifpl                    # Basic check
  %(prog)s file.aifpl -l 100-200         # Check with line range
  %(prog)s file.aifpl -s                 # Summary only
  %(prog)s file.aifpl -a -c              # With colored annotations
        """
    )

    parser.add_argument(
        'file',
        help='AIFPL file to check'
    )

    parser.add_argument(
        '-l', '--lines',
        type=str,
        help='Line range to display (e.g., "100-200", "100-", "-200")'
    )

    parser.add_argument(
        '-s', '--summary-only',
        action='store_true',
        help='Only show summary without depth chart'
    )

    parser.add_argument(
        '-a', '--annotate',
        action='store_true',
        help='Annotate closing parens with form types (lambda, let, etc.)'
    )

    parser.add_argument(
        '-c', '--color',
        action='store_true',
        help='Use ANSI colors for output (annotations, line numbers, errors)'
    )

    args = parser.parse_args()

    # Parse line range if provided
    line_range = None
    if args.lines:
        try:
            line_range = parse_line_range(args.lines)

        except ValueError as e:
            print(f"Error: Invalid line range: {e}", file=sys.stderr)
            return 2

    # Check file
    checker = ParenChecker(args.file)
    is_balanced = checker.check_balance()

    # Print report
    checker.print_report(
        line_range=line_range,
        summary_only=args.summary_only,
        annotate=args.annotate,
        color=args.color
    )

    # Exit with appropriate code
    return 0 if is_balanced else 1


if __name__ == '__main__':
    sys.exit(main())
