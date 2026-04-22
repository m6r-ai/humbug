"""
reformat_braces.py — Reformat C control-flow statements to always use braces.

Rewrites all if/else/while/for statements in C source files so that:

  - Every body is enclosed in braces.
  - The opening brace cuddles the statement: if (cond) {
  - The body is on its own line, indented.
  - The closing brace is on its own line.
  - else / else if cuddles the closing brace: } else {
  - No code appears on the same line after an opening brace.
  - A blank line follows any lone '}' not immediately preceding another '}'.

Usage:
    python reformat_braces.py [--write] [file ...]

    Without --write, runs in dry-run mode: prints a unified diff for each file
    that would change, but makes no modifications.

    With --write, applies the changes in place.

    If no files are given, all *.c and *.h files under src/menai/ are processed.
"""

import argparse
import difflib
import os
import sys

# Add src/ to the path so we can use the syntax package.
_SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
_SRC_DIR = os.path.normpath(os.path.join(_SCRIPT_DIR, '..', '..', '..', 'src'))
sys.path.insert(0, _SRC_DIR)

from syntax.c.c_lexer import CLexer, CLexerState  # noqa: E402
from syntax.lexer import TokenType  # noqa: E402


# ---------------------------------------------------------------------------
# Token stream builder
# ---------------------------------------------------------------------------

def _tokenise_file(source: str):
    """
    Tokenise an entire C source file.

    Returns a list of (line_index, col, Token) triples where line_index is
    0-based and col is the byte offset within that line.
    """
    lines = source.splitlines(keepends=False)
    result = []
    lexer_state = None

    for line_idx, line in enumerate(lines):
        lexer = CLexer()
        lexer_state = lexer.lex(lexer_state, line)
        while True:
            tok = lexer.get_next_token()
            if tok is None:
                break
            result.append((line_idx, tok.start, tok))

    return result


def _skip_comments(tokens, i):
    """Advance i past any COMMENT tokens, returning the new index."""
    while i < len(tokens) and tokens[i][2].type == TokenType.COMMENT:
        i += 1
    return i


def _find_matching_paren(tokens, start_idx):
    """
    tokens[start_idx] must be a '(' operator.
    Returns the index of the matching ')'.  Returns -1 if not found.
    """
    depth = 0
    for i in range(start_idx, len(tokens)):
        _, _, tok = tokens[i]
        if tok.type == TokenType.OPERATOR:
            if tok.value == '(':
                depth += 1
            elif tok.value == ')':
                depth -= 1
                if depth == 0:
                    return i
    return -1


def _find_block_end(tokens, start_idx):
    """
    tokens[start_idx] must be a '{' operator.
    Returns the index of the matching '}'.  Returns -1 if not found.
    """
    depth = 0
    for i in range(start_idx, len(tokens)):
        _, _, tok = tokens[i]
        if tok.type == TokenType.OPERATOR:
            if tok.value == '{':
                depth += 1
            elif tok.value == '}':
                depth -= 1
                if depth == 0:
                    return i
    return -1


def _find_single_statement_end(tokens, start_idx):
    """
    Find the terminating ';' of a single (braceless) C statement starting at
    tokens[start_idx], respecting nested parens and braces.

    Returns the index of the ';', or -1 if not found.
    """
    depth_brace = 0
    depth_paren = 0

    for i in range(start_idx, len(tokens)):
        _, _, tok = tokens[i]
        if tok.type != TokenType.OPERATOR:
            continue
        if tok.value == '(':
            depth_paren += 1
        elif tok.value == ')':
            depth_paren -= 1
        elif tok.value == '{':
            depth_brace += 1
        elif tok.value == '}':
            depth_brace -= 1
        elif tok.value == ';' and depth_brace == 0 and depth_paren == 0:
            return i

    return -1


def _indent_of(line: str) -> str:
    """Return the leading whitespace of a line."""
    return line[: len(line) - len(line.lstrip())]


def _strip_trailing_comment(text: str) -> str:
    """
    Return text with any trailing // or /* comment stripped and whitespace
    trimmed.  Used to check whether there is real code after a '{'.
    """
    idx = text.find('//')
    if idx >= 0:
        text = text[:idx]
    idx = text.find('/*')
    if idx >= 0:
        text = text[:idx]
    return text.strip()


# ---------------------------------------------------------------------------
# Pass 1: Add missing braces to braceless if/while/for/else bodies.
#
# For if/while/for: looks for the keyword immediately followed by '(' and
# wraps a braceless body in braces.
# For else: wraps a braceless body that is not 'else if'.
#
# Only advances i past the body when registering a replacement (so that nested
# correctly-braced statements inside a correct outer statement are still
# visited).
# ---------------------------------------------------------------------------

def _add_missing_braces(source: str) -> str:
    """Add braces to braceless if/while/for/else bodies."""
    tokens = _tokenise_file(source)
    lines = source.splitlines(keepends=False)
    replacements = {}

    i = 0
    while i < len(tokens):
        line_idx, col, tok = tokens[i]

        if tok.type != TokenType.KEYWORD or tok.value not in ('if', 'while', 'for', 'else'):
            i += 1
            continue

        kw_line = line_idx

        # Handle braceless 'else' bodies (not 'else if').
        if tok.value == 'else':
            j = _skip_comments(tokens, i + 1)
            if j >= len(tokens):
                i += 1
                continue
            lj, cj, tj = tokens[j]
            # 'else if' — skip; the 'if' will be handled on the next iteration.
            if tj.type == TokenType.KEYWORD and tj.value == 'if':
                i += 1
                continue
            # Already braced — advance by 1 so nested statements are visited.
            if tj.type == TokenType.OPERATOR and tj.value == '{':
                i += 1
                continue
            # Braceless else body — wrap it.
            stmt_end_idx = _find_single_statement_end(tokens, j)
            if stmt_end_idx < 0:
                i += 1
                continue
            se_line, se_col, _ = tokens[stmt_end_idx]
            indent = _indent_of(lines[kw_line])
            body_indent = indent + '    '
            # Header is everything up to and including 'else' — the body may
            # follow on the same line so we must not include it in the header.
            header = lines[kw_line][:col + len('else')].rstrip()
            if lj == se_line:
                body_text = lines[lj][cj:se_col + 1].strip()
                new_lines = [header + ' {', body_indent + body_text, indent + '}']
            else:
                new_lines = [header + ' {']
                first_body = lines[lj][cj:].rstrip()
                if first_body.strip():
                    new_lines.append(body_indent + first_body.lstrip())
                for bl in range(lj + 1, se_line):
                    new_lines.append(lines[bl])
                last_body = lines[se_line][:se_col + 1].rstrip()
                if last_body.strip():
                    new_lines.append(body_indent + last_body.lstrip())
                new_lines.append(indent + '}')
            replacements[kw_line] = (se_line, new_lines)
            i = stmt_end_idx + 1
            continue

        # Must be immediately followed (ignoring comments) by '('.
        j = _skip_comments(tokens, i + 1)
        if j >= len(tokens):
            i += 1
            continue

        lj, cj, tj = tokens[j]
        if not (tj.type == TokenType.OPERATOR and tj.value == '('):
            i += 1
            continue

        close_paren_idx = _find_matching_paren(tokens, j)
        if close_paren_idx < 0:
            i += 1
            continue

        cp_line, cp_col, _ = tokens[close_paren_idx]

        k = _skip_comments(tokens, close_paren_idx + 1)
        if k >= len(tokens):
            i += 1
            continue

        lk, ck, tk = tokens[k]

        # Already braced — don't touch it here; pass 2 handles the body layout.
        # Advance by 1 so nested statements are visited.
        if tk.type == TokenType.OPERATOR and tk.value == '{':
            i += 1
            continue

        # Braceless body — add braces.
        stmt_end_idx = _find_single_statement_end(tokens, k)
        if stmt_end_idx < 0:
            i += 1
            continue

        se_line, se_col, _ = tokens[stmt_end_idx]
        indent = _indent_of(lines[kw_line])
        body_indent = indent + '    '
        header = lines[cp_line][:cp_col + 1].rstrip()

        if lk == se_line:
            body_text = lines[lk][ck:se_col + 1].strip()
            new_lines = [header + ' {', body_indent + body_text, indent + '}']
        else:
            new_lines = [header + ' {']
            first_body = lines[lk][ck:].rstrip()
            if first_body.strip():
                new_lines.append(body_indent + first_body.lstrip())
            for bl in range(lk + 1, se_line):
                new_lines.append(lines[bl])
            last_body = lines[se_line][:se_col + 1].rstrip()
            if last_body.strip():
                new_lines.append(body_indent + last_body.lstrip())
            new_lines.append(indent + '}')

        replacements[kw_line] = (se_line, new_lines)
        i = stmt_end_idx + 1

    if not replacements:
        return source

    result_lines = list(lines)
    for first in sorted(replacements.keys(), reverse=True):
        last, new_lines = replacements[first]
        result_lines[first:last + 1] = new_lines

    sep = '\r\n' if '\r\n' in source else '\n'
    result = sep.join(result_lines)
    if source.endswith('\n') or source.endswith('\r\n'):
        result += sep
    return result


# ---------------------------------------------------------------------------
# Pass 2: Expand single-line braced bodies.
#
# For every '{' token that is a block body (preceded by ')', 'else', or 'do')
# and has non-comment code after it on the same line, splits the body onto its
# own line and puts the closing '}' on its own line.
# ---------------------------------------------------------------------------

def _expand_single_line_bodies(source: str) -> str:
    """
    Expand any block-body '{' that has code after it on the same line, and
    ensure every matching '}' is on its own line.

    Only processes '{' tokens that are block bodies (preceded by ')', 'else',
    or 'do'), not struct/array initializers.
    """
    tokens = _tokenise_file(source)
    lines = source.splitlines(keepends=False)
    replacements = {}

    i = 0
    while i < len(tokens):
        line_idx, col, tok = tokens[i]

        if not (tok.type == TokenType.OPERATOR and tok.value == '{'):
            i += 1
            continue

        open_brace_line = line_idx
        open_brace_col = col

        # Only expand '{' that is a block body, not an initializer.
        # A block body '{' is preceded (ignoring comments) by ')' or by the
        # keywords 'else' or 'do'.
        prev_idx = i - 1
        while prev_idx >= 0 and tokens[prev_idx][2].type == TokenType.COMMENT:
            prev_idx -= 1
        if prev_idx < 0:
            i += 1
            continue
        prev_line, prev_col, prev_tok = tokens[prev_idx]
        is_block = (
            (prev_tok.type == TokenType.OPERATOR and prev_tok.value == ')') or
            (prev_tok.type == TokenType.KEYWORD and prev_tok.value in ('else', 'do'))
        )
        if not is_block:
            i += 1
            continue

        close_brace_idx = _find_block_end(tokens, i)
        if close_brace_idx < 0:
            i += 1
            continue

        cb_line, cb_col, _ = tokens[close_brace_idx]

        indent = _indent_of(lines[open_brace_line])

        # Check for code after '{' on the same line.
        rest_after_brace = _strip_trailing_comment(
            lines[open_brace_line][open_brace_col + 1:]
        )

        if rest_after_brace:
            body_indent = indent + '    '
            header = lines[open_brace_line][:open_brace_col + 1].rstrip()

            if open_brace_line == cb_line:
                body_text = lines[open_brace_line][open_brace_col + 1:cb_col].strip()
                after_close = _strip_trailing_comment(lines[cb_line][cb_col + 1:])
                new_lines = [header]
                if body_text:
                    new_lines.append(body_indent + body_text)
                new_lines.append(indent + '}')
                if after_close:
                    new_lines.append(indent + after_close)
                replacements[open_brace_line] = (cb_line, new_lines)
                i = close_brace_idx + 1
                continue
            else:
                new_lines = [header, body_indent + rest_after_brace]
                for bl in range(open_brace_line + 1, cb_line):
                    new_lines.append(lines[bl])
                after_close = _strip_trailing_comment(lines[cb_line][cb_col + 1:])
                new_lines.append(indent + '}')
                if after_close:
                    new_lines.append(indent + after_close)
                replacements[open_brace_line] = (cb_line, new_lines)
                i = close_brace_idx + 1
                continue

        # Nothing after '{' — check the closing '}'.
        if not lines[cb_line].strip().startswith('}'):
            before_close = lines[cb_line][:cb_col].rstrip()
            after_close = _strip_trailing_comment(lines[cb_line][cb_col + 1:])
            new_lines = []
            if before_close.strip():
                new_lines.append(before_close)
            new_lines.append(indent + '}')
            if after_close:
                new_lines.append(indent + after_close)
            replacements[cb_line] = (cb_line, new_lines)
            i = close_brace_idx + 1
            continue

        # Already correct — advance by 1 so nested blocks are visited.
        i += 1

    if not replacements:
        return source

    result_lines = list(lines)
    for first in sorted(replacements.keys(), reverse=True):
        last, new_lines = replacements[first]
        result_lines[first:last + 1] = new_lines

    sep = '\r\n' if '\r\n' in source else '\n'
    result = sep.join(result_lines)
    if source.endswith('\n') or source.endswith('\r\n'):
        result += sep
    return result


# ---------------------------------------------------------------------------
# Pass 3: Cuddle lone '}' with a following 'else'.
# ---------------------------------------------------------------------------

def _cuddle_else(source: str) -> str:
    """
    Merge a lone '}' line with a following 'else' line so that they cuddle:

        }           }
        else {  ->  } else {

    Only merges when the '}' line contains nothing but the closing brace
    (plus optional leading whitespace and a trailing // comment), and the
    next non-blank line begins with 'else' (after optional leading whitespace).

    Uses the token stream to avoid acting inside strings or block comments.
    """
    tokens = _tokenise_file(source)
    brace_only_lines = set()
    lines_with_tokens = {}
    for line_idx, col, tok in tokens:
        if tok.type != TokenType.COMMENT:
            lines_with_tokens.setdefault(line_idx, []).append(tok.value)
    for line_idx, values in lines_with_tokens.items():
        if values == ['}']:
            brace_only_lines.add(line_idx)

    lines = source.splitlines(keepends=False)
    result_lines = list(lines)
    # Walk backwards so index shifts from earlier merges don't affect later ones.
    for idx in range(len(lines) - 2, -1, -1):
        if idx not in brace_only_lines:
            continue
        # Find the next non-blank line.
        next_idx = idx + 1
        while next_idx < len(lines) and lines[next_idx].strip() == '':
            next_idx += 1
        if next_idx >= len(lines):
            continue
        next_stripped = lines[next_idx].lstrip()
        if not (next_stripped == 'else' or
                next_stripped.startswith('else ') or
                next_stripped.startswith('else{')):
            continue
        # Merge: replace the '}' line and the 'else' line (removing any blank
        # lines between them) with a single '} else ...' line.
        brace_line = lines[idx]
        else_content = lines[next_idx].lstrip()
        merged = brace_line.rstrip() + ' ' + else_content
        result_lines[idx:next_idx + 1] = [merged]

    if result_lines == list(lines):
        return source

    sep = '\r\n' if '\r\n' in source else '\n'
    result = sep.join(result_lines)
    if source.endswith('\n') or source.endswith('\r\n'):
        result += sep
    return result


# ---------------------------------------------------------------------------
# Pass 4: Insert blank line after lone '}' not followed by another '}'.
# ---------------------------------------------------------------------------

def _blank_after_close(source: str) -> str:
    """
    Insert a blank line after any lone '}' line that is not immediately
    followed by another closing brace, a blank line, or end-of-file.

    A lone '}' line is one whose only non-whitespace content is '}'.
    The following are not given a blank line:
      - The next line is already blank.
      - The next line starts with '}' (closing brace cascade).
      - The '}' is the last line of the file.

    Uses the token stream to identify genuine '}' lines (not '}' inside
    strings or block comments).
    """
    tokens = _tokenise_file(source)
    lines_with_tokens = {}
    for line_idx, col, tok in tokens:
        if tok.type != TokenType.COMMENT:
            lines_with_tokens.setdefault(line_idx, []).append(tok.value)
    brace_only_lines = {
        line_idx for line_idx, values in lines_with_tokens.items()
        if values == ['}']
    }

    lines = source.splitlines(keepends=False)
    result_lines = []
    for idx, line in enumerate(lines):
        result_lines.append(line)
        if idx not in brace_only_lines:
            continue
        if idx == len(lines) - 1:
            continue
        next_stripped = lines[idx + 1].lstrip()
        # Don't insert if next line is blank or starts with '}'.
        if next_stripped == '' or next_stripped.startswith('}'):
            continue
        result_lines.append('')

    if result_lines == list(lines):
        return source

    sep = '\r\n' if '\r\n' in source else '\n'
    result = sep.join(result_lines)
    if source.endswith('\n') or source.endswith('\r\n'):
        result += sep
    return result


def reformat_source(source: str) -> str:
    """
    Reformat a C source string so that all if/else/while/for bodies use braces
    and no code appears on the same line after an opening brace.

    Runs four passes iteratively until the output stabilises:
      1. Expand any '{' that has code after it on the same line.
      2. Add missing braces to braceless if/while/for/else bodies.
      3. Cuddle any lone '}' line with a following 'else' line.
      4. Insert a blank line after lone '}' not followed by another '}'.

    Returns the reformatted source (identical to input if no changes needed).
    """
    for _ in range(20):  # safety limit — converges in practice within a few passes
        after_expand = _expand_single_line_bodies(source)
        after_braces = _add_missing_braces(after_expand)
        after_cuddle = _cuddle_else(after_braces)
        after_blank = _blank_after_close(after_cuddle)
        if after_blank == source:
            break
        source = after_blank
    return source


def _make_diff(original: str, reformatted: str, filename: str) -> str:
    """Return a unified diff string, or empty string if no changes."""
    orig_lines = original.splitlines(keepends=True)
    new_lines = reformatted.splitlines(keepends=True)
    diff = list(difflib.unified_diff(
        orig_lines, new_lines,
        fromfile=f'a/{filename}',
        tofile=f'b/{filename}',
    ))
    return ''.join(diff)


def _default_files():
    """Return the default list of C source files to process."""
    menai_src = os.path.normpath(
        os.path.join(_SCRIPT_DIR, '..', '..', '..', 'src', 'menai')
    )
    result = []
    for name in sorted(os.listdir(menai_src)):
        if name.endswith('.c') or name.endswith('.h'):
            result.append(os.path.join(menai_src, name))
    return result


def main():
    parser = argparse.ArgumentParser(
        description='Reformat C control-flow statements to always use braces.'
    )
    parser.add_argument(
        '--write', action='store_true',
        help='Apply changes in place (default: dry-run, print diffs only).'
    )
    parser.add_argument(
        'files', nargs='*',
        help='C source files to process (default: all *.c and *.h in src/menai/).'
    )
    args = parser.parse_args()

    files = args.files if args.files else _default_files()

    any_changes = False
    for path in files:
        with open(path, 'r', encoding='utf-8') as f:
            original = f.read()

        reformatted = reformat_source(original)

        if reformatted == original:
            continue

        any_changes = True
        filename = os.path.relpath(path)
        diff = _make_diff(original, reformatted, filename)

        if args.write:
            with open(path, 'w', encoding='utf-8') as f:
                f.write(reformatted)
            print(f'Reformatted: {filename}')
        else:
            print(diff, end='')

    if not any_changes:
        print('No changes needed.')

    sys.exit(0)


if __name__ == '__main__':
    main()
