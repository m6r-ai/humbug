# Action: Review code

## 1. Summary of How the Software Works

The file `/Users/dave/github/m6r/humbug/src/humbug/markdown/markdown_parser.py` implements a Markdown parser in Python. Its main purpose is to parse Markdown text and build an Abstract Syntax Tree (AST) representing the structure and formatting of the document.

High-level overview:
- **AST Construction:** The parser analyzes Markdown source line by line, classifies each line (heading, list item, paragraph, code block, table row, etc.), and builds corresponding AST nodes.
- **Inline Formatting:** The parser recognizes and processes inline elements (bold, italics, links, images, inline code) using a recursive descent approach.
- **Block Elements:** Handles block-level elements: headings, lists (ordered/unordered), code blocks (with language detection and possible nesting), tables (with buffering and alignment), horizontal rules, and paragraphs.
- **State Tracking:** Maintains various parsing state machines for lists, tables, code blocks, and paragraphs to accurately reflect Markdown’s flexible syntax.
- **Line Mapping:** Associates parsed AST nodes with line numbers for incremental updates and error reporting.
- **Incremental Parsing:** Provides an `update_ast` method for incremental re-parsing, though currently this mostly falls back to a full re-parse.
- **Error Handling:** Uses custom exceptions to signal parsing errors, with logging for debugging.

## 2. File-by-File Review

### File: `/Users/dave/github/m6r/humbug/src/humbug/markdown/markdown_parser.py`

#### Review & Recommendations

Below are detailed review items, each with:

- **Description of the issue or suggestion**
- **Relevant guideline(s)**
- **Suggested code change** (if applicable)
- **Ranking of importance (1-4)**

---

#### 2.1. **Type Hint Consistency and Python Version Compatibility**

- **Issue:**  
  Uses PEP 604 (`str | None`) type hints, which require Python 3.10+. If the codebase is intended to support earlier Python versions (3.7–3.9), this will break compatibility. PEP 484 (`Optional[str]`) is more widely supported.
- **Guideline:**  
  - *Python Code Style and Readability*: Use type hints consistently.
  - *Extra Questions*: Is the code compliant with relevant regulations or standards?
- **Suggested Change:**  
  If Python >= 3.10 is required, document this clearly. Otherwise, use `Optional[str]` from `typing`.
- **Ranking:** 2 (Major) if compatibility is required, 4 (Observation) if 3.10+ is mandated.

**Example:**
```python
from typing import Optional

self._source_path: Optional[str] = None
```

---

#### 2.2. **Docstring Completeness and Examples**

- **Issue:**  
  Many methods have good docstrings, but some (especially private methods or those with tricky logic, e.g., `_handle_text_continuation`, `_convert_list_items_to_paragraphs`, `_handle_incomplete_table`) lack usage examples. Complex inline parsing could use more explanation.
- **Guideline:**  
  - *Python Code Style and Readability*: Have docstrings for modules, classes, and functions. Include examples in docstrings for complex functionality.
- **Suggested Change:**  
  Add examples to docstrings for complex methods (e.g., `parse_inline_formatting`, `identify_line_type`).
- **Ranking:** 3 (Minor)

**Example:**
```python
def parse_inline_formatting(self, text: str) -> List[MarkdownASTNode]:
    """
    Parse inline formatting (bold, italic, inline code) in text and create appropriate AST nodes.

    Example:
        Input: "This is **bold** and _italic_"
        Output: [
            MarkdownTextNode("This is "),
            MarkdownBoldNode([MarkdownTextNode("bold")]),
            MarkdownTextNode(" and "),
            MarkdownEmphasisNode([MarkdownTextNode("italic")])
        ]
    ...
    """
```

---

#### 2.3. **Exception Handling Granularity**

- **Issue:**  
  In `parse_line`, a blanket `except Exception as e` is used, which is generally discouraged as it can mask system exit, keyboard interrupts, etc.
- **Guideline:**  
  - *Error Handling*: Detect and handle all exception or failure conditions. Use specific exceptions.
- **Suggested Change:**  
  Catch more specific exceptions, or at minimum, avoid catching `BaseException`.
- **Ranking:** 2 (Major)

**Example:**
```python
try:
    ...
except (ValueError, AttributeError, SomeCustomException) as e:
    ...
```

---

#### 2.4. **Logging Usage**

- **Issue:**  
  Logging is used for error reporting, but the logger is named `"ASTBuilder"`, which may be outdated or misleading if the class is now called `MarkdownParser`. Also, logging is only used for exceptions.
- **Guideline:**  
  - *Error Handling*: Log errors appropriately.
  - *Code Organization*: Maintain consistent naming conventions.
- **Suggested Change:**  
  Rename logger to `"MarkdownParser"` and consider adding debug logs for major parsing events (start/end of code block, table, etc.).
- **Ranking:** 4 (Observation)

---

#### 2.5. **Complexity in `parse_inline_formatting`**

- **Issue:**  
  The method is large, deeply nested, and manually implements a state machine for inline parsing. It is difficult to read and maintain, and could be decomposed for clarity.
- **Guideline:**  
  - *Code Smells*: Large functions, deep nesting, complex boolean logic.
  - *Code Organization*: Keep functions focused and under 50 lines.
  - *KISS*: Strive for simplicity.
- **Suggested Change:**  
  Break out each inline element (image, link, code, bold, italic) parsing into helper methods. Consider using a parser combinator, or at least flatten the function.
- **Ranking:** 2 (Major)

**Example Refactor (outline):**
```python
def parse_inline_formatting(self, text: str) -> List[MarkdownASTNode]:
    # ... state machine loop ...
    if self._is_image_start(text, i):
        node, new_i = self._parse_image(text, i)
        nodes.append(node)
        i = new_i
        continue
    # ... etc ...
```

---

#### 2.6. **Mutable Default Arguments**

- **Issue:**  
  None detected in this file, but ensure future functions never use mutable defaults. (Currently, all mutable state is on `self` or explicitly initialized.)
- **Guideline:**  
  - *Python Code Style and Readability*: Avoid using mutable default arguments in function definitions.
- **Suggested Change:**  
  No change needed.
- **Ranking:** 4 (Observation)

---

#### 2.7. **Use of Magic Numbers/Strings**

- **Issue:**  
  Some constants (e.g., regex patterns, indentation values, list marker lengths) are sprinkled in the code. Magic numbers like "2" for marker lengths are used directly.
- **Guideline:**  
  - *Code Smells*: Magic numbers/strings.
- **Suggested Change:**  
  Define named constants for such values at the top of the file or within the class.
- **Ranking:** 3 (Minor)

**Example:**
```python
LIST_MARKER_DOT_SPACE_LENGTH = 2
...
current_list.marker_length = len(number) + LIST_MARKER_DOT_SPACE_LENGTH
```

---

#### 2.8. **Repetition in Table Parsing**

- **Issue:**  
  Table row handling and cell processing logic is duplicated for headers and body rows.
- **Guideline:**  
  - *DRY*: Eliminate code duplication.
- **Suggested Change:**  
  Abstract table row/cell creation into a helper method.
- **Ranking:** 3 (Minor)

---

#### 2.9. **Performance: Inefficient Line Parsing in `update_ast`**

- **Issue:**  
  The incremental update logic computes a diff but always falls back to a full re-parse. This is inefficient for large documents.
- **Guideline:**  
  - *Performance Considerations*: Be mindful of the time complexity of operations on large datasets.
- **Suggested Change:**  
  Implement incremental AST update for changed regions only, if possible.
- **Ranking:** 3 (Minor), but could be 2 (Major) in performance-critical apps.

---

#### 2.10. **Security: Input Validation and Injection**

- **Issue:**  
  No direct user input is being executed, but in case the parser is exposed via an API, ensure Markdown is sanitized before rendering or executing any output.
- **Guideline:**  
  - *Security Best Practices*: Sanitize all user inputs.
- **Suggested Change:**  
  Add usage notes or comments warning downstream users not to trust rendered Markdown or AST node content.
- **Ranking:** 4 (Observation) unless this code is part of a web server, in which case higher.

---

#### 2.11. **Use of `Any` in Typing**

- **Issue:**  
  The `identify_line_type` method returns `Tuple[str, Any]`, which is not type-safe. This makes it hard to reason about downstream code.
- **Guideline:**  
  - *Python Code Style and Readability*: Use type hints for function arguments and return values.
  - *Code Smells*: Primitive obsession.
- **Suggested Change:**  
  Define a `NamedTuple` or `TypedDict` for return values.
- **Ranking:** 3 (Minor)

**Example:**
```python
from typing import NamedTuple, Optional, Union

class LineTypeResult(NamedTuple):
    line_type: str
    content: Optional[Union[str, tuple]]
```

---

#### 2.12. **Long Functions**

- **Issue:**  
  Several functions (notably `parse_line`, `parse_inline_formatting`, `build_ast`) exceed 50 lines.
- **Guideline:**  
  - *Python Code Style and Readability*: Keep functions focused and under 50 lines when possible.
- **Suggested Change:**  
  Break long functions into smaller logical units.
- **Ranking:** 3 (Minor)

---

#### 2.13. **Error Message Exposure**

- **Issue:**  
  Error messages in `MarkdownParserError` may include input lines, which could expose sensitive information depending on the context.
- **Guideline:**  
  - *Error Handling*: Ensure error messages do not expose sensitive information.
- **Suggested Change:**  
  If used in sensitive environments, redact or sanitize error messages.
- **Ranking:** 4 (Observation)

---

#### 2.14. **Separation of Concerns**

- **Issue:**  
  The parser mixes AST construction, parsing, and some document management (line mapping, etc.) in a single class.
- **Guideline:**  
  - *Code Organization*: Separate concerns (business logic, data access, presentation).
- **Suggested Change:**  
  Consider splitting into a parser, an AST builder, and a document manager if the project grows.
- **Ranking:** 4 (Observation)

---

#### 2.15. **Use of `cast`**

- **Issue:**  
  Frequent use of `cast` hints at places where type inference or better structure could avoid casts.
- **Guideline:**  
  - *Python Code Style and Readability*: Use type hints consistently and avoid type casts where possible.
- **Suggested Change:**  
  Restructure code to reduce the need for casting.
- **Ranking:** 4 (Observation)

---

#### 2.16. **Unit Tests**

- **Issue:**  
  No unit tests are included in this file, nor is their presence mentioned. Ensure comprehensive test coverage, especially for edge cases.
- **Guideline:**  
  - *Extra Questions*: Is the error handling comprehensive? Would another developer be able to maintain this?
- **Suggested Change:**  
  Add or reference unit tests for all parsing logic.
- **Ranking:** 1 (Critical) if missing entirely; 4 (Observation) if covered elsewhere.

---

#### 2.17. **Explicit Imports for Used Classes**

- **Issue:**  
  All AST node classes are imported as a block, which is fine, but if not all are used, could bloat the module.
- **Guideline:**  
  - *YAGNI*: Avoid adding functionality until it is necessary.
- **Suggested Change:**  
  Only import what is used, or at least add a comment if the full import is intentional for extensibility.
- **Ranking:** 4 (Observation)

---

#### 2.18. **Line Length**

- **Issue:**  
  Most lines are <132 chars as recommended, but a few approach this limit, especially in docstrings and argument lists.
- **Guideline:**  
  - *Python Code Style and Readability*: Limit line length to less than 132 characters.
- **Suggested Change:**  
  Break up long lines for readability.
- **Ranking:** 4 (Observation)

---

## 3. Example Refactor: `parse_inline_formatting`

**Current:**
A single large method with nested logic for all inline parsing.

**Suggested:**
Break into smaller helper functions for each inline type.

```python
def parse_inline_formatting(self, text: str) -> List[MarkdownASTNode]:
    """
    Parse inline formatting (bold, italic, inline code) in text and create appropriate AST nodes.
    """
    i = 0
    nodes = []
    while i < len(text):
        if self._is_image_start(text, i):
            node, i = self._parse_image(text, i)
            nodes.append(node)
        elif self._is_link_start(text, i):
            node, i = self._parse_link(text, i)
            nodes.append(node)
        elif self._is_inline_code_start(text, i):
            node, i = self._parse_inline_code(text, i)
            nodes.append(node)
        elif self._is_bold_start(text, i):
            node, i = self._parse_bold(text, i)
            nodes.append(node)
        elif self._is_italic_start(text, i):
            node, i = self._parse_italic(text, i)
            nodes.append(node)
        else:
            # Accumulate plain text until next formatting
            start = i
            while i < len(text) and not self._is_formatting_start(text, i):
                i += 1
            nodes.append(MarkdownTextNode(text[start:i]))
    return nodes
```
Each `_parse_*` and `_is_*_start` helper can focus on just one kind of element.

---

## 4. Summary Table

| Review Item                             | Guideline(s)                                          | Importance |
|-----------------------------------------|-------------------------------------------------------|------------|
| Type hint compatibility                 | Python code style, compliance                         | 2/4        |
| Docstring completeness/examples         | Python code style                                     | 3          |
| Exception handling granularity          | Error handling                                        | 2          |
| Logging usage                           | Error handling, code organization                     | 4          |
| Complexity in parse_inline_formatting   | Code smells, KISS, code organization                  | 2          |
| Mutable default arguments               | Python code style                                     | 4          |
| Magic numbers/strings                   | Code smells                                           | 3          |
| Table parsing repetition                | DRY                                                   | 3          |
| Performance of incremental parsing      | Performance considerations                            | 3/2        |
| Security/input validation               | Security best practices                               | 4/1        |
| Use of Any in typing                    | Python code style, code smells                        | 3          |
| Long functions                          | Python code style                                     | 3          |
| Error message exposure                  | Error handling                                        | 4          |
| Separation of concerns                  | Code organization                                     | 4          |
| Use of cast                             | Python code style                                     | 4          |
| Unit tests                              | Maintainability, error handling                       | 1/4        |
| Explicit imports                        | YAGNI                                                 | 4          |
| Line length                             | Python code style                                     | 4          |

---

## 5. Conclusion

The core parser is well-structured, and the code generally follows many best practices. The most important improvements would be:

- Refactoring complex and long methods (especially inline parsing)
- Ensuring type hint compatibility and safety
- Improving exception handling specificity
- Adding or documenting comprehensive unit test coverage

Other changes are mostly minor or stylistic. The parser is a solid base, but could be made easier to maintain and extend with these adjustments.
