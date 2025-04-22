"""Functions for formatting Metaphor AST nodes and error messages."""

import io
from typing import List

from humbug.metaphor.metaphor_parser import MetaphorParserSyntaxError


def format_preamble() -> str:
    """
    Format the preamble for the Metaphor language.

    Returns:
        Formatted preamble string
    """
    preamble: List[str] = [
        "The following preamble describes some elements of a language called Metaphor.  Please pay",
        "extremely close attention to the details as they will affect the way you interpret",
        "everything that follows after \"BEGIN DESCRIPTION IN METAPHOR:\"",
        "",
        "Metaphor is a structured natural language prompt creation language.  It is designed to",
        "let a user convey their requirements to a large language model AI.",
        "",
        "Metaphor has the structure of a document tree with branches and leaves being prefixed ",
        "by new sections containing the keywords \"Role:\", \"Context:\" or \"Action:\".  Each of",
        "these indicates the start of a new block of information.  Blocks are introduced using",
        "Markdown-style headings (using hash symbols).  The number of hashes gives an indication",
        "of the nesting depth and the parent/child relationship.",
        "",
        "Block keywords have an optional name that will immediately follow them on the same line.",
        "If this is missing then the block name is not defined.",
        "",
        "After a block heading there may be one or more lines of text that will describe the purpose",
        "of that block.  A block may also include one or more optional child blocks inside them and",
        "that further clarify their parent block.",
        "",
        "Within the text of a block, you may be presented with code or document fragments inside a",
        "block delimited by 3 backticks.  Please pay close attention to the indentation level of the",
        "opening 3 backticks.  The identation of such code or document fragments is relative to this,",
        "not relative to the block in which the code or document fragment occurs.",
        "",
        "If \"Role:\" block exists then this contains details about the role you should fulfil.  This",
        "may also describe specific skills you have, knowledge you should apply, and the",
        "approach you take to apply these."
        "",
        "\"Context:\" blocks provide context necessary to understand what you will be asked to do.",
        "",
        "\"Action:\" blocks describes the task, or tasks, you should do.",
        "",
        "If you are asked to offer any opinions on the Metaphor prompt then always provide the full",
        "set of parent headings leading up to any block you want to talk about.  This will allow",
        "the user to understand which part of the Metaphor prompt is being discussed.",
        "",
        "When you process the actions please carefully ensure you do all of them accurately and",
        "complete all the elements requested.  Unless otherwise instructed, do not include any",
        "placeholders in your responses.",
        "",
        "BEGIN DESCRIPTION IN METAPHOR:"
    ]

    output = io.StringIO()
    for text in preamble:
        output.write(text)
        output.write("\n")

    return output.getvalue()


def format_errors(errors: List[MetaphorParserSyntaxError]) -> str:
    """Format a list of syntax errors as a string.

    Args:
        errors: List of syntax errors to format

    Returns:
        Formatted error string with each error on separate lines
    """
    output = io.StringIO()

    for error in errors:
        caret = " " * (error.column - 1)
        error_message = (
            f"{error.message}: line {error.line}, column {error.column}, "
            f"file {error.filename}\n{caret}|\n{caret}v\n{error.input_text}"
        )
        output.write(f"----------------\n{error_message}\n")

    output.write("----------------\n")
    return output.getvalue()
