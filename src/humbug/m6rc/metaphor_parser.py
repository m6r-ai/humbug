# Copyright 2024 M6R Ltd.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import glob
import os
from pathlib import Path
from typing import List, Set, cast

from humbug.m6rc.metaphor_token import MetaphorToken, MetaphorTokenType
from humbug.m6rc.embed_lexer import EmbedLexer
from humbug.m6rc.metaphor_lexer import MetaphorLexer
from humbug.m6rc.metaphor_ast_node import (
    MetaphorRootNode, MetaphorTextNode, MetaphorCodeNode,
    MetaphorRoleNode, MetaphorContextNode, MetaphorActionNode
)


class MetaphorParserFileAlreadyUsedError(Exception):
    """Exception raised when a file is used more than once."""
    def __init__(self, filename: str, token: MetaphorToken) -> None:
        super().__init__(f"The file '{filename}' has already been used.")
        self.filename: str = filename
        self.token: MetaphorToken = token


class MetaphorParserSyntaxError(Exception):
    """Exception generated when there is a syntax error."""
    def __init__(self, message: str, filename: str, line: int, column: int, input_text: str) -> None:
        super().__init__(f"{message}: file: {filename}, line {line}, column {column}, ")
        self.message: str = message
        self.filename: str = filename
        self.line: int = line
        self.column: int = column
        self.input_text: str = input_text


class MetaphorParserError(Exception):
    """Exception wrapper generated when there is a syntax error."""
    def __init__(self, message: str, errors: List[MetaphorParserSyntaxError]) -> None:
        super().__init__(message)
        self.errors: List[MetaphorParserSyntaxError] = errors


class MetaphorParser:
    """
    Parser class to process tokens and build an Abstract Syntax Tree (AST).

    Attributes:
        syntax_tree (MetaphorRootNode): The root node of the AST.
        parse_errors (List[MetaphorParserSyntaxError]): List of syntax errors encountered during parsing.
        lexers (List[MetaphorLexer | EmbedLexer]): Stack of lexers used for parsing multiple files.
        previously_seen_files (Set[str]): Set of canonical filenames already processed.
        search_paths (List[str]): List of paths to search for included files.
        embed_path (str): Path used to search for embedded files.
        current_token (MetaphorToken | None): The current token being processed.
    """
    def __init__(self) -> None:
        self.syntax_tree: MetaphorRootNode = MetaphorRootNode()
        self.parse_errors: List[MetaphorParserSyntaxError] = []
        self.lexers: List[MetaphorLexer | EmbedLexer] = []
        self.previously_seen_files: Set[str] = set()
        self.search_paths: List[str] = []
        self.embed_path: str = ""
        self.current_token: MetaphorToken | None = None

    def _insert_preamble_text(self, text: str) -> None:
        self.syntax_tree.add_child(MetaphorTextNode(text))

    def _generate_preamble(self) -> None:
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

        for text in preamble:
            self._insert_preamble_text(text)

    def parse(self, input_text: str, filename: str, search_paths: List[str], embed_path: str = "") -> MetaphorRootNode:
        """
        Parse an input string and construct the AST.

        Args:
            input_text (str): The text to be parsed.
            filename (str): The name of the file being parsed.
            search_paths (List[str]): List of paths to search for included files.
            embed_path: Path used to search for embedded files (uses CWD if None).

        Returns:
            MetaphorRootNode: The root node of the AST.

        Raises:
            MetaphorParserError: If there are syntax errors during parsing.
            FileNotFoundError: If a required file cannot be found.
        """
        self.search_paths = search_paths
        self.embed_path = embed_path if embed_path else os.getcwd()

        try:
            self.lexers.append(MetaphorLexer(input_text, filename))
            self._generate_preamble()

            seen_action_tree: bool = False
            seen_context_tree: bool = False
            seen_role_tree: bool = False

            while True:
                token = self.get_next_non_blank_token()
                if token.type == MetaphorTokenType.ACTION:
                    if seen_action_tree:
                        self._record_syntax_error(token, "'Action' already defined")

                    self.syntax_tree.add_child(self._parse_action(token))
                    seen_action_tree = True
                    continue

                if token.type == MetaphorTokenType.CONTEXT:
                    if seen_context_tree:
                        self._record_syntax_error(token, "'Context' already defined")

                    self.syntax_tree.add_child(self._parse_context(token))
                    seen_context_tree = True
                    continue

                if token.type == MetaphorTokenType.ROLE:
                    if seen_role_tree:
                        self._record_syntax_error(token, "'Role' already defined")

                    self.syntax_tree.add_child(self._parse_role(token))
                    seen_role_tree = True
                    continue

                if token.type == MetaphorTokenType.END_OF_FILE:
                    if self.parse_errors:
                        raise MetaphorParserError("parser error", self.parse_errors)

                    return self.syntax_tree

                self._record_syntax_error(token, f"Unexpected token: {token.value} at top level")

        except FileNotFoundError as e:
            err_token = cast(MetaphorToken, self.current_token)
            self.parse_errors.append(MetaphorParserSyntaxError(
                f"{e}", err_token.filename, err_token.line, err_token.column, err_token.input
            ))
            raise(MetaphorParserError("parser error", self.parse_errors)) from e

        except MetaphorParserFileAlreadyUsedError as e:
            self.parse_errors.append(MetaphorParserSyntaxError(
                f"The file '{e.filename}' has already been used",
                e.token.filename,
                e.token.line,
                e.token.column,
                e.token.input
            ))
            raise(MetaphorParserError("parser error", self.parse_errors)) from e

    def parse_file(self, filename: str, search_paths: List[str], embed_path: str = "") -> MetaphorRootNode:
        """
        Parse a file and construct the AST.

        Args:
            filename (str): The path to the file to be parsed.
            search_paths (List[str]): List of paths to search for included files.
            embed_path: Path used to search for embedded files (uses CWD if None).

        Returns:
            MetaphorRootNode: The root node of the AST.

        Raises:
            MetaphorParserError: If there are syntax errors during parsing.
            FileNotFoundError: If the file cannot be found.
        """
        try:
            self._check_file_not_loaded(filename)
            input_text = self._read_file(filename)
            return self.parse(input_text, filename, search_paths, embed_path)

        except FileNotFoundError as e:
            self.parse_errors.append(MetaphorParserSyntaxError(
                f"{e}", "", 0, 0, ""
            ))
            raise(MetaphorParserError("parser error", self.parse_errors)) from e

        except MetaphorParserError as e:
            raise(MetaphorParserError("parser error", self.parse_errors)) from e

    def get_next_token(self) -> MetaphorToken:
        """Get the next token from the active lexer."""
        while self.lexers:
            lexer = self.lexers[-1]
            token = lexer.get_next_token()
            self.current_token = token

            if token.type == MetaphorTokenType.INCLUDE:
                self._parse_include()
            elif token.type == MetaphorTokenType.EMBED:
                self._parse_embed()
            elif token.type == MetaphorTokenType.END_OF_FILE:
                self.lexers.pop()
            else:
                return token

        return MetaphorToken(MetaphorTokenType.END_OF_FILE, "", "", "", 0, 0)

    def get_next_non_blank_token(self) -> MetaphorToken:
        """Get the next token that is not a blank line."""
        token = None

        while True:
            token = self.get_next_token()
            if not token or token.type != MetaphorTokenType.TEXT or token.value != "":
                return token

    def _record_syntax_error(self, token: MetaphorToken, message: str) -> None:
        """Raise a syntax error and add it to the error list."""
        error = MetaphorParserSyntaxError(
            message, token.filename, token.line, token.column, token.input
        )
        self.parse_errors.append(error)

    def _find_file_path(self, filename: str) -> str:
        """Try to find a valid path for a file, given all the search path options"""
        if Path(filename).exists():
            return filename

        # If we don't have an absolute path then we can try search paths.
        if not os.path.isabs(filename):
            for path in self.search_paths:
                try_name = os.path.join(path, filename)
                if Path(try_name).exists():
                    return try_name

        raise FileNotFoundError(f"File not found: {filename}")

    def _read_file(self, filename: str) -> str:
        """Read file content into memory."""
        try:
            with open(filename, 'r', encoding='utf-8') as file:
                return file.read()

        except FileNotFoundError as e:
            raise FileNotFoundError(f"File not found: {filename}") from e

        except PermissionError as e:
            raise FileNotFoundError(f"You do not have permission to access: {filename}") from e

        except IsADirectoryError as e:
            raise FileNotFoundError(f"Is a directory: {filename}") from e

        except OSError as e:
            raise FileNotFoundError(f"OS error: {e}") from e

    def _check_file_not_loaded(self, filename: str) -> None:
        """Check we have not already loaded a file."""
        canonical_filename = os.path.realpath(filename)
        if canonical_filename in self.previously_seen_files:
            raise MetaphorParserFileAlreadyUsedError(filename, cast(MetaphorToken, self.current_token))

        self.previously_seen_files.add(canonical_filename)

    def _parse_text(self, token: MetaphorToken) -> MetaphorTextNode:
        """Parse a text block."""
        return MetaphorTextNode(token.value)

    def _parse_code(self, token: MetaphorToken) -> MetaphorCodeNode:
        """Parse a code block."""
        return MetaphorCodeNode(token.value)

    def _parse_action(self, token: MetaphorToken) -> MetaphorActionNode:
        """Parse an action block and construct its AST node."""
        label_name = ""

        seen_token_type = MetaphorTokenType.NONE

        init_token = self.get_next_token()
        if init_token.type == MetaphorTokenType.KEYWORD_TEXT:
            label_name = init_token.value
            indent_token = self.get_next_non_blank_token()
            if indent_token.type != MetaphorTokenType.INDENT:
                self._record_syntax_error(
                    token,
                    "Expected indent after keyword description for 'Action' block"
                )

        elif init_token.type != MetaphorTokenType.INDENT:
            self._record_syntax_error(token, "Expected description or indent for 'Action' block")

        action_node = MetaphorActionNode(label_name)

        while True:
            token = self.get_next_token()
            if token.type == MetaphorTokenType.TEXT:
                if token.value != "" and seen_token_type != MetaphorTokenType.NONE:
                    self._record_syntax_error(token, "Text must come first in an 'Action' block")

                action_node.add_child(self._parse_text(token))
                continue

            if token.type == MetaphorTokenType.CODE:
                if seen_token_type != MetaphorTokenType.NONE:
                    self._record_syntax_error(token, "Code must come first in an 'Action' block")

                action_node.add_child(self._parse_code(token))
                continue

            if token.type == MetaphorTokenType.ACTION:
                action_node.add_child(self._parse_action(token))
                seen_token_type = MetaphorTokenType.ACTION
                continue

            if token.type in (MetaphorTokenType.OUTDENT, MetaphorTokenType.END_OF_FILE):
                return action_node

            self._record_syntax_error(
                token,
                f"Unexpected token: {token.value} in 'Action' block"
            )

    def _parse_context(self, token: MetaphorToken) -> MetaphorContextNode:
        """Parse a Context block."""
        label_name = ""

        seen_token_type = MetaphorTokenType.NONE

        init_token = self.get_next_token()
        if init_token.type == MetaphorTokenType.KEYWORD_TEXT:
            label_name = init_token.value
            indent_token = self.get_next_non_blank_token()
            if indent_token.type != MetaphorTokenType.INDENT:
                self._record_syntax_error(
                    token,
                    "Expected indent after keyword description for 'Context' block"
                )

        elif init_token.type != MetaphorTokenType.INDENT:
            self._record_syntax_error(token, "Expected description or indent for 'Context' block")

        context_node = MetaphorContextNode(label_name)

        while True:
            token = self.get_next_token()
            if token.type == MetaphorTokenType.TEXT:
                if token.value != "" and seen_token_type != MetaphorTokenType.NONE:
                    self._record_syntax_error(token, "Text must come first in a 'Context' block")

                context_node.add_child(self._parse_text(token))
                continue

            if token.type == MetaphorTokenType.CODE:
                if seen_token_type != MetaphorTokenType.NONE:
                    self._record_syntax_error(token, "Code must come first in a 'Context' block")

                context_node.add_child(self._parse_code(token))
                continue

            if token.type == MetaphorTokenType.CONTEXT:
                context_node.add_child(self._parse_context(token))
                seen_token_type = MetaphorTokenType.CONTEXT
                continue

            if token.type in (MetaphorTokenType.OUTDENT, MetaphorTokenType.END_OF_FILE):
                return context_node

            self._record_syntax_error(
                token,
                f"Unexpected token: {token.value} in 'Context' block"
            )

    def _parse_role(self, token: MetaphorToken) -> MetaphorRoleNode:
        """Parse a Role block."""
        label_name = ""

        seen_token_type = MetaphorTokenType.NONE

        init_token = self.get_next_token()
        if init_token.type == MetaphorTokenType.KEYWORD_TEXT:
            label_name = init_token.value
            indent_token = self.get_next_non_blank_token()
            if indent_token.type != MetaphorTokenType.INDENT:
                self._record_syntax_error(
                    token,
                    "Expected indent after keyword description for 'Role' block"
                )

        elif init_token.type != MetaphorTokenType.INDENT:
            self._record_syntax_error(token, "Expected description or indent for 'Role' block")

        role_node = MetaphorRoleNode(label_name)

        while True:
            token = self.get_next_token()
            if token.type == MetaphorTokenType.TEXT:
                if token.value != "" and seen_token_type != MetaphorTokenType.NONE:
                    self._record_syntax_error(token, "Text must come first in a 'Role' block")

                role_node.add_child(self._parse_text(token))
                continue

            if token.type == MetaphorTokenType.CODE:
                if seen_token_type != MetaphorTokenType.NONE:
                    self._record_syntax_error(token, "Code must come first in a 'Role' block")

                role_node.add_child(self._parse_code(token))
                continue

            if token.type == MetaphorTokenType.ROLE:
                role_node.add_child(self._parse_role(token))
                seen_token_type = MetaphorTokenType.ROLE
                continue

            if token.type in (MetaphorTokenType.OUTDENT, MetaphorTokenType.END_OF_FILE):
                return role_node

            self._record_syntax_error(
                token,
                f"Unexpected token: {token.value} in 'Role' block"
            )

    def _parse_include(self) -> None:
        """Parse an Include block and load the included file."""
        token_next = self.get_next_token()
        if token_next.type != MetaphorTokenType.KEYWORD_TEXT:
            self._record_syntax_error(token_next, "Expected file name for 'Include'")
            return

        filename = token_next.value
        self._check_file_not_loaded(filename)
        try_file = self._find_file_path(filename)
        input_text = self._read_file(try_file)
        self.lexers.append(MetaphorLexer(input_text, try_file))

    def _parse_embed(self) -> None:
        """Parse an Embed block and load the embedded file."""
        token_next = self.get_next_token()
        if token_next.type != MetaphorTokenType.KEYWORD_TEXT:
            self._record_syntax_error(token_next, "Expected file name or wildcard match for 'Embed'")
            return

        recurse = False
        match = token_next.value
        if "**/" in match:
            recurse = True

        path = os.path.join(self.embed_path, match)
        files = glob.glob(path, recursive=recurse)
        if not files:
            self._record_syntax_error(token_next, f"{match} does not match any files for 'Embed' in {self.embed_path}")
            return

        for file in files:
            input_text = self._read_file(file)
            self.lexers.append(EmbedLexer(input_text, file))
