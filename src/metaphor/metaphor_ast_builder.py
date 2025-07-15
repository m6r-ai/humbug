import glob
import os
from pathlib import Path
from typing import List, Set, cast, Callable

from metaphor.metaphor_token import MetaphorToken, MetaphorTokenType
from metaphor.metaphor_embed_lexer import MetaphorEmbedLexer
from metaphor.metaphor_lexer import MetaphorLexer
from metaphor.metaphor_ast_node import (
    MetaphorASTNode, MetaphorASTTextNode, MetaphorASTCodeNode,
    MetaphorASTRoleNode, MetaphorASTContextNode, MetaphorASTActionNode
)


class MetaphorASTBuilderFileAlreadyUsedError(Exception):
    """Exception raised when a file is used more than once."""
    def __init__(self, filename: str, token: MetaphorToken) -> None:
        super().__init__(f"The file '{filename}' has already been used.")
        self.filename: str = filename
        self.token: MetaphorToken = token


class MetaphorASTBuilderSyntaxError(Exception):
    """Exception generated when there is a syntax error."""
    def __init__(self, message: str, filename: str, line: int, column: int, input_text: str) -> None:
        super().__init__(f"{message}: file: {filename}, line {line}, column {column}, ")
        self.message: str = message
        self.filename: str = filename
        self.line: int = line
        self.column: int = column
        self.input_text: str = input_text


class MetaphorASTBuilderError(Exception):
    """Exception wrapper generated when there is a syntax error."""
    def __init__(self, message: str, errors: List[MetaphorASTBuilderSyntaxError]) -> None:
        super().__init__(message)
        self.errors: List[MetaphorASTBuilderSyntaxError] = errors


class MetaphorASTBuilder:
    """
    Parser class to process tokens and build an Abstract Syntax Tree (AST).

    Attributes:
        _parse_errors (List[MetaphorASTBuilderSyntaxError]): List of syntax errors encountered during parsing.
        _lexers (List[MetaphorLexer | MetaphorEmbedLexer]): Stack of lexers used for parsing multiple files.
        _previously_seen_files (Set[str]): Set of canonical filenames already processed.
        _search_paths (List[str]): List of paths to search for included files.
        embed_path (str): Path used to search for embedded files.
        current_token (MetaphorToken | None): The current token being processed.
    """
    def __init__(self, get_cannonical_path_callback: Callable[[str], str | None] | None = None) -> None:
        self._parse_errors: List[MetaphorASTBuilderSyntaxError] = []
        self._lexers: List[MetaphorLexer | MetaphorEmbedLexer] = []
        self._previously_seen_files: Set[str] = set()
        self._search_paths: List[str] = []
        self._embed_path: str = ""
        self._current_token: MetaphorToken | None = None
        self._arguments: List[str] = []
        self._get_cannonical_path: Callable[[str], str | None] | None = get_cannonical_path_callback

    def _has_role_node(self, node: MetaphorASTNode) -> bool:
        """
        Check if the node tree contains any Role nodes.

        Args:
            node: The node to check

        Returns:
            True if the node or any of its descendants is a Role node
        """
        if isinstance(node, MetaphorASTRoleNode):
            return True

        return any(self._has_role_node(cast(MetaphorASTNode, child)) for child in node.children)

    def _has_context_node(self, node: MetaphorASTNode) -> bool:
        """
        Check if the node tree contains any Context nodes.

        Args:
            node: The node to check

        Returns:
            True if the node or any of its descendants is a Context node
        """
        if isinstance(node, MetaphorASTContextNode):
            return True

        return any(self._has_context_node(cast(MetaphorASTNode, child)) for child in node.children)

    def _has_action_node(self, node: MetaphorASTNode) -> bool:
        """
        Check if the node tree contains any Action nodes.

        Args:
            node: The node to check

        Returns:
            True if the node or any of its descendants is an Action node
        """
        if isinstance(node, MetaphorASTActionNode):
            return True

        return any(self._has_action_node(cast(MetaphorASTNode, child)) for child in node.children)

    def build_ast(
        self,
        parent_node: MetaphorASTNode,
        input_text: str,
        filename: str,
        search_paths: List[str],
        embed_path: str = "",
        arguments: List[str] | None = None
    ) -> None:
        """
        Parse an input string and add the resulting nodes to the provided parent node.

        Args:
            parent_node (MetaphorASTNode): The node to which parsed content will be added.
            input_text (str): The text to be parsed.
            filename (str): The name of the file being parsed.
            search_paths (List[str]): List of paths to search for included files.
            embed_path: Path used to search for embedded files (uses CWD if None).
            arguments (List[str] | None): List of command line arguments.

        Raises:
            MetaphorASTBuilderError: If there are syntax errors during parsing.
        """
        self._search_paths = search_paths
        self._embed_path = embed_path if embed_path else os.getcwd()
        self._arguments = arguments if arguments else []

        try:
            self._lexers.append(MetaphorLexer(input_text, filename))

            seen_action_tree: bool = self._has_action_node(parent_node)
            seen_context_tree: bool = self._has_context_node(parent_node)
            seen_role_tree: bool = self._has_role_node(parent_node)

            while True:
                token = self.get_next_non_blank_token()
                if token.type == MetaphorTokenType.ACTION:
                    if seen_action_tree and not isinstance(parent_node, MetaphorASTActionNode):
                        self._record_syntax_error(token, "'Action' already defined")

                    parent_node.add_child(self._parse_action(token))
                    seen_action_tree = True
                    continue

                if token.type == MetaphorTokenType.CONTEXT:
                    if seen_context_tree and not isinstance(parent_node, MetaphorASTContextNode):
                        self._record_syntax_error(token, "'Context' already defined")

                    parent_node.add_child(self._parse_context(token))
                    seen_context_tree = True
                    continue

                if token.type == MetaphorTokenType.ROLE:
                    if seen_role_tree and not isinstance(parent_node, MetaphorASTRoleNode):
                        self._record_syntax_error(token, "'Role' already defined")

                    parent_node.add_child(self._parse_role(token))
                    seen_role_tree = True
                    continue

                if token.type == MetaphorTokenType.END_OF_FILE:
                    if self._parse_errors:
                        raise MetaphorASTBuilderError("parser error", self._parse_errors)

                    return  # No return value needed

                if token.type in (MetaphorTokenType.TEXT, MetaphorTokenType.CODE):
                    if parent_node.children:
                        # If our parent is processing text or code then we can add more text or code
                        if isinstance(parent_node.children[-1], MetaphorASTTextNode | MetaphorASTCodeNode):
                            parent_node.add_child(self._parse_text(token))
                            continue

                        self._record_syntax_error(token, "Text or code must come first in parent's block")
                        continue

                    # If our parent is an action, context or role node and has no children
                    # then we can add text or code to it.
                    if isinstance(parent_node, MetaphorASTActionNode | MetaphorASTContextNode | MetaphorASTRoleNode):
                        parent_node.add_child(self._parse_text(token))
                        continue

                self._record_syntax_error(token, f"Unexpected token: {token.value} at top level")

        except FileNotFoundError as e:
            err_token = cast(MetaphorToken, self._current_token)
            self._parse_errors.append(MetaphorASTBuilderSyntaxError(
                f"{e}", err_token.filename, err_token.line, err_token.column, err_token.input
            ))
            raise(MetaphorASTBuilderError("parser error", self._parse_errors)) from e

        except MetaphorASTBuilderFileAlreadyUsedError as e:
            self._parse_errors.append(MetaphorASTBuilderSyntaxError(
                f"The file '{e.filename}' has already been used",
                e.token.filename,
                e.token.line,
                e.token.column,
                e.token.input
            ))
            raise(MetaphorASTBuilderError("parser error", self._parse_errors)) from e

    def build_ast_from_file(
        self,
        parent_node: MetaphorASTNode,
        filename: str,
        search_paths: List[str],
        embed_path: str = "",
        arguments: List[str] | None = None
    ) -> None:
        """
        Parse a file and add the resulting nodes to the provided parent node.

        Args:
            parent_node (MetaphorASTNode): The node to which parsed content will be added.
            filename (str): The path to the file to be parsed.
            search_paths (List[str]): List of paths to search for included files.
            embed_path: Path used to search for embedded files (uses CWD if None).
            arguments (List[str] | None): List of command line arguments.

        Raises:
            MetaphorASTBuilderError: If there are syntax errors during parsing.
        """
        try:
            self._check_file_not_loaded(filename)
            input_text = self._read_file(filename)
            self.build_ast(parent_node, input_text, filename, search_paths, embed_path, arguments)

        except FileNotFoundError as e:
            self._parse_errors.append(MetaphorASTBuilderSyntaxError(
                f"{e}", "", 0, 0, ""
            ))
            raise(MetaphorASTBuilderError("parser error", self._parse_errors)) from e

        except MetaphorASTBuilderError as e:
            raise(MetaphorASTBuilderError("parser error", self._parse_errors)) from e

    def get_next_token(self) -> MetaphorToken:
        """Get the next token from the active lexer."""
        while self._lexers:
            lexer = self._lexers[-1]
            token = lexer.get_next_token()
            self._current_token = token

            if token.type == MetaphorTokenType.INCLUDE:
                self._parse_include(token)

            elif token.type == MetaphorTokenType.EMBED:
                self._parse_embed(token)

            elif token.type == MetaphorTokenType.END_OF_FILE:
                self._lexers.pop()

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
        error = MetaphorASTBuilderSyntaxError(
            message, token.filename, token.line, token.column, token.input
        )
        self._parse_errors.append(error)

    def _find_file_path(self, filename: str) -> str:
        """Try to find a valid path for a file, given all the search path options"""
        if os.path.isabs(filename):
            if Path(filename).exists():
                return filename

            raise FileNotFoundError(f"File not found: {filename}")

        # If we don't have an absolute path then we can try search paths.
        for path in self._search_paths:
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
        if canonical_filename in self._previously_seen_files:
            raise MetaphorASTBuilderFileAlreadyUsedError(filename, cast(MetaphorToken, self._current_token))

        self._previously_seen_files.add(canonical_filename)

    def _parse_text(self, token: MetaphorToken) -> MetaphorASTTextNode:
        """Parse a text block."""
        return MetaphorASTTextNode(token.value)

    def _parse_code(self, token: MetaphorToken) -> MetaphorASTCodeNode:
        """Parse a code block."""
        return MetaphorASTCodeNode(token.value)

    def _parse_action(self, token: MetaphorToken) -> MetaphorASTActionNode:
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
            self._record_syntax_error(token, "Expected description or indent after 'Action' keyword")

        action_node = MetaphorASTActionNode(label_name)

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

            self._record_syntax_error(token, f"Unexpected token: {token.value} in 'Action' block")

    def _parse_context(self, token: MetaphorToken) -> MetaphorASTContextNode:
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
            self._record_syntax_error(token, "Expected description or indent after 'Context' keyword")

        context_node = MetaphorASTContextNode(label_name)

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

            self._record_syntax_error(token, f"Unexpected token: {token.value} in 'Context' block")

    def _parse_role(self, token: MetaphorToken) -> MetaphorASTRoleNode:
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
            self._record_syntax_error(token, "Expected description or indent after 'Role' keyword")

        role_node = MetaphorASTRoleNode(label_name)

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

            self._record_syntax_error(token, f"Unexpected token: {token.value} in 'Role' block")

    def _parse_argument(self, token: MetaphorToken) -> str | None:
        """
        Parse a $ argument token.

        Args:
            token: The token to parse.

        Returns:
            The parsed argument as a string or None if we could not determine the string.
        """
        numeric_part = token.value[1:]
        if not numeric_part.isdigit():
            self._record_syntax_error(token, "After '$', the string must contain only digits")

        arg_index = int(numeric_part)
        if arg_index < 0 or arg_index >= len(self._arguments):
            self._record_syntax_error(token, f"Argument index {arg_index} is out of range")
            return None

        return self._arguments[arg_index]

    def _parse_include(self, token: MetaphorToken) -> None:
        """Parse an Include block and load the included file."""
        token_next = self.get_next_token()
        if token_next.type != MetaphorTokenType.KEYWORD_TEXT:
            self._record_syntax_error(token, "Expected file name after 'Include' keyword")
            return

        if token_next.value[0] == "$":
            filename = self._parse_argument(token_next)
            if filename is None:
                return

        else:
            filename = token_next.value

        self._check_file_not_loaded(filename)
        try_file = self._find_file_path(filename)
        input_text = self._read_file(try_file)
        self._lexers.append(MetaphorLexer(input_text, try_file))

    def _parse_embed(self, token: MetaphorToken) -> None:
        """Parse an Embed block and load the embedded file."""
        token_next = self.get_next_token()
        if token_next.type != MetaphorTokenType.KEYWORD_TEXT:
            self._record_syntax_error(token, "Expected file name or wildcard match after 'Embed' keyword")
            return

        if token_next.value[0] == "$":
            match = self._parse_argument(token_next)
            if match is None:
                return

        else:
            match = token_next.value

        stripped_match = match.rstrip()
        path = os.path.join(self._embed_path, stripped_match)
        recurse = "**/" in match
        files = glob.glob(path, recursive=recurse)
        if not files:
            self._record_syntax_error(token_next, f"{match} does not match any files for 'Embed' in {self._embed_path}")
            return

        for file in files:
            input_text = self._read_file(file)
            relative_path = self._get_cannonical_path(file) if self._get_cannonical_path else file
            if not relative_path:
                self._record_syntax_error(token_next, f"Could not get canonical path for {file}")
                return

            self._lexers.append(MetaphorEmbedLexer(input_text, relative_path))
