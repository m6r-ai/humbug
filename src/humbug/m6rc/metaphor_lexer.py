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

from typing import Dict, List, Final

from .metaphor_token import Token, TokenType

class MetaphorLexer:
    """
    Lexer for handling the Metaphor language with its specific syntax.

    The Metaphor language consists of:
    - Keywords (Action:, Context:, Role:, etc)
    - Indented blocks
    - Text content
    - Include/Embed directives

    This lexer handles proper indentation, text block detection, and keyword parsing.
    """

    # Constants for language elements
    INDENT_SPACES = 4

    # Mapping of keywords to their token types
    KEYWORDS: Final[Dict[str, TokenType]] = {
        "Action:": TokenType.ACTION,
        "Context:": TokenType.CONTEXT,
        "Embed:": TokenType.EMBED,
        "Include:": TokenType.INCLUDE,
        "Role:": TokenType.ROLE
    }

    def __init__(self, input_text: str, filename: str) -> None:
        """
        Initialize the MetaphorLexer.

        Args:
            input_text (str): The text content to be lexically analyzed
            filename (str): Name of the file being processed
        """
        self.in_text_block: bool = False
        self.in_fenced_code: bool = False
        self.indent_column: int = 1
        self.filename: str = filename
        self.tokens: List[Token] = []
        self.current_line: int = 1
        self.input: str = input_text
        self._tokenize()

    def get_next_token(self) -> Token:
        """Return the next token from the token list."""
        if self.tokens:
            return self.tokens.pop(0)

        return Token(TokenType.END_OF_FILE, "", "", self.filename, self.current_line, 1)

    def _tokenize(self) -> None:
        """
        Tokenize the input file into appropriate tokens.
        Processes each line for indentation, keywords, and text content.
        """
        if not self.input:
            return

        lines: List[str] = self.input.splitlines()
        for line in lines:
            self._process_line(line)
            self.current_line += 1

        # Handle remaining outdents at end of file
        self._handle_final_outdents()

    def _handle_final_outdents(self) -> None:
        """Handle any remaining outdents needed at the end of file."""
        while self.indent_column > 1:
            self.tokens.append(
                Token(
                    type=TokenType.OUTDENT,
                    value="[Outdent]",
                    input="",
                    filename=self.filename,
                    line=self.current_line,
                    column=self.indent_column
                )
            )
            self.indent_column -= self.INDENT_SPACES

    def _process_line(self, line: str) -> None:
        """
        Process a single line of input.

        Args:
            line: The line to process
        """
        stripped_line = line.lstrip(' ')
        start_column = len(line) - len(stripped_line) + 1

        if not stripped_line:
            if self.in_fenced_code:
                self._handle_blank_line(start_column)

            return

        # Is this line a comment?
        if stripped_line.startswith('#'):
            return

        # Does this line start with a tab character?
        if stripped_line.startswith('\t'):
            self._handle_tab_character(stripped_line, start_column)
            stripped_line = stripped_line[1:]
            if not stripped_line:
                return

        # Does this line start with a code fence?
        if stripped_line.startswith('```'):
            self.in_fenced_code = not self.in_fenced_code

        # If we're not in a fenced code block then look for keywords.
        if not self.in_fenced_code:
            words = stripped_line.split(maxsplit=1)
            first_word = words[0].capitalize()

            if first_word in self.KEYWORDS:
                self._handle_keyword_line(line, words, first_word, start_column)
                return

        # Treat this as a text block.
        self._handle_text_line(line, start_column)

    def _handle_tab_character(self, line: str, column: int) -> None:
        """
        Handle tab characters in the input.

        Args:
            line: The line to check
            column: The current column number
        """
        self.tokens.append(
            Token(
                type=TokenType.TAB,
                value="[Tab]",
                input=line,
                filename=self.filename,
                line=self.current_line,
                column=column
            )
        )

    def _handle_keyword_line(self, line: str, words: List[str], keyword: str, start_column: int) -> None:
        """
        Handle a line that starts with a keyword.

        Args:
            line: The complete line
            words: The line split into words
            keyword: The keyword found
            start_column: The starting column of the content
        """
        self._process_indentation(line, start_column)

        # Create keyword token
        self.tokens.append(
            Token(
                type=self.KEYWORDS[keyword],
                value=keyword,
                input=line,
                filename=self.filename,
                line=self.current_line,
                column=start_column
            )
        )

        # Handle any text after the keyword
        if len(words) > 1:
            self.tokens.append(
                Token(
                    type=TokenType.KEYWORD_TEXT,
                    value=words[1],
                    input=line,
                    filename=self.filename,
                    line=self.current_line,
                    column=start_column + len(keyword) + 1
                )
            )

        self.in_text_block = False

    def _handle_text_line(self, line: str, start_column: int) -> None:
        """
        Handle a line that contains text content.

        Args:
            line: The line to process
            start_column: The starting column of the content
        """
        # Adjust indentation for continued text blocks
        if self.in_text_block:
            if start_column > self.indent_column:
                start_column = self.indent_column
            elif start_column < self.indent_column:
                self._process_indentation(line, start_column)
        else:
            self._process_indentation(line, start_column)

        text_content = line[start_column - 1:]
        self.tokens.append(
            Token(
                type=TokenType.TEXT,
                value=text_content,
                input=line,
                filename=self.filename,
                line=self.current_line,
                column=start_column
            )
        )
        self.in_text_block = True

    def _handle_blank_line(self, start_column: int) -> None:
        self.tokens.append(
            Token(
                type=TokenType.TEXT,
                value="",
                input="",
                filename=self.filename,
                line=self.current_line,
                column=start_column
            )
        )

    def _process_indentation(self, line: str, start_column: int) -> None:
        """
        Process the indentation of the current line.

        Args:
            line: The current line
            start_column: The starting column of the content
        """
        indent_offset = start_column - self.indent_column

        if indent_offset > 0:
            self._handle_indent(line, start_column, indent_offset)
        elif indent_offset < 0:
            self._handle_outdent(line, start_column, indent_offset)

    def _handle_indent(self, line: str, start_column: int, indent_offset: int) -> None:
        """
        Handle an increase in indentation.

        Args:
            line: The current line
            start_column: The starting column of the content
            indent_offset: The change in indentation
        """
        if indent_offset % self.INDENT_SPACES != 0:
            self.tokens.append(
                Token(
                    type=TokenType.BAD_INDENT,
                    value="[Bad Indent]",
                    input=line,
                    filename=self.filename,
                    line=self.current_line,
                    column=start_column
                )
            )
            return

        while indent_offset > 0:
            self.tokens.append(
                Token(
                    type=TokenType.INDENT,
                    value="[Indent]",
                    input=line,
                    filename=self.filename,
                    line=self.current_line,
                    column=start_column
                )
            )
            indent_offset -= self.INDENT_SPACES

        self.indent_column = start_column

    def _handle_outdent(self, line: str, start_column: int, indent_offset: int) -> None:
        """
        Handle a decrease in indentation.

        Args:
            line: The current line
            start_column: The starting column of the content
            indent_offset: The change in indentation
        """
        if abs(indent_offset) % self.INDENT_SPACES != 0:
            self.tokens.append(
                Token(
                    type=TokenType.BAD_OUTDENT,
                    value="[Bad Outdent]",
                    input=line,
                    filename=self.filename,
                    line=self.current_line,
                    column=start_column
                )
            )
            return

        while indent_offset < 0:
            self.tokens.append(
                Token(
                    type=TokenType.OUTDENT,
                    value="[Outdent]",
                    input=line,
                    filename=self.filename,
                    line=self.current_line,
                    column=start_column
                )
            )
            indent_offset += self.INDENT_SPACES

        self.indent_column = start_column
