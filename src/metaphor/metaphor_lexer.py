from typing import Dict, List, Final, Any

from syntax import ParserRegistry, ProgrammingLanguage, ProgrammingLanguageUtils

from metaphor.metaphor_token import MetaphorToken, MetaphorTokenType


class MetaphorLexer:
    """
    Lexer for handling the Metaphor language with its specific syntax.

    The Metaphor language consists of:
    - Keywords (Action:, Context:, Role:, etc)
    - Indented blocks
    - Text content
    - Include/Embed directives

    This lexer handles proper indentation, text block detection, and keyword parsing,
    including nested code blocks with proper continuation detection.
    """

    # Constants for language elements
    INDENT_SPACES = 4

    # Mapping of keywords to their token types
    KEYWORDS: Final[Dict[str, MetaphorTokenType]] = {
        "Action:": MetaphorTokenType.ACTION,
        "Context:": MetaphorTokenType.CONTEXT,
        "Embed:": MetaphorTokenType.EMBED,
        "Include:": MetaphorTokenType.INCLUDE,
        "Role:": MetaphorTokenType.ROLE
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
        self.tokens: List[MetaphorToken] = []
        self.current_line: int = 1
        self.input: str = input_text

        # Code block state tracking
        self._code_block_language: str = ""
        self._embedded_parser_state: Any = None
        self._embedded_language: ProgrammingLanguage = ProgrammingLanguage.UNKNOWN
        self._code_block_nesting_level: int = 0
        self._code_block_indents: List[int] = []

        self._tokenize()

    def get_next_token(self) -> MetaphorToken:
        """Return the next token from the token list."""
        if self.tokens:
            return self.tokens.pop(0)

        return MetaphorToken(MetaphorTokenType.END_OF_FILE, "", "", self.filename, self.current_line, 1)

    def _should_parse_code_block_as_continuation(
        self,
        language: ProgrammingLanguage,
        line_content: str
    ) -> bool:
        """
        Check if a line should be parsed as a continuation by the embedded parser.

        Args:
            language: The programming language of the code block
            line_content: The content of the line to check

        Returns:
            True if this line should be treated as a continuation
        """
        if language == ProgrammingLanguage.TEXT:
            return False

        # Get or create parser for this language
        parser = ParserRegistry.create_parser(language)
        assert parser is not None, f"No parser found for language: {language}"

        # If language changed, reset state
        if language != self._embedded_language:
            self._embedded_parser_state = None
            self._embedded_language = language

        # Parse the line and check if we're in a continuation
        new_state = parser.parse(self._embedded_parser_state, line_content)

        # Update stored state
        self._embedded_parser_state = new_state

        # Return whether we're in a continuation
        return new_state is not None and new_state.parsing_continuation

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
                MetaphorToken(
                    type=MetaphorTokenType.OUTDENT,
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

        # Handle code block state
        if self.in_fenced_code:
            # Check if our embedded parser indicates this should be a continuation
            if self._code_block_language:
                language = ProgrammingLanguageUtils.from_name(self._code_block_language)
                if language != ProgrammingLanguage.UNKNOWN:
                    if self._should_parse_code_block_as_continuation(language, line):
                        self._handle_text_line(line, start_column)
                        return

            # If not a continuation and it's a code fence, check for nesting or closing
            if stripped_line.startswith('```'):
                # Check if this is a nested fence (indented more than the opening fence)
                if start_column > self._code_block_indents[-1]:
                    self._code_block_nesting_level += 1
                    self._code_block_indents.append(start_column)
                    self._handle_text_line(line, start_column)
                    return

                # This is closing the current fence
                self._code_block_indents.pop()
                self._code_block_nesting_level -= 1

                if self._code_block_nesting_level == 0:
                    # This closes the outer-most fence
                    self._handle_text_line(line, start_column)
                    self.in_fenced_code = False

                    # Reset code block state
                    self._code_block_language = ""
                    self._embedded_parser_state = None
                    self._embedded_language = ProgrammingLanguage.UNKNOWN
                    return

                # This closes a nested fence
                self._handle_text_line(line, start_column)
                return

            # Regular content within code block
            self._handle_text_line(line, start_column)
            return

        # Handle code fence start when not in code block
        if stripped_line.startswith('```'):
            self.in_fenced_code = True
            self._code_block_nesting_level = 1
            self._code_block_indents = [start_column]

            # Extract language if present
            language_match = stripped_line[3:].strip()
            if language_match:
                self._code_block_language = language_match

            # Generate the opening token
            self._handle_text_line(line, start_column)
            return

        # If we're not in a fenced code block then look for keywords.
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
            MetaphorToken(
                type=MetaphorTokenType.TAB,
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
            MetaphorToken(
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
                MetaphorToken(
                    type=MetaphorTokenType.KEYWORD_TEXT,
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
        token_type = MetaphorTokenType.CODE if self.in_fenced_code else MetaphorTokenType.TEXT
        self.tokens.append(
            MetaphorToken(
                type=token_type,
                value=text_content,
                input=line,
                filename=self.filename,
                line=self.current_line,
                column=start_column
            )
        )
        self.in_text_block = True

    def _handle_blank_line(self, start_column: int) -> None:
        """
        Handle a blank line.

        Args:
            start_column: The starting column of the content
        """
        token_type = MetaphorTokenType.CODE if self.in_fenced_code else MetaphorTokenType.TEXT
        self.tokens.append(
            MetaphorToken(
                type=token_type,
                value="",
                input="",
                filename=self.filename,
                line=self.current_line,
                column=start_column
            )
        )

        # Also update parser state for blank lines in code blocks
        if self.in_fenced_code and self._code_block_language:
            language = ProgrammingLanguageUtils.from_name(self._code_block_language)
            if language != ProgrammingLanguage.UNKNOWN:
                self._should_parse_code_block_as_continuation(language, "")

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
                MetaphorToken(
                    type=MetaphorTokenType.BAD_INDENT,
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
                MetaphorToken(
                    type=MetaphorTokenType.INDENT,
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
                MetaphorToken(
                    type=MetaphorTokenType.BAD_OUTDENT,
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
                MetaphorToken(
                    type=MetaphorTokenType.OUTDENT,
                    value="[Outdent]",
                    input=line,
                    filename=self.filename,
                    line=self.current_line,
                    column=start_column
                )
            )
            indent_offset += self.INDENT_SPACES

        self.indent_column = start_column
