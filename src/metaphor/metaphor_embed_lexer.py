from typing import List

from syntax.programming_language_utils import ProgrammingLanguageUtils

from metaphor.metaphor_token import MetaphorToken, MetaphorTokenType


class MetaphorEmbedLexer:
    """
    Lexer for handling embedded content like code blocks.
    """

    def __init__(self, input_text: str, filename: str) -> None:
        """
        Initialize the MetaphorEmbedLexer for handling embedded content.

        Args:
            input_text: The text content to be lexically analyzed
            filename: Name of the file being processed
        """
        self.filename: str = filename
        self.tokens: List[MetaphorToken] = []
        self.current_line: int = 1
        self.input: str = input_text
        self._tokenize()

    def get_next_token(self) -> MetaphorToken:
        """Return the next token from the token list."""
        if self.tokens:
            return self.tokens.pop(0)

        return MetaphorToken(MetaphorTokenType.END_OF_FILE, "", "", self.filename, self.current_line, 1)

    def _get_language_from_file_extension(self, filename: str) -> str:
        """
        Get a language name from a filename extension.

        Args:
            filename: The filename to extract language from

        Returns:
            Lowercase language name suitable for code block formatting
        """
        detected_language = ProgrammingLanguageUtils.from_file_extension(filename)
        return ProgrammingLanguageUtils.get_name(detected_language)

    def _tokenize(self) -> None:
        """Tokenizes the input file and handles embedded content."""
        self.tokens.append(MetaphorToken(MetaphorTokenType.TEXT, f"File: {self.filename}", "", self.filename, 0, 1))
        self.tokens.append(MetaphorToken(MetaphorTokenType.TEXT, "", "", self.filename, 0, 1))
        self.tokens.append(
            MetaphorToken(
                MetaphorTokenType.CODE,
                "```" + self._get_language_from_file_extension(self.filename),
                "",
                self.filename,
                0,
                1
            )
        )

        lines = self.input.splitlines()
        for line in lines:
            token = MetaphorToken(MetaphorTokenType.CODE, line, line, self.filename, self.current_line, 1)
            self.tokens.append(token)
            self.current_line += 1

        self.tokens.append(MetaphorToken(MetaphorTokenType.CODE, "```", "", self.filename, self.current_line, 1))
        self.tokens.append(MetaphorToken(MetaphorTokenType.TEXT, "", "", self.filename, self.current_line, 1))
