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

from typing import Dict, List

from .metaphor_token import Token, TokenType

class EmbedLexer:
    """
    Lexer for handling embedded content like code blocks.
    """

    file_exts: Dict[str, str] = {
        "bash": "bash",
        "c": "c",
        "clj": "clojure",
        "cpp": "cpp",
        "cs": "csharp",
        "css": "css",
        "dart": "dart",
        "ebnf": "ebnf",
        "erl": "erlang",
        "ex": "elixir",
        "hpp": "cpp",
        "go": "go",
        "groovy": "groovy",
        "h": "c",
        "hs": "haskell",
        "html": "html",
        "java": "java",
        "js": "javascript",
        "json": "json",
        "kt": "kotlin",
        "lua": "lua",
        "m6r": "metaphor",
        "m": "objectivec",
        "md": "markdown",
        "mm": "objectivec",
        "php": "php",
        "pl": "perl",
        "py": "python",
        "r": "r",
        "rkt": "racket",
        "rb": "ruby",
        "rs": "rust",
        "scala": "scala",
        "sh": "bash",
        "sql": "sql",
        "swift": "swift",
        "ts": "typescript",
        "vb": "vbnet",
        "vbs": "vbscript",
        "xml": "xml",
        "yaml": "yaml",
        "yml": "yaml"
    }

    def __init__(self, input_text, filename):
        """
        Initialize the EmbedLexer for handling embedded content.

        Args:
            input_text (str): The text content to be lexically analyzed
            filename (str): Name of the file being processed
        """
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

    def _get_language_from_file_extension(self, filename: str) -> str:
        """Get a language name from a filename extension."""
        extension: str = ""
        if '.' in filename:
            extension = (filename.rsplit('.', 1)[-1]).lower()

        return self.file_exts.get(extension, "plaintext")

    def _tokenize(self) -> None:
        """Tokenizes the input file and handles embedded content."""
        self.tokens.append(Token(TokenType.TEXT, f"File: {self.filename}", "", self.filename, 0, 1))
        self.tokens.append(
            Token(
                TokenType.TEXT,
                "```" + self._get_language_from_file_extension(self.filename),
                "",
                self.filename,
                0,
                1
            )
        )

        lines = self.input.splitlines()
        for line in lines:
            token = Token(TokenType.TEXT, line, line, self.filename, self.current_line, 1)
            self.tokens.append(token)
            self.current_line += 1

        self.tokens.append(Token(TokenType.TEXT, "```", "", self.filename, self.current_line, 1))
