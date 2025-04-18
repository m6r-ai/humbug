from typing import Dict, List

from humbug.metaphor.metaphor_token import MetaphorToken, MetaphorTokenType

class MetaphorEmbedLexer:
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

    def __init__(self, input_text: str, filename: str) -> None:
        """
        Initialize the MetaphorEmbedLexer for handling embedded content.

        Args:
            input_text (str): The text content to be lexically analyzed
            filename (str): Name of the file being processed
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
        """Get a language name from a filename extension."""
        extension: str = ""
        if '.' in filename:
            extension = (filename.rsplit('.', 1)[-1]).lower()

        return self.file_exts.get(extension, "plaintext")

    def _tokenize(self) -> None:
        """MetaphorTokenizes the input file and handles embedded content."""
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
