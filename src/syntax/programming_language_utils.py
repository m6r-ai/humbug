"""
Utilities for handling programming language detection, conversion, and display.

This module centralizes conversion between language names, ProgrammingLanguage enum values,
file extensions, and display names, eliminating code duplication across the application.
"""

import os
import logging
from typing import Dict, List, Tuple

from syntax.programming_language import ProgrammingLanguage


class ProgrammingLanguageUtils:
    """
    Utility class for handling programming language conversions and metadata.

    This class centralizes all language-related operations including:
    - Converting between string identifiers and ProgrammingLanguage enum values
    - Getting display names for languages
    - Getting file extensions for languages
    - Detecting language from file extensions
    """

    # Logger for the class
    _logger = logging.getLogger("LanguageUtils")

    # Mapping from lowercase language names to enum members
    _NAME_TO_LANGUAGE: Dict[str, ProgrammingLanguage] = {
        "aifpl": ProgrammingLanguage.MENAI,
        "bash": ProgrammingLanguage.BASH,
        "c": ProgrammingLanguage.C,
        "c++": ProgrammingLanguage.CPP,
        "cpp": ProgrammingLanguage.CPP,
        "cs": ProgrammingLanguage.CSHARP,
        "csharp": ProgrammingLanguage.CSHARP,
        "css": ProgrammingLanguage.CSS,
        "go": ProgrammingLanguage.GO,
        "diff": ProgrammingLanguage.DIFF,
        "fish": ProgrammingLanguage.BASH,
        "html": ProgrammingLanguage.HTML,
        "java": ProgrammingLanguage.JAVA,
        "javascript": ProgrammingLanguage.JAVASCRIPT,
        "js": ProgrammingLanguage.JAVASCRIPT,
        "json": ProgrammingLanguage.JSON,
        "lua": ProgrammingLanguage.LUA,
        "kotlin": ProgrammingLanguage.KOTLIN,
        "kt": ProgrammingLanguage.KOTLIN,
        "markdown": ProgrammingLanguage.MARKDOWN,
        "md": ProgrammingLanguage.MARKDOWN,
        "menai": ProgrammingLanguage.MENAI,
        "metaphor": ProgrammingLanguage.METAPHOR,
        "m6r": ProgrammingLanguage.METAPHOR,
        "move": ProgrammingLanguage.MOVE,
        "patch": ProgrammingLanguage.DIFF,
        "php": ProgrammingLanguage.PHP,
        "plaintext": ProgrammingLanguage.TEXT,
        "python": ProgrammingLanguage.PYTHON,
        "py": ProgrammingLanguage.PYTHON,
        "rb": ProgrammingLanguage.RUBY,
        "ruby": ProgrammingLanguage.RUBY,
        "rust": ProgrammingLanguage.RUST,
        "rs": ProgrammingLanguage.RUST,
        "scheme": ProgrammingLanguage.SCHEME,
        "scm": ProgrammingLanguage.SCHEME,
        "sh": ProgrammingLanguage.BASH,
        "sol": ProgrammingLanguage.SOLIDITY,
        "solidity": ProgrammingLanguage.SOLIDITY,
        "swift": ProgrammingLanguage.SWIFT,
        "toml": ProgrammingLanguage.TOML,
        "typescript": ProgrammingLanguage.TYPESCRIPT,
        "ts": ProgrammingLanguage.TYPESCRIPT,
        "text": ProgrammingLanguage.TEXT,
        "txt": ProgrammingLanguage.TEXT,
        "xml": ProgrammingLanguage.XML,
        "yaml": ProgrammingLanguage.YAML,
        "yml": ProgrammingLanguage.YAML,
        "zsh": ProgrammingLanguage.BASH,

        # Empty string defaults to text
        "": ProgrammingLanguage.TEXT
    }

    # Mapping from enum members to lowercase language names
    _LANGUAGE_TO_NAME: Dict[ProgrammingLanguage, str] = {
        ProgrammingLanguage.BASH: "bash",
        ProgrammingLanguage.C: "c",
        ProgrammingLanguage.CPP: "cpp",
        ProgrammingLanguage.CSHARP: "csharp",
        ProgrammingLanguage.CSS: "css",
        ProgrammingLanguage.GO: "go",
        ProgrammingLanguage.DIFF: "diff",
        ProgrammingLanguage.HTML: "html",
        ProgrammingLanguage.JAVA: "java",
        ProgrammingLanguage.JAVASCRIPT: "javascript",
        ProgrammingLanguage.JSON: "json",
        ProgrammingLanguage.KOTLIN: "kotlin",
        ProgrammingLanguage.LUA: "lua",
        ProgrammingLanguage.MARKDOWN: "markdown",
        ProgrammingLanguage.MENAI: "menai",
        ProgrammingLanguage.METAPHOR: "metaphor",
        ProgrammingLanguage.MOVE: "move",
        ProgrammingLanguage.PHP: "php",
        ProgrammingLanguage.PYTHON: "python",
        ProgrammingLanguage.RUBY: "ruby",
        ProgrammingLanguage.RUST: "rust",
        ProgrammingLanguage.SCHEME: "scheme",
        ProgrammingLanguage.SOLIDITY: "solidity",
        ProgrammingLanguage.SWIFT: "swift",
        ProgrammingLanguage.TOML: "toml",
        ProgrammingLanguage.TYPESCRIPT: "typescript",
        ProgrammingLanguage.TEXT: "plaintext",
        ProgrammingLanguage.UNKNOWN: "",
        ProgrammingLanguage.XML: "xml",
        ProgrammingLanguage.YAML: "yaml",
    }

    # Mapping from file extensions to programming languages
    _EXTENSION_TO_LANGUAGE: Dict[str, ProgrammingLanguage] = {
        '.aifpl': ProgrammingLanguage.MENAI,
        '.bash': ProgrammingLanguage.BASH,
        '.c': ProgrammingLanguage.C,
        '.cc': ProgrammingLanguage.CPP,
        '.cfg': ProgrammingLanguage.TEXT,
        '.conf': ProgrammingLanguage.TEXT,
        '.conv': ProgrammingLanguage.JSON,
        '.cpp': ProgrammingLanguage.CPP,
        '.cs': ProgrammingLanguage.CSHARP,
        '.css': ProgrammingLanguage.CSS,
        '.csv': ProgrammingLanguage.TEXT,
        '.cxx': ProgrammingLanguage.CPP,
        '.diff': ProgrammingLanguage.DIFF,
        '.env': ProgrammingLanguage.TEXT,
        '.fish': ProgrammingLanguage.BASH,
        '.go': ProgrammingLanguage.GO,
        '.h': ProgrammingLanguage.C,
        '.hh': ProgrammingLanguage.CPP,
        '.hpp': ProgrammingLanguage.CPP,
        '.html': ProgrammingLanguage.HTML,
        '.htm': ProgrammingLanguage.HTML,
        '.hxx': ProgrammingLanguage.CPP,
        '.ini': ProgrammingLanguage.TEXT,
        '.java': ProgrammingLanguage.JAVA,
        '.js': ProgrammingLanguage.JAVASCRIPT,
        '.json': ProgrammingLanguage.JSON,
        '.jsx': ProgrammingLanguage.JAVASCRIPT,
        '.kt': ProgrammingLanguage.KOTLIN,
        '.kts': ProgrammingLanguage.KOTLIN,
        '.lua': ProgrammingLanguage.LUA,
        '.m6r': ProgrammingLanguage.METAPHOR,
        '.md': ProgrammingLanguage.MARKDOWN,
        '.menai': ProgrammingLanguage.MENAI,
        '.move': ProgrammingLanguage.MOVE,
        '.patch': ProgrammingLanguage.DIFF,
        '.php': ProgrammingLanguage.PHP,
        '.py': ProgrammingLanguage.PYTHON,
        '.pyw': ProgrammingLanguage.PYTHON,
        '.pyi': ProgrammingLanguage.PYTHON,
        '.rb': ProgrammingLanguage.RUBY,
        '.rst': ProgrammingLanguage.TEXT,
        '.rs': ProgrammingLanguage.RUST,
        '.scm': ProgrammingLanguage.SCHEME,
        '.sh': ProgrammingLanguage.BASH,
        '.sol': ProgrammingLanguage.SOLIDITY,
        '.swift': ProgrammingLanguage.SWIFT,
        '.toml': ProgrammingLanguage.TOML,
        '.ts': ProgrammingLanguage.TYPESCRIPT,
        '.tsx': ProgrammingLanguage.TYPESCRIPT,
        '.txt': ProgrammingLanguage.TEXT,
        '.xml': ProgrammingLanguage.XML,
        '.yaml': ProgrammingLanguage.YAML,
        '.yml': ProgrammingLanguage.YAML,
        '.zsh': ProgrammingLanguage.BASH,
    }

    # Mapping from programming languages to display names
    _LANGUAGE_TO_DISPLAY_NAME: Dict[ProgrammingLanguage, str] = {
        ProgrammingLanguage.BASH: "Bash",
        ProgrammingLanguage.C: "C",
        ProgrammingLanguage.CPP: "C++",
        ProgrammingLanguage.CSHARP: "C#",
        ProgrammingLanguage.CSS: "CSS",
        ProgrammingLanguage.GO: "Go",
        ProgrammingLanguage.DIFF: "Diff",
        ProgrammingLanguage.HTML: "HTML",
        ProgrammingLanguage.JAVA: "Java",
        ProgrammingLanguage.JAVASCRIPT: "JavaScript",
        ProgrammingLanguage.JSON: "JSON",
        ProgrammingLanguage.KOTLIN: "Kotlin",
        ProgrammingLanguage.LUA: "Lua",
        ProgrammingLanguage.MARKDOWN: "Markdown",
        ProgrammingLanguage.MENAI: "Menai",
        ProgrammingLanguage.METAPHOR: "Metaphor",
        ProgrammingLanguage.MOVE: "Move",
        ProgrammingLanguage.PHP: "PHP",
        ProgrammingLanguage.PYTHON: "Python",
        ProgrammingLanguage.RUBY: "Ruby",
        ProgrammingLanguage.RUST: "Rust",
        ProgrammingLanguage.SCHEME: "Scheme",
        ProgrammingLanguage.SOLIDITY: "Solidity",
        ProgrammingLanguage.SWIFT: "Swift",
        ProgrammingLanguage.TOML: "TOML",
        ProgrammingLanguage.TYPESCRIPT: "TypeScript",
        ProgrammingLanguage.TEXT: "None",
        ProgrammingLanguage.UNKNOWN: "Unknown",
        ProgrammingLanguage.XML: "XML",
        ProgrammingLanguage.YAML: "YAML",
    }

    # Mapping from programming languages to file extensions (with leading dot)
    _LANGUAGE_TO_EXTENSION: Dict[ProgrammingLanguage, str] = {
        ProgrammingLanguage.BASH: ".sh",
        ProgrammingLanguage.C: ".c",
        ProgrammingLanguage.CPP: ".cpp",
        ProgrammingLanguage.CSHARP: ".cs",
        ProgrammingLanguage.CSS: ".css",
        ProgrammingLanguage.GO: ".go",
        ProgrammingLanguage.DIFF: ".diff",
        ProgrammingLanguage.HTML: ".html",
        ProgrammingLanguage.JAVA: ".java",
        ProgrammingLanguage.JAVASCRIPT: ".js",
        ProgrammingLanguage.JSON: ".json",
        ProgrammingLanguage.KOTLIN: ".kt",
        ProgrammingLanguage.LUA: ".lua",
        ProgrammingLanguage.MARKDOWN: ".md",
        ProgrammingLanguage.MENAI: ".menai",
        ProgrammingLanguage.METAPHOR: ".m6r",
        ProgrammingLanguage.MOVE: ".move",
        ProgrammingLanguage.PHP: ".php",
        ProgrammingLanguage.PYTHON: ".py",
        ProgrammingLanguage.RUBY: ".rb",
        ProgrammingLanguage.RUST: ".rs",
        ProgrammingLanguage.SCHEME: ".scm",
        ProgrammingLanguage.SOLIDITY: ".sol",
        ProgrammingLanguage.SWIFT: ".swift",
        ProgrammingLanguage.TOML: ".toml",
        ProgrammingLanguage.TYPESCRIPT: ".ts",
        ProgrammingLanguage.TEXT: ".txt",
        ProgrammingLanguage.UNKNOWN: ".txt",
        ProgrammingLanguage.XML: ".xml",
        ProgrammingLanguage.YAML: ".yaml",
    }

    @classmethod
    def get_all_programming_languages(cls) -> List[ProgrammingLanguage]:
        """
        Get a list of all available programming languages.

        Returns:
            List of all programming language enum values excluding UNKNOWN
        """
        return [lang for lang in ProgrammingLanguage if lang != ProgrammingLanguage.UNKNOWN]

    @classmethod
    def get_all_display_names(cls) -> List[Tuple[ProgrammingLanguage, str]]:
        """
        Get all language display names.

        Returns:
            List of (language enum, display name) tuples
        """
        return [(lang, cls.get_display_name(lang)) for lang in cls.get_all_programming_languages()]

    @classmethod
    def from_name(cls, name: str) -> ProgrammingLanguage:
        """
        Convert a language name string to a ProgrammingLanguage enum value.

        Args:
            name: The name of the programming language

        Returns:
            The corresponding ProgrammingLanguage enum value,
            or ProgrammingLanguage.TEXT if not found
        """
        if not name:
            return ProgrammingLanguage.TEXT

        normalized = name.strip().lower()
        return cls._NAME_TO_LANGUAGE.get(normalized, ProgrammingLanguage.TEXT)

    @classmethod
    def from_file_extension(cls, filename: str | None) -> ProgrammingLanguage:
        """
        Detect programming language from file extension.

        Args:
            filename: Path to file or None

        Returns:
            The detected programming language enum value,
            or ProgrammingLanguage.TEXT if not detected
        """
        if not filename:
            return ProgrammingLanguage.TEXT

        ext = os.path.splitext(filename)[1].lower()
        return cls._EXTENSION_TO_LANGUAGE.get(ext, ProgrammingLanguage.TEXT)

    @classmethod
    def get_name(cls, language: ProgrammingLanguage) -> str:
        """
        Get the human-readable lower-case name for a programming language.

        Args:
            language: The programming language enum value

        Returns:
            Human-readable name
        """
        return cls._LANGUAGE_TO_NAME.get(language, "")

    @classmethod
    def get_display_name(cls, language: ProgrammingLanguage) -> str:
        """
        Get the human-readable display name for a programming language.

        Args:
            language: The programming language enum value

        Returns:
            Human-readable language name for display
        """
        return cls._LANGUAGE_TO_DISPLAY_NAME.get(language, "Code")

    @classmethod
    def get_file_extension(cls, language: ProgrammingLanguage | None) -> str:
        """
        Get the file extension for a programming language.

        Args:
            language: The programming language or None

        Returns:
            The file extension with leading dot, or empty string if language is None
        """
        if language is None:
            return ""

        return cls._LANGUAGE_TO_EXTENSION.get(language, ".txt")

    @classmethod
    def get_supported_file_extensions(cls) -> List[str]:
        """
        Get a list of all supported file extensions.

        Returns:
            List of supported file extensions with leading dots
        """
        return list(cls._EXTENSION_TO_LANGUAGE.keys())
