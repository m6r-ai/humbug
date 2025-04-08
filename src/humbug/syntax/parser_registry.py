from typing import Dict, Optional, Type, Callable

from humbug.syntax.parser import Parser
from humbug.syntax.programming_language import ProgrammingLanguage


class ParserRegistry:
    """
    A registry for parser classes that handles parser creation and dependency management.

    This registry serves as a central location for registering parser classes and
    creating parser instances, avoiding cyclic dependencies between parsers and the
    parser factory.

    The registry uses a lazy initialization pattern to avoid import cycles, allowing
    parser classes to be registered after the registry is created.
    """

    _instance = None
    _parser_classes: Dict[ProgrammingLanguage, Type[Parser]] = {}

    def __new__(cls) -> "ParserRegistry":
        """
        Implement the singleton pattern to ensure only one registry exists.

        Returns:
            The single ParserRegistry instance
        """
        if cls._instance is None:
            cls._instance = super(ParserRegistry, cls).__new__(cls)

        return cls._instance

    @classmethod
    def register_parser(cls, language: ProgrammingLanguage) -> Callable[[Type[Parser]], Type[Parser]]:
        """
        Register a parser class for a specific programming language.

        This is designed to be used as a decorator on parser classes.

        Args:
            language: The programming language enum value to register the parser for

        Returns:
            A decorator function that registers the parser class

        Example:
            @ParserRegistry.register_parser(ProgrammingLanguage.PYTHON)
            class PythonParser(Parser):
                ...
        """
        def decorator(parser_class: Type[Parser]) -> Type[Parser]:
            cls._parser_classes[language] = parser_class
            return parser_class

        return decorator

    @classmethod
    def create_parser(cls, language: ProgrammingLanguage) -> Optional[Parser]:
        """
        Create and return a parser instance for the specified programming language.

        Args:
            language: The programming language enum value indicating which parser to create

        Returns:
            A parser instance appropriate for the specified language, or None if the
            language is unknown or no parser is registered for it

        Example:
            >>> parser = ParserRegistry.create_parser(ProgrammingLanguage.PYTHON)
            >>> python_parser = parser.parse(None, "def example():")
        """
        parser_class = cls._parser_classes.get(language)
        if parser_class:
            return parser_class()

        return None
