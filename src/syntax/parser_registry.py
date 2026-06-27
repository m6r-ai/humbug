from collections.abc import Callable

from syntax.parser import Parser
from syntax.programming_language import ProgrammingLanguage


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
    _parser_classes: dict[ProgrammingLanguage, type[Parser]] = {}

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
    def register_parser(cls, language: ProgrammingLanguage) -> Callable[[type[Parser]], type[Parser]]:
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
        def decorator(parser_class: type[Parser]) -> type[Parser]:
            cls._parser_classes[language] = parser_class
            return parser_class

        return decorator

    @classmethod
    def create_parser(cls, language: ProgrammingLanguage) -> Parser | None:
        """
        Create and return a parser instance for the specified programming language.

        Args:
            language: The programming language enum value indicating which parser to create

        Returns:
            A parser instance appropriate for the specified language. If no dedicated
            parser is registered, falls back to the TEXT parser for any known language.
            Returns None only if the language is UNKNOWN or no TEXT parser is registered.

        Example:
            >>> parser = ParserRegistry.create_parser(ProgrammingLanguage.PYTHON)
            >>> python_parser = parser.parse(None, "def example():")
        """
        parser_class = cls._parser_classes.get(language)
        if parser_class:
            return parser_class()

        # Fall back to TEXT parser for known languages without a dedicated parser yet.
        # UNKNOWN is intentionally excluded — no parser is the correct result there.
        if language != ProgrammingLanguage.UNKNOWN:
            text_parser_class = cls._parser_classes.get(ProgrammingLanguage.TEXT)
            if text_parser_class:
                return text_parser_class()

        return None
