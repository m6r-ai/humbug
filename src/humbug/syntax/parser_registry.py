from typing import Dict, Optional, Type, List, Deque
from collections import deque

from humbug.syntax.parser import Parser
from humbug.syntax.programming_language import ProgrammingLanguage


class ParserRegistry:
    """
    A registry for parser classes that handles parser creation, caching, and dependency management.

    This registry serves as a central location for registering parser classes and
    creating parser instances, avoiding cyclic dependencies between parsers and the
    parser factory.

    The registry uses a lazy initialization pattern to avoid import cycles, allowing
    parser classes to be registered after the registry is created.

    Attributes:
        _instance: Singleton instance of the registry
        _parser_classes: Mapping of programming languages to their parser classes
        _parser_cache: Cache of parser instances for each programming language
        _cache_size: Maximum number of parser instances to cache per language
    """

    _instance = None
    _parser_classes: Dict[ProgrammingLanguage, Type[Parser]] = {}
    _parser_cache: Dict[ProgrammingLanguage, Deque[Parser]] = {}
    _cache_size: int = 4  # Default cache size per language

    def __new__(cls):
        """
        Implement the singleton pattern to ensure only one registry exists.

        Returns:
            The single ParserRegistry instance
        """
        if cls._instance is None:
            cls._instance = super(ParserRegistry, cls).__new__(cls)
            cls._initialize_cache()

        return cls._instance

    @classmethod
    def _initialize_cache(cls):
        """
        Initialize the parser cache for all registered languages.
        """
        for language in cls._parser_classes:
            if language not in cls._parser_cache:
                cls._parser_cache[language] = deque(maxlen=cls._cache_size)

    @classmethod
    def set_cache_size(cls, size: int) -> None:
        """
        Set the maximum number of parser instances to cache per language.

        Args:
            size: The maximum number of parser instances to cache per language

        Note:
            Setting a larger cache size may improve performance at the cost of memory usage.
            Setting a size of 0 disables caching.
        """
        if size < 0:
            raise ValueError("Cache size cannot be negative")

        cls._cache_size = size

        # Resize existing caches
        for language in cls._parser_cache:
            cached_parsers = list(cls._parser_cache[language])
            if size == 0:
                cls._parser_cache[language] = deque(maxlen=0)
            else:
                cls._parser_cache[language] = deque(maxlen=size)
                # Preserve the most recently used parsers up to the new size
                for parser in cached_parsers[-size:]:
                    cls._parser_cache[language].append(parser)

    @classmethod
    def register_parser(cls, language: ProgrammingLanguage) -> callable:
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
            # Initialize cache for this language
            if language not in cls._parser_cache:
                cls._parser_cache[language] = deque(maxlen=cls._cache_size)
            return parser_class

        return decorator

    @classmethod
    def create_parser(cls, language: ProgrammingLanguage) -> Optional[Parser]:
        """
        Create and return a parser instance for the specified programming language.

        If a cached parser is available, it will be returned. Otherwise, a new parser
        will be created and potentially cached for future use.

        Args:
            language: The programming language enum value indicating which parser to create

        Returns:
            A parser instance appropriate for the specified language, or None if the
            language is unknown or no parser is registered for it

        Example:
            >>> parser = ParserRegistry.create_parser(ProgrammingLanguage.PYTHON)
            >>> python_parser = parser.parse(None, "def example():")
        """
        # If caching is disabled, always create a new parser
        if cls._cache_size == 0:
            parser_class = cls._parser_classes.get(language)
            return parser_class() if parser_class else None

        # Check if we have a cached parser for this language
        parser_cache = cls._parser_cache.get(language, deque(maxlen=cls._cache_size))

        # Try to get a parser from the cache
        if parser_cache:
            return parser_cache.pop()  # Remove and return the most recently used parser (LIFO)

        # Create a new parser if the cache is empty
        parser_class = cls._parser_classes.get(language)
        if parser_class:
            return parser_class()

        return None

    @classmethod
    def release_parser(cls, language: ProgrammingLanguage, parser: Parser) -> None:
        """
        Return a parser to the cache for later reuse.

        Args:
            language: The programming language associated with the parser
            parser: The parser instance to cache

        Note:
            If the cache for this language is full, the oldest parser instance will be
            discarded to make room for this one.
        """
        if cls._cache_size == 0:
            return  # Caching is disabled

        if language not in cls._parser_cache:
            cls._parser_cache[language] = deque(maxlen=cls._cache_size)

        # Add the parser back to the cache
        cls._parser_cache[language].append(parser)

    @classmethod
    def clear_cache(cls) -> None:
        """
        Clear all cached parser instances.

        This can be useful when changing application settings that might affect parser behavior,
        or when memory needs to be freed.
        """
        for language in cls._parser_cache:
            cls._parser_cache[language].clear()
