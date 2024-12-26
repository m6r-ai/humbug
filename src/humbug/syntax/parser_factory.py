from typing import Optional

from humbug.syntax.parser import Parser
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.c_parser import CParser
from humbug.syntax.cpp_parser import CppParser
from humbug.syntax.css_parser import CSSParser
from humbug.syntax.html_parser import HTMLParser
from humbug.syntax.javascript_parser import JavaScriptParser
from humbug.syntax.metaphor_parser import MetaphorParser
from humbug.syntax.python_parser import PythonParser
from humbug.syntax.text_parser import TextParser
from humbug.syntax.typescript_parser import TypeScriptParser


class ParserFactory:
    """
    Factory class for creating parser instances based on programming language.

    This factory provides a centralized way to instantiate the appropriate parser
    for a given programming language, abstracting away the specific parser class
    implementations from the calling code.
    """

    @staticmethod
    def create_parser(language: ProgrammingLanguage) -> Optional[Parser]:
        """
        Create and return a parser instance for the specified programming language.

        Args:
            language: The programming language enum value indicating which parser to create

        Returns:
            A parser instance appropriate for the specified language, or None if the
            language is unknown

        Example:
            >>> parser = ParserFactory.create_parser(ProgrammingLanguage.PYTHON)
            >>> python_parser = parser.parse(None, "def example():")
        """
        parser_map = {
            ProgrammingLanguage.C: CParser,
            ProgrammingLanguage.CPP: CppParser,
            ProgrammingLanguage.CSS: CSSParser,
            ProgrammingLanguage.HTML: HTMLParser,
            ProgrammingLanguage.JAVASCRIPT: JavaScriptParser,
            ProgrammingLanguage.METAPHOR: MetaphorParser,
            ProgrammingLanguage.PYTHON: PythonParser,
            ProgrammingLanguage.TEXT: TextParser,
            ProgrammingLanguage.TYPESCRIPT: TypeScriptParser
        }

        parser_class = parser_map.get(language)
        if parser_class:
            return parser_class()

        return None
