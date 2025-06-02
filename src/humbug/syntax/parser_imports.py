"""Imports all parsers to ensure they are registered in the ParserRegistry."""

# pylint: disable=unused-import
from humbug.syntax.c.c_parser import CParser
from humbug.syntax.cpp.cpp_parser import CppParser
from humbug.syntax.csharp.csharp_parser import CSharpParser
from humbug.syntax.css.css_parser import CSSParser
from humbug.syntax.go.go_parser import GoParser
from humbug.syntax.html.html_parser import HTMLParser
from humbug.syntax.java.java_parser import JavaParser
from humbug.syntax.javascript.javascript_parser import JavaScriptParser
from humbug.syntax.json.json_parser import JSONParser
from humbug.syntax.kotlin.kotlin_parser import KotlinParser
from humbug.syntax.markdown.markdown_parser import MarkdownParser
from humbug.syntax.metaphor.metaphor_parser import MetaphorParser
from humbug.syntax.move.move_parser import MoveParser
from humbug.syntax.python.python_parser import PythonParser
from humbug.syntax.rust.rust_parser import RustParser
from humbug.syntax.scheme.scheme_parser import SchemeParser
from humbug.syntax.solidity.solidity_parser import SolidityParser
from humbug.syntax.swift.swift_parser import SwiftParser
from humbug.syntax.text.text_parser import TextParser
from humbug.syntax.typescript.typescript_parser import TypeScriptParser
from humbug.syntax.parser_registry import ParserRegistry
# pylint: enable=unused-import
