"""Imports all parsers to ensure they are registered in the ParserRegistry."""

# pylint: disable=unused-import
from humbug.lib.syntax.c.c_parser import CParser
from humbug.lib.syntax.cpp.cpp_parser import CppParser
from humbug.lib.syntax.csharp.csharp_parser import CSharpParser
from humbug.lib.syntax.css.css_parser import CSSParser
from humbug.lib.syntax.go.go_parser import GoParser
from humbug.lib.syntax.html.html_parser import HTMLParser
from humbug.lib.syntax.java.java_parser import JavaParser
from humbug.lib.syntax.javascript.javascript_parser import JavaScriptParser
from humbug.lib.syntax.json.json_parser import JSONParser
from humbug.lib.syntax.kotlin.kotlin_parser import KotlinParser
from humbug.lib.syntax.markdown.markdown_parser import MarkdownParser
from humbug.lib.syntax.metaphor.metaphor_parser import MetaphorParser
from humbug.lib.syntax.move.move_parser import MoveParser
from humbug.lib.syntax.python.python_parser import PythonParser
from humbug.lib.syntax.rust.rust_parser import RustParser
from humbug.lib.syntax.scheme.scheme_parser import SchemeParser
from humbug.lib.syntax.solidity.solidity_parser import SolidityParser
from humbug.lib.syntax.swift.swift_parser import SwiftParser
from humbug.lib.syntax.text.text_parser import TextParser
from humbug.lib.syntax.typescript.typescript_parser import TypeScriptParser
from humbug.lib.syntax.parser_registry import ParserRegistry
# pylint: enable=unused-import
