"""Imports all parsers to ensure they are registered in the ParserRegistry."""

# pylint: disable=unused-import
from syntax.aifpl.aifpl_parser import AIFPLParser
from syntax.c.c_parser import CParser
from syntax.cpp.cpp_parser import CppParser
from syntax.csharp.csharp_parser import CSharpParser
from syntax.css.css_parser import CSSParser
from syntax.diff.diff_parser import DiffParser
from syntax.go.go_parser import GoParser
from syntax.html.html_parser import HTMLParser
from syntax.java.java_parser import JavaParser
from syntax.javascript.javascript_parser import JavaScriptParser
from syntax.json.json_parser import JSONParser
from syntax.kotlin.kotlin_parser import KotlinParser
from syntax.markdown.markdown_parser import MarkdownParser
from syntax.metaphor.metaphor_parser import MetaphorParser
from syntax.move.move_parser import MoveParser
from syntax.python.python_parser import PythonParser
from syntax.rust.rust_parser import RustParser
from syntax.scheme.scheme_parser import SchemeParser
from syntax.solidity.solidity_parser import SolidityParser
from syntax.swift.swift_parser import SwiftParser
from syntax.text.text_parser import TextParser
from syntax.typescript.typescript_parser import TypeScriptParser
from syntax.parser_registry import ParserRegistry
# pylint: enable=unused-import
