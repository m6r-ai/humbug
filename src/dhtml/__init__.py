from dhtml.html_errors import HtmlError, HtmlExtractionError, HtmlParseError
from dhtml.html_ast_builder import parse_html
from dhtml.html_ast_to_document_ir import html_ast_to_document_ir
from dhtml.document_ir_to_html import document_ir_to_html
from dhtml.html_extractor import extract_text
from dhtml.html_ast_node import (
    HtmlASTNode,
    HtmlASTVisitor,
    HtmlASTDocumentNode,
    HtmlASTElementNode,
    HtmlASTTextNode,
    HtmlASTCommentNode,
)

__all__ = [
    # Errors
    "HtmlError",
    "HtmlParseError",
    "HtmlExtractionError",
    # API
    "parse_html",
    "html_ast_to_document_ir",
    "document_ir_to_html",
    "extract_text",
    # AST nodes
    "HtmlASTNode",
    "HtmlASTVisitor",
    "HtmlASTDocumentNode",
    "HtmlASTElementNode",
    "HtmlASTTextNode",
    "HtmlASTCommentNode",
]
