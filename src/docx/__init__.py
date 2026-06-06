from docx.docx_errors import DocxError, DocxExtractionError, DocxParseError, DocxUnsupportedError
from docx.docx_extractor import extract_text
from docx.docx_ast_parser import DocxASTParser, parse_docx
from docx.docx_ast_to_document_ir import docx_ast_to_document_ir
from docx.document_ir_to_docx_ast import document_ir_to_docx_ast
from docx.docx_ast_serialiser import serialise_docx
from docx.docx_ast_node import (
    DocxASTNode,
    DocxASTVisitor,
    # Document structure
    DocxASTDocumentNode,
    DocxASTBodyNode,
    DocxASTSectionPropertiesNode,
    # Paragraph-level
    DocxASTParagraphNode,
    DocxASTParagraphPropertiesNode,
    DocxASTNumberingPropertiesNode,
    # Run-level
    DocxASTRunNode,
    DocxASTRunPropertiesNode,
    DocxASTTextNode,
    DocxASTTabNode,
    DocxASTBreakNode,
    DocxASTLastRenderedPageBreakNode,
    # Bookmarks
    DocxASTBookmarkStartNode,
    DocxASTBookmarkEndNode,
    # Drawing
    DocxASTDrawingNode,
    # Hyperlinks
    DocxASTHyperlinkNode,
    # Tables
    DocxASTTableNode,
    DocxASTTablePropertiesNode,
    DocxASTTableGridNode,
    DocxASTTableRowNode,
    DocxASTTableRowPropertiesNode,
    DocxASTTableCellNode,
    DocxASTTableCellPropertiesNode,
    # Styles
    DocxASTStylesNode,
    DocxASTStyleNode,
    # Numbering
    DocxASTNumberingNode,
    DocxASTAbstractNumNode,
    DocxASTNumNode,
    DocxASTNumLevelNode,
)

__all__ = [
    # Errors
    "DocxError",
    "DocxParseError",
    "DocxUnsupportedError",
    "DocxExtractionError",
    # Text extraction (existing API)
    "extract_text",
    # AST parsing
    "DocxASTParser",
    "parse_docx",
    # document_ir mapping
    "docx_ast_to_document_ir",
    "document_ir_to_docx_ast",
    "serialise_docx",
    # AST base
    "DocxASTNode",
    "DocxASTVisitor",
    # Document structure
    "DocxASTDocumentNode",
    "DocxASTBodyNode",
    "DocxASTSectionPropertiesNode",
    # Paragraph-level
    "DocxASTParagraphNode",
    "DocxASTParagraphPropertiesNode",
    "DocxASTNumberingPropertiesNode",
    # Run-level
    "DocxASTRunNode",
    "DocxASTRunPropertiesNode",
    "DocxASTTextNode",
    "DocxASTTabNode",
    "DocxASTBreakNode",
    "DocxASTLastRenderedPageBreakNode",
    # Bookmarks
    "DocxASTBookmarkStartNode",
    "DocxASTBookmarkEndNode",
    # Drawing
    "DocxASTDrawingNode",
    "DocxASTHyperlinkNode",
    # Tables
    "DocxASTTableNode",
    "DocxASTTablePropertiesNode",
    "DocxASTTableGridNode",
    "DocxASTTableRowNode",
    "DocxASTTableRowPropertiesNode",
    "DocxASTTableCellNode",
    "DocxASTTableCellPropertiesNode",
    # Styles
    "DocxASTStylesNode",
    "DocxASTStyleNode",
    # Numbering
    "DocxASTNumberingNode",
    "DocxASTAbstractNumNode",
    "DocxASTNumNode",
    "DocxASTNumLevelNode",
]
