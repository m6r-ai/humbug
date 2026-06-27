import logging
import tempfile
import os
from pathlib import Path
from typing import Any, Callable, Dict, Tuple

from ai_tool import (
    AITool, AIToolAuthorizationCallback, AIToolAuthorizationDenied,
    AIToolCall, AIToolDefinition, AIToolExecutionError,
    AIToolOperationDefinition, AIToolParameter, AIToolResult
)
from dhtml import (
    HtmlError, HtmlParseError, document_ir_to_html, html_ast_to_document_ir, parse_html
)
from dmarkdown.document_ir_to_markdown import document_ir_to_markdown
from dmarkdown.markdown_ast_builder import MarkdownASTBuilder
from dmarkdown.markdown_to_document_ir import markdown_ast_to_document_ir
from document_ir.document_ir_node import DocumentIRDocumentNode
from document_ir.image_sidecar import extract_images_to_sidecar
from docx import (
    DocxError, DocxUnsupportedError,
    docx_ast_to_document_ir, document_ir_to_docx_ast, parse_docx, serialise_docx
)
from mindspace.mindspace import Mindspace
from mindspace.mindspace_log_level import MindspaceLogLevel

# Map each supported format name to its canonical file extension.
_FORMAT_EXTENSIONS: Dict[str, str] = {
    "docx": ".docx",
    "html": ".html",
    "md": ".md",
}

# Map each file extension to its canonical format name.
_EXTENSION_FORMATS: Dict[str, str] = {v: k for k, v in _FORMAT_EXTENSIONS.items()}

# Also recognise the legacy .htm extension as HTML.
_EXTENSION_FORMATS[".htm"] = "html"

_SUPPORTED_FORMAT_NAMES = sorted(_FORMAT_EXTENSIONS.keys())


def _import_md(input_path: Path) -> DocumentIRDocumentNode:
    """Import a Markdown file to document_ir."""
    try:
        md_text = input_path.read_text(encoding="utf-8")

    except OSError as e:
        raise AIToolExecutionError(f"Failed to read input file: {e}") from e

    builder = MarkdownASTBuilder(True)
    builder.update_ast(md_text, "", str(input_path))
    return markdown_ast_to_document_ir(builder.document())


def _import_html(input_path: Path) -> DocumentIRDocumentNode:
    """Import an HTML file to document_ir."""
    try:
        source = input_path.read_text(encoding="utf-8")

    except OSError as e:
        raise AIToolExecutionError(f"Failed to read input file: {e}") from e

    try:
        html_doc = parse_html(source, source_path=str(input_path))
        return html_ast_to_document_ir(html_doc)

    except (HtmlParseError, HtmlError) as e:
        raise AIToolExecutionError(f"Failed to parse HTML: {e}") from e


def _import_docx(input_path: Path) -> DocumentIRDocumentNode:
    """Import a DOCX file to document_ir."""
    try:
        docx_bytes = input_path.read_bytes()

    except OSError as e:
        raise AIToolExecutionError(f"Failed to read input file: {e}") from e

    try:
        docx_ast = parse_docx(docx_bytes)

    except DocxUnsupportedError as e:
        raise AIToolExecutionError(f"Unsupported DOCX format: {e}") from e

    except DocxError as e:
        raise AIToolExecutionError(f"Failed to parse DOCX: {e}") from e

    return docx_ast_to_document_ir(docx_ast)


def _export_md(document_ir: DocumentIRDocumentNode) -> str:
    """Export document_ir to Markdown text."""
    return document_ir_to_markdown(document_ir)


def _export_html(document_ir: DocumentIRDocumentNode) -> str:
    """Export document_ir to HTML text."""
    return document_ir_to_html(document_ir)


def _export_docx(document_ir: DocumentIRDocumentNode) -> bytes:
    """Export document_ir to DOCX bytes."""
    return serialise_docx(document_ir_to_docx_ast(document_ir))


# Importers: format name → callable that reads a file and returns a DocumentIRDocumentNode.
_IMPORTERS: Dict[str, Callable[[Path], DocumentIRDocumentNode]] = {
    "docx": _import_docx,
    "html": _import_html,
    "md": _import_md,
}

# Exporters: format name → callable that serialises a DocumentIRDocumentNode to bytes or str.
_EXPORTERS: Dict[str, Callable[[DocumentIRDocumentNode], bytes | str]] = {
    "docx": _export_docx,
    "html": _export_html,
    "md": _export_md,
}


# Formats that use text files and need embedded images extracted to a sidecar directory.
_SIDECAR_FORMATS = frozenset({"html", "md"})


def _resolve_format_from_extension(path: Path) -> str | None:
    """Return the format name for a path's extension, or None if unrecognised."""
    return _EXTENSION_FORMATS.get(path.suffix.lower())


class DocumentConverterAITool(AITool):
    """
    AI tool for converting documents between supported formats via the document_ir intermediate
    representation.

    Every supported format provides an importer (source → document_ir) and an exporter
    (document_ir → target), so any pair of formats is automatically supported without
    writing per-pair conversion code.

    Write operations are restricted to the current mindspace and require user
    authorization. The output file is written atomically via a temporary file.
    """

    def __init__(
        self,
        resolve_path: Callable[[str], Tuple[Path, str]],
        mindspace: Mindspace,
    ) -> None:
        """
        Initialize the document converter tool.

        Args:
            resolve_path: Callback that resolves a path string to
                          (absolute_path, display_path), raising ValueError
                          if the path falls outside the mindspace.
            mindspace: The active mindspace, used for audit logging.
        """
        self._resolve_path = resolve_path
        self._mindspace = mindspace
        self._logger = logging.getLogger("DocumentConverterAITool")

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        return self._build_definition_from_operations(
            name="document_converter",
            description_prefix=(
                "The document_converter tool lets you (the AI) convert documents between "
                "supported formats via a common intermediate representation. "
                "Both input and output paths must be inside the current mindspace. "
                "Write operations require user authorization. "
                "The tool logs every conversion it performs. "
                f"Supported formats: {', '.join(_SUPPORTED_FORMAT_NAMES)}."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="input_path",
                    type="string",
                    description="Path to the input file (relative to mindspace root or absolute within mindspace)",
                    required=True,
                ),
                AIToolParameter(
                    name="output_path",
                    type="string",
                    description=(
                        "Path for the output file (relative to mindspace root or absolute within mindspace). "
                        "If omitted the output is placed alongside the input with the appropriate extension."
                    ),
                    required=False,
                ),
                AIToolParameter(
                    name="from_format",
                    type="string",
                    description=(
                        "Format of the input file. "
                        "If omitted it is inferred from the input file's extension."
                    ),
                    required=False,
                    enum=_SUPPORTED_FORMAT_NAMES,
                ),
                AIToolParameter(
                    name="to_format",
                    type="string",
                    description=(
                        "Format of the output file. "
                        "If omitted it is inferred from the output file's extension, "
                        "or from to_format if output_path is also omitted."
                    ),
                    required=False,
                    enum=_SUPPORTED_FORMAT_NAMES,
                ),
            ],
        )

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return "Document conversion between supported formats (mindspace only)."

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """Get operation definitions for this tool."""
        return {
            "convert": AIToolOperationDefinition(
                name="convert",
                handler=self._convert,
                extract_context=None,
                allowed_parameters={"input_path", "output_path", "from_format", "to_format"},
                required_parameters={"input_path"},
                description=(
                    "Convert a document from one format to another. "
                    "The input file must exist inside the mindspace. "
                    "The output file will be written inside the mindspace, requiring user authorization. "
                    "Formats are inferred from file extensions when not explicitly specified."
                ),
            ),
        }

    def _resolve_mindspace_path(self, key: str, path_str: str) -> Tuple[Path, str]:
        """
        Resolve and validate a path, ensuring it is inside the mindspace.

        Args:
            key: Parameter name (used in error messages).
            path_str: Raw path string from the tool call arguments.

        Returns:
            Tuple of (resolved absolute Path, display path string).

        Raises:
            AIToolExecutionError: If the path is empty or outside the mindspace.
        """
        if not path_str:
            raise AIToolExecutionError(f"'{key}': path must not be empty")

        try:
            return self._resolve_path(path_str)

        except ValueError as e:
            raise AIToolExecutionError(
                f"'{key}': path '{path_str}' is outside the mindspace: {e}"
            ) from e

        except Exception as e:
            raise AIToolExecutionError(
                f"'{key}': failed to resolve path '{path_str}': {e}"
            ) from e

    def _determine_formats(
        self,
        input_path: Path,
        output_path: Path | None,
        from_format_arg: str | None,
        to_format_arg: str | None,
    ) -> Tuple[str, str]:
        """
        Determine the from/to format pair, raising on ambiguity or unsupported formats.

        Returns:
            Tuple of (from_format, to_format) strings.

        Raises:
            AIToolExecutionError: If formats cannot be determined or are unsupported.
        """
        if from_format_arg:
            from_format = from_format_arg

        else:
            detected = _resolve_format_from_extension(input_path)
            if detected is None:
                raise AIToolExecutionError(
                    f"Cannot determine input format from extension '{input_path.suffix}'. "
                    f"Please specify 'from_format'. Supported formats: {', '.join(_SUPPORTED_FORMAT_NAMES)}."
                )

            from_format = detected

        if to_format_arg:
            to_format = to_format_arg

        elif output_path is not None:
            detected = _resolve_format_from_extension(output_path)
            if detected is None:
                raise AIToolExecutionError(
                    f"Cannot determine output format from extension '{output_path.suffix}'. "
                    f"Please specify 'to_format'. Supported formats: {', '.join(_SUPPORTED_FORMAT_NAMES)}."
                )

            to_format = detected

        else:
            raise AIToolExecutionError(
                "Cannot determine output format: neither 'to_format' nor 'output_path' was provided. "
                "Please specify at least one."
            )

        if from_format == to_format:
            raise AIToolExecutionError(
                f"Input and output formats are both '{from_format}'; no conversion needed."
            )

        if from_format not in _IMPORTERS:
            raise AIToolExecutionError(
                f"No importer available for format '{from_format}'. "
                f"Supported formats: {', '.join(_SUPPORTED_FORMAT_NAMES)}."
            )

        if to_format not in _EXPORTERS:
            raise AIToolExecutionError(
                f"No exporter available for format '{to_format}'. "
                f"Supported formats: {', '.join(_SUPPORTED_FORMAT_NAMES)}."
            )

        return from_format, to_format

    def _default_output_path(self, input_path: Path, to_format: str) -> Path:
        """Return the default output path: input stem + target extension, same directory."""
        return input_path.with_suffix(_FORMAT_EXTENSIONS[to_format])

    def _write_output(self, output_path: Path, content: bytes | str) -> int:
        """
        Write output content atomically via a temporary file.

        Args:
            output_path: Destination path.
            content: Bytes or str to write.

        Returns:
            Number of bytes written.

        Raises:
            AIToolExecutionError: On any I/O failure.
        """
        is_binary = isinstance(content, bytes)
        try:
            with tempfile.NamedTemporaryFile(
                mode="wb" if is_binary else "w",
                encoding=None if is_binary else "utf-8",
                dir=output_path.parent,
                delete=False,
                suffix=".tmp",
            ) as tmp:
                tmp.write(content)
                tmp_path = Path(tmp.name)

            tmp_path.replace(output_path)

            umask = os.umask(0)
            os.umask(umask)
            output_path.chmod(0o666 & ~umask)

        except PermissionError as e:
            raise AIToolExecutionError(f"Permission denied writing output file: {e}") from e

        except OSError as e:
            raise AIToolExecutionError(f"Failed to write output file: {e}") from e

        return len(content) if isinstance(content, bytes) else len(content.encode("utf-8"))

    async def _convert(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback,
    ) -> AIToolResult:
        """Handle the convert operation."""
        arguments = tool_call.arguments

        input_path_str = self._get_required_str_value("input_path", arguments)
        output_path_str = self._get_optional_str_value("output_path", arguments)
        from_format_arg = self._get_optional_str_value("from_format", arguments)
        to_format_arg = self._get_optional_str_value("to_format", arguments)

        input_path, input_display = self._resolve_mindspace_path("input_path", input_path_str)
        if not input_path.exists():
            raise AIToolExecutionError(f"Input file not found: '{input_display}'")

        if not input_path.is_file():
            raise AIToolExecutionError(f"Input path is not a file: '{input_display}'")

        explicit_output_path: Path | None = None
        explicit_output_display: str | None = None
        if output_path_str:
            explicit_output_path, explicit_output_display = self._resolve_mindspace_path(
                "output_path", output_path_str
            )

        from_format, to_format = self._determine_formats(
            input_path, explicit_output_path, from_format_arg, to_format_arg
        )

        if explicit_output_path is not None:
            output_path = explicit_output_path
            output_display = explicit_output_display or str(explicit_output_path)

        else:
            output_path = self._default_output_path(input_path, to_format)
            try:
                _, output_display = self._resolve_path(str(output_path))

            except Exception:
                output_display = str(output_path)

        if output_path.exists():
            auth_context = (
                f"This will convert '{input_display}' ({from_format}) to '{output_display}' ({to_format}). "
                f"The output file already exists and will be overwritten."
            )
            destructive = True

        else:
            auth_context = (
                f"This will convert '{input_display}' ({from_format}) to '{output_display}' ({to_format}). "
                f"A new file will be created."
            )
            destructive = False

        authorized = await request_authorization(
            "document_converter", arguments, auth_context, None, destructive
        )
        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to write '{output_display}'"
            )

        try:
            document_ir = _IMPORTERS[from_format](input_path)

            if to_format in _SIDECAR_FORMATS:
                sidecar_dir = output_path.parent / f"{output_path.stem}_files"
                extract_images_to_sidecar(
                    document_ir,
                    sidecar_dir,
                )

            content = _EXPORTERS[to_format](document_ir)

        except AIToolExecutionError:
            raise

        except Exception as e:
            raise AIToolExecutionError(f"Conversion failed: {e}") from e

        try:
            output_path.parent.mkdir(parents=True, exist_ok=True)

        except OSError as e:
            raise AIToolExecutionError(f"Failed to create output directory: {e}") from e

        byte_count = self._write_output(output_path, content)

        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"AI converted '{input_display}' ({from_format}) → '{output_display}' ({to_format}) "
            f"({byte_count:,} bytes written)",
        )

        return AIToolResult(
            id=tool_call.id,
            name="document_converter",
            content=(
                f"Converted '{input_display}' ({from_format}) → '{output_display}' ({to_format}). "
                f"{byte_count:,} bytes written."
            ),
        )
