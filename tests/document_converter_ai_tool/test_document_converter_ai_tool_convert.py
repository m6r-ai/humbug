"""Tests for DocumentConverterAITool convert operation behaviour."""

import asyncio
from pathlib import Path
from unittest.mock import MagicMock, patch, call

import pytest

from ai_tool import AIToolExecutionError, AIToolAuthorizationDenied
from mindspace.mindspace_log_level import MindspaceLogLevel


class TestConvertFormatDetection:
    """Tests for automatic format detection from file extensions."""

    def test_detects_md_input_format_from_extension(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """Input format is inferred as 'md' from a .md extension."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: b"fake docx")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/doc.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "to_format": "docx",
            })
            result = asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))
            assert "md" in result.content
            assert "docx" in result.content

    def test_detects_docx_input_format_from_extension(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """Input format is inferred as 'docx' from a .docx extension."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: "fake md")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/doc.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.docx",
                "to_format": "md",
            })
            result = asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))
            assert "docx" in result.content
            assert "md" in result.content

    def test_detects_output_format_from_output_path_extension(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """Output format is inferred from the output_path extension when to_format is absent."""
        with patch("pathlib.Path.exists", side_effect=[True, False]), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: b"fake docx")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/out.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            # exists() is called twice: once for input (must be True), once for output (False = new file)
            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "output_path": "out.docx",
            })
            result = asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))
            assert "docx" in result.content

    def test_unrecognised_input_extension_raises_error(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """An unrecognised input extension raises an error when from_format is absent."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True):

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.pdf",
                "to_format": "md",
            })
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

            assert "from_format" in str(exc_info.value)

    def test_unrecognised_output_extension_raises_error(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """An unrecognised output_path extension raises an error when to_format is absent."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True):

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "output_path": "out.html",
            })
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

            assert "to_format" in str(exc_info.value)

    def test_no_output_path_and_no_to_format_raises_error(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """Omitting both output_path and to_format raises an error."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True):

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
            })
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

            assert "to_format" in str(exc_info.value)

    def test_same_input_and_output_format_raises_error(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """Specifying identical from_format and to_format raises an error."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True):

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "from_format": "md",
                "to_format": "md",
            })
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

            assert "no conversion needed" in str(exc_info.value)

    def test_explicit_formats_override_extension_detection(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """Explicit from_format and to_format are used even when extensions differ."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: b"fake docx")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/doc.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            # .txt extension would normally be unrecognised, but explicit formats override
            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.txt",
                "from_format": "md",
                "to_format": "docx",
            })
            result = asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))
            assert "md" in result.content
            assert "docx" in result.content


class TestConvertPathValidation:
    """Tests for path validation in the convert operation."""

    def test_missing_input_path_raises_error(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """Omitting input_path raises an execution error."""
        tool_call = make_tool_call("document_converter", {
            "operation": "convert",
            "to_format": "docx",
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

        assert "input_path" in str(exc_info.value)

    def test_input_path_outside_mindspace_raises_error(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """An input path outside the mindspace raises an execution error."""
        tool_call = make_tool_call("document_converter", {
            "operation": "convert",
            "input_path": "../outside/doc.md",
            "to_format": "docx",
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

        assert "outside the mindspace" in str(exc_info.value)

    def test_output_path_outside_mindspace_raises_error(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """An output path outside the mindspace raises an execution error."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True):

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "output_path": "../outside/out.docx",
                "to_format": "docx",
            })
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

            assert "outside the mindspace" in str(exc_info.value)

    def test_input_file_not_found_raises_error(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """A non-existent input file raises an execution error."""
        with patch("pathlib.Path.exists", return_value=False):
            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "missing.md",
                "to_format": "docx",
            })
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

            assert "not found" in str(exc_info.value)

    def test_input_path_is_directory_raises_error(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """A directory supplied as input_path raises an execution error."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=False):

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "somedir.md",
                "to_format": "docx",
            })
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

            assert "not a file" in str(exc_info.value)


class TestConvertAuthorization:
    """Tests for authorization behaviour in the convert operation."""

    def test_authorization_is_requested_before_writing(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """Authorization is requested exactly once before the output file is written."""
        with patch("pathlib.Path.exists", side_effect=[True, False]), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: b"data")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/doc.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "to_format": "docx",
            })
            asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

        mock_authorization.assert_called_once()

    def test_authorization_denied_raises_error(
        self, converter_tool, mock_authorization_denied, make_tool_call
    ):
        """Denied authorization raises AIToolAuthorizationDenied."""
        with patch("pathlib.Path.exists", side_effect=[True, False]), \
             patch("pathlib.Path.is_file", return_value=True):

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "to_format": "docx",
            })
            with pytest.raises(AIToolAuthorizationDenied):
                asyncio.run(converter_tool.execute(tool_call, "", mock_authorization_denied))

    def test_overwrite_is_flagged_as_destructive(
        self, converter_tool, make_tool_call
    ):
        """When the output file already exists the destructive flag is True."""
        captured = {}

        async def capture_auth(_tool_name, _arguments, _context, _requester_ref, destructive):
            captured["destructive"] = destructive
            return True

        mock_auth = MagicMock()
        mock_auth.side_effect = capture_auth

        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: b"data")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/doc.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "to_format": "docx",
            })
            asyncio.run(converter_tool.execute(tool_call, "", mock_auth))

        assert captured["destructive"] is True

    def test_new_file_is_not_flagged_as_destructive(
        self, converter_tool, make_tool_call
    ):
        """When the output file does not exist the destructive flag is False."""
        captured = {}

        async def capture_auth(_tool_name, _arguments, _context, _requester_ref, destructive):
            captured["destructive"] = destructive
            return True

        mock_auth = MagicMock()
        mock_auth.side_effect = capture_auth

        with patch("pathlib.Path.exists", side_effect=[True, False]), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: b"data")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/doc.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "to_format": "docx",
            })
            asyncio.run(converter_tool.execute(tool_call, "", mock_auth))

        assert captured["destructive"] is False


class TestConvertLoggingAndResult:
    """Tests for audit logging and result content."""

    def test_successful_conversion_logs_interaction(
        self, converter_tool, mock_mindspace, mock_authorization, make_tool_call
    ):
        """A successful conversion logs an INFO interaction to the mindspace."""
        with patch("pathlib.Path.exists", side_effect=[True, False]), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: b"output")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/doc.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "to_format": "docx",
            })
            asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

        mock_mindspace.add_interaction.assert_called_once()
        log_level, log_message = mock_mindspace.add_interaction.call_args[0]
        assert log_level == MindspaceLogLevel.INFO
        assert "md" in log_message
        assert "docx" in log_message

    def test_result_content_includes_both_formats(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """The result content string names both the source and target format."""
        with patch("pathlib.Path.exists", side_effect=[True, False]), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: b"output")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/doc.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "to_format": "docx",
            })
            result = asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

        assert "md" in result.content
        assert "docx" in result.content
        assert "bytes" in result.content

    def test_result_name_is_document_converter(
        self, converter_tool, mock_authorization, make_tool_call
    ):
        """The AIToolResult name is 'document_converter'."""
        with patch("pathlib.Path.exists", side_effect=[True, False]), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters, \
             patch("pathlib.Path.parent", new_callable=lambda: property(lambda self: MagicMock())), \
             patch("tempfile.NamedTemporaryFile") as mock_tmp, \
             patch("pathlib.Path.replace"), \
             patch("pathlib.Path.chmod"):

            mock_document_ir = MagicMock()
            mock_importers.__getitem__ = lambda self, k: (lambda p: mock_document_ir)
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__getitem__ = lambda self, k: (lambda d: b"output")
            mock_exporters.__contains__ = lambda self, k: True

            mock_tmp_inst = MagicMock()
            mock_tmp_inst.name = "/test/mindspace/doc.tmp"
            mock_tmp_inst.__enter__ = lambda s: s
            mock_tmp_inst.__exit__ = MagicMock(return_value=False)
            mock_tmp.return_value = mock_tmp_inst

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "to_format": "docx",
            })
            result = asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

        assert result.name == "document_converter"

    def test_failed_conversion_does_not_log(
        self, converter_tool, mock_mindspace, mock_authorization, make_tool_call
    ):
        """A conversion that raises an error does not log an interaction."""
        with patch("pathlib.Path.exists", return_value=True), \
             patch("pathlib.Path.is_file", return_value=True), \
             patch("document_converter_ai_tool.document_converter_ai_tool._IMPORTERS") as mock_importers, \
             patch("document_converter_ai_tool.document_converter_ai_tool._EXPORTERS") as mock_exporters:

            mock_importers.__getitem__ = lambda self, k: (lambda p: (_ for _ in ()).throw(RuntimeError("parse error")))
            mock_importers.__contains__ = lambda self, k: True
            mock_exporters.__contains__ = lambda self, k: True

            tool_call = make_tool_call("document_converter", {
                "operation": "convert",
                "input_path": "doc.md",
                "to_format": "docx",
            })
            with pytest.raises(AIToolExecutionError):
                asyncio.run(converter_tool.execute(tool_call, "", mock_authorization))

        mock_mindspace.add_interaction.assert_not_called()
