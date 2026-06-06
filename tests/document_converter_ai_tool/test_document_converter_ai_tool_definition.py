"""Tests for DocumentConverterAITool definition and structure."""

from ai_tool import AITool, AIToolDefinition
from document_converter_ai_tool.document_converter_ai_tool import DocumentConverterAITool


class TestDocumentConverterAIToolDefinition:
    """Tests for the tool definition structure."""

    def test_inherits_from_ai_tool(self, converter_tool):
        """Tool is a proper AITool subclass."""
        assert isinstance(converter_tool, AITool)

    def test_get_definition_returns_ai_tool_definition(self, converter_tool):
        """get_definition returns an AIToolDefinition instance."""
        assert isinstance(converter_tool.get_definition(), AIToolDefinition)

    def test_tool_name_is_document_converter(self, converter_tool):
        """Tool definition name is 'document_converter'."""
        assert converter_tool.get_definition().name == "document_converter"

    def test_description_mentions_supported_formats(self, converter_tool):
        """Tool description lists the supported formats."""
        description = converter_tool.get_definition().description
        assert "docx" in description
        assert "md" in description

    def test_description_mentions_mindspace_restriction(self, converter_tool):
        """Tool description states that paths must be inside the mindspace."""
        assert "mindspace" in converter_tool.get_definition().description.lower()

    def test_operation_parameter_is_first_and_required(self, converter_tool):
        """The 'operation' parameter is first and required."""
        params = converter_tool.get_definition().parameters
        op_param = params[0]
        assert op_param.name == "operation"
        assert op_param.required is True

    def test_operation_enum_contains_convert(self, converter_tool):
        """The 'operation' enum contains exactly 'convert'."""
        params = converter_tool.get_definition().parameters
        op_param = params[0]
        assert op_param.enum == ["convert"]

    def test_input_path_parameter_is_required(self, converter_tool):
        """The 'input_path' parameter is required."""
        params = converter_tool.get_definition().parameters
        param = next(p for p in params if p.name == "input_path")
        assert param.required is True
        assert param.type == "string"

    def test_output_path_parameter_is_optional(self, converter_tool):
        """The 'output_path' parameter is optional."""
        params = converter_tool.get_definition().parameters
        param = next(p for p in params if p.name == "output_path")
        assert param.required is False
        assert param.type == "string"

    def test_from_format_parameter_is_optional_with_enum(self, converter_tool):
        """The 'from_format' parameter is optional and has a format enum."""
        params = converter_tool.get_definition().parameters
        param = next(p for p in params if p.name == "from_format")
        assert param.required is False
        assert param.type == "string"
        assert "docx" in param.enum
        assert "md" in param.enum

    def test_to_format_parameter_is_optional_with_enum(self, converter_tool):
        """The 'to_format' parameter is optional and has a format enum."""
        params = converter_tool.get_definition().parameters
        param = next(p for p in params if p.name == "to_format")
        assert param.required is False
        assert param.type == "string"
        assert "docx" in param.enum
        assert "md" in param.enum

    def test_from_format_and_to_format_enums_are_identical(self, converter_tool):
        """Both format parameters advertise exactly the same set of formats."""
        params = converter_tool.get_definition().parameters
        from_param = next(p for p in params if p.name == "from_format")
        to_param = next(p for p in params if p.name == "to_format")
        assert sorted(from_param.enum) == sorted(to_param.enum)

    def test_get_brief_description_is_non_empty(self, converter_tool):
        """get_brief_description returns a non-empty string."""
        brief = converter_tool.get_brief_description()
        assert isinstance(brief, str)
        assert len(brief) > 0

    def test_get_operation_definitions_contains_convert(self, converter_tool):
        """get_operation_definitions exposes exactly the 'convert' operation."""
        ops = converter_tool.get_operation_definitions()
        assert "convert" in ops
        assert len(ops) == 1

    def test_convert_operation_requires_input_path(self, converter_tool):
        """The convert operation requires 'input_path'."""
        op = converter_tool.get_operation_definitions()["convert"]
        assert "input_path" in op.required_parameters

    def test_convert_operation_allows_all_parameters(self, converter_tool):
        """The convert operation allows all four conversion parameters."""
        op = converter_tool.get_operation_definitions()["convert"]
        assert {"input_path", "output_path", "from_format", "to_format"} == op.allowed_parameters
