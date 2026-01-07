# AI Tool Module

This module provides a comprehensive framework for creating, managing, and executing AI tools that can be called by AI models. It supports tool registration, authorization, operation validation, and provides a clean abstraction for building custom AI tools.

## Core Components

### Tool Infrastructure
- **`ai_tool.py`** - Abstract base class for all AI tools, providing validation logic, operation routing, and helper methods for tool execution
- **`ai_tool_manager.py`** - Singleton manager that handles tool registration, enablement, and execution coordination
- **`ai_tool_exceptions.py`** - Custom exception classes for tool-specific error handling

### Tool Definition System
- **`ai_tool_definition.py`** - Defines the structure and metadata for AI tools, including parameters and descriptions
- **`ai_tool_parameter.py`** - Represents individual tool parameters with type information, validation rules, and documentation
- **`ai_tool_operation_definition.py`** - Defines sub-operations within tools, specifying allowed and required parameters

### Tool Execution System
- **`ai_tool_call.py`** - Represents a tool call request from an AI model with arguments and metadata
- **`ai_tool_result.py`** - Encapsulates the result of tool execution, including success/error states
- **`ai_tool_config.py`** - Configuration settings for individual tools, including display names and default states
- **`ai_tool_registered.py`** - Internal representation of registered tools with their metadata

### Supporting Components
- **`__init__.py`** - Public API exports for backward compatibility

## Key Features

- **Operation-Based Architecture**: Tools use a standard `operation` parameter to dispatch to different sub-operations with specific parameter requirements
- **Tool Registration**: Dynamic registration and management of AI tools with metadata
- **Authorization System**: Built-in authorization callbacks for secure tool execution
- **Operation Validation**: Comprehensive validation of tool arguments and operations with automatic parameter checking
- **Error Handling**: Structured exception handling with contextual information
- **Singleton Management**: Centralized tool management through singleton pattern
- **Flexible Configuration**: Per-tool configuration with enable/disable functionality
- **Type Safety**: Full type hint support for better development experience
- **Helper Methods**: Built-in utilities for parameter extraction and definition building

## Tool Architecture

### Operation-Based Pattern

Modern AI tools follow a standard **operation-based pattern** where:

1. **Single Tool, Multiple Operations**: Each tool defines multiple related operations (e.g., `clock` tool has `get_time`, `sleep`, `alarm` operations)
2. **Standard `operation` Parameter**: All tools accept an `operation` parameter that specifies which sub-operation to execute
3. **Operation Definitions**: Each operation defines its own:
   - `allowed_parameters` - Set of parameters valid for this operation
   - `required_parameters` - Set of parameters that must be provided
   - `handler` - Async function that implements the operation
   - `extract_context` - Optional function to extract context information
   - `description` - Human-readable description of what the operation does

4. **Automatic Validation**: The base `AITool` class automatically:
   - Routes to the correct operation handler
   - Validates that all required parameters are present
   - Rejects invalid parameters for the operation
   - Provides clear error messages for validation failures

### Tool Lifecycle

1. **Definition**: Tools implement the `AITool` abstract base class
2. **Registration**: Tools are registered with the `AIToolManager` singleton
3. **Configuration**: Tools can be enabled/disabled and configured per-instance
4. **Execution**: AI models call tools through `AIToolCall` objects
5. **Authorization**: Optional authorization checks before execution
6. **Operation Routing**: Base class routes to appropriate operation handler
7. **Validation**: Automatic parameter validation before execution
8. **Results**: Structured results returned via `AIToolResult` objects

### Exception Hierarchy

- **`AIToolExecutionError`** - General tool execution failures
- **`AIToolAuthorizationDenied`** - Authorization-related failures
- **`AIToolTimeoutError`** - Timeout-related failures

## Usage

The AI tool framework is designed to be used by:

1. **Tool Developers**: Create custom tools by inheriting from `AITool`
2. **AI Systems**: Register and execute tools through `AIToolManager`
3. **Applications**: Configure and manage tool availability and permissions

### Example Tool Implementation

Here's a complete example showing the modern operation-based pattern:

```python
from typing import Any, Dict
from ai_tool import (
    AITool, AIToolDefinition, AIToolParameter, AIToolOperationDefinition,
    AIToolCall, AIToolResult, AIToolAuthorizationCallback, AIToolExecutionError
)


class CalculatorAITool(AITool):
    """Calculator tool with multiple math operations."""

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        # Use helper method to build definition from operations
        return self._build_definition_from_operations(
            name="calculator",
            description_prefix=(
                "The calculator tool performs mathematical operations. "
                "It supports basic arithmetic and advanced functions."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="x",
                    type="number",
                    description="First operand",
                    required=False
                ),
                AIToolParameter(
                    name="y",
                    type="number",
                    description="Second operand (for binary operations)",
                    required=False
                )
            ]
        )

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """Define the operations this tool supports."""
        return {
            "add": AIToolOperationDefinition(
                name="add",
                handler=self._add,
                extract_context=None,
                allowed_parameters={"x", "y"},
                required_parameters={"x", "y"},
                description="Add two numbers together"
            ),
            "square": AIToolOperationDefinition(
                name="square",
                handler=self._square,
                extract_context=None,
                allowed_parameters={"x"},
                required_parameters={"x"},
                description="Square a number"
            )
        }

    async def _add(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Add operation handler."""
        arguments = tool_call.arguments

        # Use helper methods for parameter extraction
        x = arguments.get("x")
        y = arguments.get("y")

        if not isinstance(x, (int, float)) or not isinstance(y, (int, float)):
            raise AIToolExecutionError("Both x and y must be numbers")

        result = x + y

        return AIToolResult(
            id=tool_call.id,
            name="calculator",
            content=str(result)
        )

    async def _square(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Square operation handler."""
        arguments = tool_call.arguments
        x = arguments.get("x")

        if not isinstance(x, (int, float)):
            raise AIToolExecutionError("x must be a number")

        result = x * x

        return AIToolResult(
            id=tool_call.id,
            name="calculator",
            content=str(result)
        )
```

### Key Implementation Details

#### 1. Operation Definitions

Each operation is defined with:
- **`handler`**: Async method that implements the operation
- **`allowed_parameters`**: Set of parameter names valid for this operation
- **`required_parameters`**: Subset of allowed parameters that must be provided
- **`description`**: Brief description for documentation
- **`extract_context`**: Optional function to extract context (for logging/debugging)

#### 2. Helper Methods

The base `AITool` class provides useful helper methods:

**Definition Building:**
```python
# Automatically builds tool definition from operations
self._build_definition_from_operations(
    name="tool_name",
    description_prefix="Tool description",
    additional_parameters=[...]  # Parameters beyond 'operation'
)
```

**Parameter Extraction:**
```python
# Extract required string parameter
path = self._get_required_str_value("path", arguments)

# Extract optional parameters with defaults
encoding = self._get_optional_str_value("encoding", arguments, default="utf-8")
line_number = self._get_optional_int_value("line", arguments, default=1)
case_sensitive = self._get_optional_bool_value("case_sensitive", arguments, default=False)
items = self._get_optional_list_value("items", arguments, default=[])
```

#### 3. Context Extraction

Operations can provide context for better error messages and logging:

```python
def _extract_my_operation_context(self, arguments: Dict[str, Any]) -> str | None:
    """Extract context for logging/debugging."""
    file_path = arguments.get("path", "")
    return f"Operating on file: {file_path}"

# In operation definition:
AIToolOperationDefinition(
    name="my_operation",
    handler=self._my_operation,
    extract_context=self._extract_my_operation_context,
    ...
)
```

#### 4. Authorization

Operations can request user authorization when needed:

```python
async def _dangerous_operation(
    self,
    tool_call: AIToolCall,
    requester_ref: Any,
    request_authorization: AIToolAuthorizationCallback
) -> AIToolResult:
    """Operation that requires authorization."""

    # Request authorization before proceeding
    authorized = await request_authorization(
        tool_name="my_tool",
        arguments=tool_call.arguments,
        operation="dangerous_operation",
        context="This will delete files",
        auto_approve=False  # Force user approval
    )

    if not authorized:
        raise AIToolAuthorizationDenied("User denied authorization")

    # Proceed with operation...
    return AIToolResult(...)
```

### Legacy Pattern (Non-Operation-Based Tools)

Tools without operations must override the `execute()` method directly:

```python
class SimpleTool(AITool):
    """Simple tool without operations."""

    def get_definition(self) -> AIToolDefinition:
        return AIToolDefinition(
            name="simple_tool",
            description="A simple tool",
            parameters=[
                AIToolParameter(
                    name="input",
                    type="string",
                    description="Input text",
                    required=True
                )
            ]
        )

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Direct execution without operations."""
        arguments = tool_call.arguments
        input_text = arguments.get("input", "")

        # Process input...
        result = input_text.upper()

        return AIToolResult(
            id=tool_call.id,
            name="simple_tool",
            content=result
        )
```

## Tool Directory

The subdirectories (`aifpl/`, `clock/`, `filesystem/`, etc.) contain concrete implementations of AI tools that follow the operation-based pattern. These serve as reference implementations demonstrating best practices for:

- Operation definition and routing
- Parameter validation
- Authorization handling
- Error management
- Context extraction
- Async execution patterns

## Best Practices

1. **Use the operation-based pattern** for tools with multiple related functions
2. **Use `_build_definition_from_operations()`** to reduce boilerplate
3. **Use helper methods** for parameter extraction to ensure consistent validation
4. **Define clear operation boundaries** - each operation should have a single, well-defined purpose
5. **Provide good descriptions** for operations to help AI models understand when to use them
6. **Request authorization** for operations that modify state or access sensitive resources
7. **Use appropriate exceptions** - `AIToolExecutionError` for errors, `AIToolAuthorizationDenied` for auth failures
8. **Add context extraction** for complex operations to improve debugging
9. **Follow async patterns** - all handlers must be async, use `asyncio` for I/O operations
10. **Validate parameter types** explicitly in handlers, don't assume the AI will provide correct types
