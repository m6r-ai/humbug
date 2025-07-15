# AI Tool Module

This module provides a comprehensive framework for creating, managing, and executing AI tools that can be called by AI models. It supports tool registration, authorization, operation validation, and provides a clean abstraction for building custom AI tools.

## Core Components

### Tool Infrastructure
- **`ai_tool.py`** - Abstract base class for all AI tools, providing validation logic and defining the interface for tool execution
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
- **`types.py`** - Type definitions and aliases used throughout the framework
- **`__init__.py`** - Public API exports for backward compatibility

## Key Features

- **Tool Registration**: Dynamic registration and management of AI tools with metadata
- **Authorization System**: Built-in authorization callbacks for secure tool execution
- **Operation Validation**: Comprehensive validation of tool arguments and operations
- **Error Handling**: Structured exception handling with contextual information
- **Singleton Management**: Centralized tool management through singleton pattern
- **Flexible Configuration**: Per-tool configuration with enable/disable functionality
- **Type Safety**: Full type hint support for better development experience

## Tool Architecture

### Tool Lifecycle
1. **Definition**: Tools implement the `AITool` abstract base class
2. **Registration**: Tools are registered with the `AIToolManager` singleton
3. **Configuration**: Tools can be enabled/disabled and configured per-instance
4. **Execution**: AI models call tools through `AIToolCall` objects
5. **Authorization**: Optional authorization checks before execution
6. **Validation**: Argument and operation validation before execution
7. **Results**: Structured results returned via `AIToolResult` objects

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

```python
from ai_tool import AITool, AIToolDefinition, AIToolParameter

class MyCustomTool(AITool):
    def get_definition(self) -> AIToolDefinition:
        return AIToolDefinition(
            name="my_tool",
            description="A custom tool example",
            parameters=[
                AIToolParameter(
                    name="input",
                    type="string",
                    description="Input text to process",
                    required=True
                )
            ]
        )
    
    async def execute(self, arguments, request_authorization):
        # Tool implementation here
        return "Tool result"
```

## Tool Directory

The `tools/` subdirectory contains concrete implementations of AI tools that can be registered and used with AI models. Each tool follows the framework's patterns and provides specific functionality for AI interactions.