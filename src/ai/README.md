# AI Module

This module provides a comprehensive framework for interacting with various AI backends and managing AI conversations. It includes support for multiple AI providers, conversation management, tool integration, and streaming responses.

## Core Components

### Backend Infrastructure
- **`ai_backend.py`** - Abstract base class for all AI backends, providing common functionality like request handling, streaming responses, rate limiting, and error handling
- **`ai_backend_settings.py`** - Configuration settings for AI backends
- **`ai_manager.py`** - Central manager for AI operations and backend coordination

### Conversation Management
- **`ai_conversation.py`** - Main conversation class that manages the flow of messages, settings, and AI interactions
- **`ai_conversation_history.py`** - Handles conversation history storage and retrieval
- **`ai_conversation_settings.py`** - Configuration and settings for AI conversations, including model selection and parameters

### Message System
- **`ai_message.py`** - Represents individual messages in a conversation with support for different message types and metadata
- **`ai_message_source.py`** - Defines the source types for messages (user, assistant, system, etc.)
- **`ai_response.py`** - Represents responses from AI backends
- **`ai_stream_response.py`** - Handles streaming responses from AI backends with incremental updates

### Utility Components
- **`ai_model.py`** - Defines AI model configurations and capabilities
- **`ai_rate_limiter.py`** - Implements rate limiting for API calls to prevent quota exhaustion
- **`ai_usage.py`** - Tracks and reports AI usage statistics

## Provider Support

The module includes support for multiple AI providers through dedicated subdirectories:

- **`anthropic/`** - Anthropic Claude integration
- **`deepseek/`** - DeepSeek AI integration  
- **`google/`** - Google AI (Gemini) integration
- **`m6r/`** - M6R AI integration
- **`mistral/`** - Mistral AI integration
- **`ollama/`** - Ollama local AI integration
- **`openai/`** - OpenAI GPT integration
- **`xai/`** - X.AI integration

## Key Features

- **Streaming Support**: Real-time streaming of AI responses with incremental updates
- **Multi-Provider**: Unified interface for different AI providers
- **Tool Calling**: Support for AI models to call external functions and tools
- **Rate Limiting**: Built-in rate limiting to handle API quotas
- **Error Handling**: Comprehensive error handling with retry mechanisms
- **Conversation History**: Persistent conversation history management
- **Flexible Configuration**: Extensive configuration options for different use cases

## Usage

The AI module is designed to be used through the main `AIConversation` class, which coordinates all the components to provide a seamless AI interaction experience.
