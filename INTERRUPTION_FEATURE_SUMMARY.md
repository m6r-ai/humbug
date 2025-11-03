# Conversation Interruption Feature Implementation

## Overview
This document summarizes the implementation of the conversation interruption feature, which allows users to interrupt an ongoing AI conversation with a new message during tool call processing.

## Design Summary

### UI Layout Changes
**During streaming (tool calls in progress):**
```
[Role Label - "Processing..."]     [Stop] [Interrupt]
```
- **Stop button** (left): Always enabled, cancels everything immediately
- **Interrupt button** (right): Only enabled when input has text, queues interruption message

**When not streaming:**
```
[Role Label - "Type your message..."]     [Submit]
```
- Single Submit button in the same position as Interrupt was

### Behavior
1. **Interruption queuing**: When user clicks Interrupt, message is submitted to AIConversation
2. **Automatic queueing**: AIConversation detects it's processing tools and queues the message internally
3. **Message injection**: After tool execution completes, queued message is **appended to tool result content**
4. **LLM receives context**: Tool results and user feedback are sent together in a single USER message
5. **Tool approval**: Pending tool approval is auto-rejected when user queues an interruption
6. **Cancellation**: User can still use Stop button to cancel everything immediately

## Architecture

### State Management in AIConversation
The `AIConversation` class tracks its processing state:

```python
class ConversationState(Enum):
    IDLE = auto()
    STREAMING_AI_RESPONSE = auto()
    EXECUTING_TOOLS = auto()
    WAITING_FOR_APPROVAL = auto()
```

When `submit_message()` is called:
- If state is IDLE: Process immediately (normal flow)
- If state is EXECUTING_TOOLS: Queue message to be appended to tool results
- If state is STREAMING_AI_RESPONSE: Queue message for later processing

### Message Flow

**Normal Message:**
```
User types → Submit → AIConversation.submit_message() → AI processes → Response
```

**Interruption During Tool Execution:**
```
User types → Interrupt → AIConversation.submit_message() 
  → Detects EXECUTING_TOOLS state
  → Queues message
  → Tool execution completes
  → Message appended to tool result content
  → AI receives: tool_results + [User feedback: message]
  → AI responds to both results and feedback
```

### Why Append to Tool Results?

The LLM conversation protocol expects:
1. AI makes tool calls
2. System returns tool results (as USER message with `tool_results` field)
3. AI processes those results

**Problem with separate USER message:**
```
[AI] tool_calls: [read_file('main.py')]
[USER] tool_results: [{"content": "..."}]
[USER] "Fix the bug"  ← Breaks protocol!
```

**Correct approach - append to tool result content:**
```
[AI] tool_calls: [read_file('main.py')]
[USER] 
  content: "[User feedback: Fix the bug]"
  tool_results: [{"content": "..."}]
[AI] "I see the file and your feedback..."
```

This maintains the tool call/result protocol while delivering the user's message.

## Files Modified

### 1. `src/ai/ai_conversation.py` ⭐ **Core Changes**
**Purpose**: Add state machine and message queueing for interruptions

**Key Changes**:
- Added `ConversationState` enum to track processing state
- Added `_state` instance variable to track current state
- Added `_pending_user_messages` list to queue interruption messages
- Modified `submit_message()` to check state and queue if necessary:
  ```python
  if self._state in (ConversationState.EXECUTING_TOOLS, 
                     ConversationState.STREAMING_AI_RESPONSE):
      self._pending_user_messages.append(message)
      return
  ```
- Modified `_execute_tool_calls()` to append interrupts to tool results:
  ```python
  # Check for pending interruptions and append to tool results
  interrupt_content = ""
  if self._pending_user_messages:
      interrupt_texts = [msg.content for msg in self._pending_user_messages if msg.content]
      if interrupt_texts:
          interrupt_content = "\n\n" + "\n\n".join([
              f"[User feedback: {text}]" for text in interrupt_texts
          ])
      self._pending_user_messages.clear()
  
  # Create USER message with tool results and interrupt content
  tool_response_message = AIMessage.create(
      AIMessageSource.USER,
      content=interrupt_content,  # ← Includes interrupt!
      tool_results=tool_results
  )
  ```
- Deleted `_process_pending_messages()` method (no longer needed)
- Added state tracking to `_start_ai()` and `_execute_tool_calls()`

### 2. `src/humbug/tabs/conversation/conversation_input.py`
**Purpose**: Add Stop and Interrupt buttons to input widget

**Key Changes**:
- Added `interrupt_requested` signal
- Added `_stop_button` widget (shown during streaming)
- Modified `_submit_button` to act as Submit or Interrupt depending on streaming state
- Updated `_update_button_states()` to show/hide buttons based on streaming state
- Added `_on_primary_button_clicked()` to route to submit or interrupt based on state
- Updated tooltips and button styling for both buttons

### 3. `src/humbug/tabs/conversation/conversation_widget.py`
**Purpose**: Handle interruption UI and coordinate with AIConversation

**Key Changes**:
- Connected `interrupt_requested` signal from input to `_on_interrupt_requested`
- Implemented `_on_interrupt_requested()` method:
  - Clears input
  - Auto-rejects pending tool approvals
  - Creates interruption message
  - Submits to AIConversation (which handles queueing internally)
- No longer needs to track pending interruption state (handled by AIConversation)
- No longer needs to manually inject messages (handled by AIConversation)
- Removed `_pending_interruption_message` and `_pending_interruption_widget` instance variables
- Removed `_inject_pending_interruption()` and `_clear_pending_interruption()` methods

### 4. `src/humbug/tabs/conversation/conversation_message.py`
**Purpose**: Support for pending state visual feedback (optional/future use)

**Key Changes**:
- Added `_is_pending` instance variable
- Added `set_pending_state(pending: bool)` method (for future use if needed)
- Modified `_get_border_color()` to support pending state

**Note**: The pending state feature is implemented but not currently used. Could be used in future for immediate visual feedback.

### 5. Language Files
**Purpose**: Add localized strings for new UI elements

**Files Modified**:
- `src/humbug/language/language_strings.py` - Added `tooltip_interrupt_message` field
- `src/humbug/language/en/en_strings.py` - English: "Interrupt with message"
- `src/humbug/language/fr/fr_strings.py` - French: "Interrompre avec un message"
- `src/humbug/language/ar/ar_strings.py` - Arabic: "مقاطعة بالرسالة"

## Visual Feedback

### Button States
- **Stop button**: Always visible during streaming, uses "stop" icon
- **Interrupt button**: 
  - Visible during streaming
  - Enabled only when input has text
  - Uses "submit" icon (same as regular submit)
  - Position matches regular Submit button for consistency

### Message Appearance
- Interruption messages appear in conversation through normal `MESSAGE_ADDED` callback
- No special visual treatment needed (message is processed normally)
- Message content appears as part of tool result in conversation

## User Flow

### Normal Interruption Flow
1. User types message while AI is processing tool calls
2. Interrupt button becomes enabled
3. User clicks Interrupt button
4. Input is cleared
5. If tool approval was pending, it's auto-rejected with message "User interrupted with new message"
6. Message is submitted to AIConversation
7. AIConversation detects EXECUTING_TOOLS state and queues message
8. Tool execution completes
9. AIConversation appends queued message to tool result content
10. Tool result message (with user feedback) is sent to LLM
11. AI processes both tool results and user feedback together
12. AI responds to the combined context

### Cancellation Flow
1. User types message while AI is processing
2. User clicks Stop button instead of Interrupt
3. All processing stops immediately via `cancel_current_tasks()`
4. Any queued messages remain in AIConversation queue (but won't be processed since conversation was cancelled)
5. Original message is lost (user can retype if needed)

## Implementation Notes

### Thread Safety
- All AI conversation operations use asyncio event loop
- UI updates happen on main thread
- Message queueing is handled within AIConversation's async context

### State Management
- **AIConversation**: Owns the conversation state and message queue
- **ConversationWidget**: Coordinates UI and delegates to AIConversation
- **ConversationInput**: Manages button visibility and user interaction

### Edge Cases Handled
1. **Multiple interruptions**: Messages are queued and all appended to tool result content
2. **Tool approval during interruption**: Auto-rejected with context message
3. **Cancellation with queued messages**: Messages remain queued but won't process (conversation is cancelled)
4. **Empty interrupt messages**: Filtered out when appending to tool results
5. **Interruption during AI streaming** (not tool execution): Message is queued and processed as separate turn after streaming completes

### Key Design Decision: Append to Tool Results

The interruption messages are **appended to tool result content** rather than processed as separate USER messages. This is critical because:

1. **Protocol compliance**: Maintains LLM's expected tool call → tool result → AI response flow
2. **Context preservation**: User feedback is delivered alongside the results it references
3. **Cleaner semantics**: Interrupt is feedback about tool execution, not a new conversation turn
4. **Better AI responses**: AI sees both results and feedback together, can respond coherently

### Message Format

When interrupts are appended to tool results, they're formatted as:
```
[User feedback: {interrupt message}]
```

Multiple interrupts are separated by double newlines:
```
[User feedback: First message]

[User feedback: Second message]
```

## Testing Recommendations

1. **Basic interruption**: Type message during tool calls, click Interrupt, verify it's appended to tool results
2. **Tool approval conflict**: Queue interruption while tool approval is showing, verify auto-rejection
3. **Cancellation**: Queue interruption then click Stop, verify conversation stops cleanly
4. **Button states**: Verify Stop always enabled, Interrupt only enabled with text
5. **Multiple tool calls**: Verify interruption appends after all tools complete
6. **Multiple interruptions**: Queue multiple interruptions, verify all are appended
7. **Streaming interruption**: Interrupt during AI response (not tool calls), verify message queues as separate turn
8. **LLM receives correctly**: Verify LLM sees tool results with user feedback in content field
9. **Localization**: Test in all supported languages (English, French, Arabic)

## Future Enhancements

Potential improvements not included in this implementation:
1. Visual indicator showing queued interruption count
2. Allow user to cancel queued interruption (Escape key?)
3. Show pending interruption with dimmed opacity (using the pending state feature)
4. Add keyboard shortcut for interrupt (Ctrl+Shift+J?)
5. Add setting to auto-interrupt on any input during streaming
6. Show preview of how interrupt will be formatted in tool result
7. Allow editing queued interruption before it's sent
