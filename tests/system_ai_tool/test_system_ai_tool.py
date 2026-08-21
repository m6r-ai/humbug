"""
Tests for the system AI tool: tab lifecycle and workspace management.
"""
import asyncio
import json
import os

import pytest

from ai_tool import AIToolExecutionError, AIToolAuthorizationDenied
from system_ai_tool.system_ai_tool import SystemAITool


class TestSystemAIToolDefinition:
    """Test the tool definition and operation definitions."""

    def test_get_definition(self, system_tool: SystemAITool) -> None:
        """Test that the tool definition is correctly structured."""
        definition = system_tool.get_definition()
        assert definition.name == "system"

    def test_brief_description(self, system_tool: SystemAITool) -> None:
        """Test the brief description."""
        assert "tabs" in system_tool.get_brief_description()

    def test_operation_definitions(self, system_tool: SystemAITool) -> None:
        """Test that all expected operations are defined."""
        ops = system_tool.get_operation_definitions()
        expected = {
            "open_editor_tab", "new_terminal_tab", "open_conversation_tab",
            "new_conversation_tab", "open_preview_tab", "open_diff_tab",
            "get_tab_info", "close_tab", "list_tabs", "move_tab",
            "get_system_info",
        }
        assert set(ops.keys()) == expected


class TestOpenEditorTab:
    """Test the open_editor_tab operation."""

    def test_open_new_file(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test opening a new editor tab creates a context."""
        mindspace, mindspace_path = temp_mindspace
        tool_call = make_tool_call("open_editor_tab", file_path="test.py")

        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        assert "Opened editor tab" in result.content
        assert "tab ID:" in result.content

        contexts = mindspace.contexts().list_all()
        assert len(contexts) == 1
        assert contexts[0].context_type == "editor"
        assert contexts[0].title == "test.py"

    def test_open_existing_file_returns_existing_tab(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test that opening an already-open file returns the existing tab."""
        mindspace, _ = temp_mindspace
        tool_call = make_tool_call("open_editor_tab", file_path="test.py")

        result1 = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))
        result2 = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        assert result1.content == result2.content
        assert len(mindspace.contexts()) == 1

    def test_open_editor_creates_parent_directory(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test that opening a file in a non-existent directory creates it."""
        _, mindspace_path = temp_mindspace
        tool_call = make_tool_call("open_editor_tab", file_path="subdir/test.py")

        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        assert "Opened editor tab" in result.content
        assert os.path.isdir(os.path.join(mindspace_path, "subdir"))

    def test_open_editor_path_outside_mindspace(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call
    ) -> None:
        """Test that a path outside the mindspace is rejected."""
        tool_call = make_tool_call("open_editor_tab", file_path="../../outside.py")

        with pytest.raises(AIToolExecutionError, match="outside mindspace"):
            asyncio.run(system_tool.execute(tool_call, None, mock_authorization))


class TestNewTerminalTab:
    """Test the new_terminal_tab operation."""

    def test_create_terminal(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test creating a new terminal tab."""
        mindspace, _ = temp_mindspace
        tool_call = make_tool_call("new_terminal_tab")

        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        assert "Created new terminal" in result.content
        assert "tab ID:" in result.content
        contexts = mindspace.contexts().list_all()
        assert len(contexts) == 1
        assert contexts[0].context_type == "terminal"


class TestOpenPreviewTab:
    """Test the open_preview_tab operation."""

    def test_open_preview_root(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test opening a preview of the mindspace root."""
        mindspace, _ = temp_mindspace
        tool_call = make_tool_call("open_preview_tab")

        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        assert "Opened preview tab" in result.content
        contexts = mindspace.contexts().list_all()
        assert len(contexts) == 1
        assert contexts[0].context_type == "preview"

    def test_open_preview_file(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test opening a preview of a specific file."""
        mindspace, mindspace_path = temp_mindspace
        with open(os.path.join(mindspace_path, "test.md"), "w") as f:
            f.write("# Test")
        tool_call = make_tool_call("open_preview_tab", file_path="test.md")

        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        assert "Opened preview tab" in result.content
        assert "test.md" in result.content


class TestOpenDiffTab:
    """Test the open_diff_tab operation."""

    def test_open_diff_for_existing_file(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test opening a diff tab for an existing file."""
        mindspace, mindspace_path = temp_mindspace
        with open(os.path.join(mindspace_path, "test.py"), "w") as f:
            f.write("print('hello')")
        tool_call = make_tool_call("open_diff_tab", file_path="test.py")

        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        assert "Opened diff tab" in result.content
        contexts = mindspace.contexts().list_all()
        assert len(contexts) == 1
        assert contexts[0].context_type == "diff"

    def test_open_diff_nonexistent_file(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call
    ) -> None:
        """Test that opening a diff for a non-existent file fails."""
        tool_call = make_tool_call("open_diff_tab", file_path="nonexistent.py")

        with pytest.raises(AIToolExecutionError, match="File not found"):
            asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

    def test_open_diff_directory(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test that opening a diff for a directory fails."""
        _, mindspace_path = temp_mindspace
        os.makedirs(os.path.join(mindspace_path, "subdir"))
        tool_call = make_tool_call("open_diff_tab", file_path="subdir")

        with pytest.raises(AIToolExecutionError, match="Cannot diff a directory"):
            asyncio.run(system_tool.execute(tool_call, None, mock_authorization))


class TestGetTabInfo:
    """Test the get_tab_info operation."""

    def test_get_info_for_specific_tab(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test getting info for a specific tab by ID."""
        mindspace, _ = temp_mindspace
        tool_call = make_tool_call("open_editor_tab", file_path="test.py")
        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))
        tab_id = result.content.split("tab ID: ")[1]

        info_call = make_tool_call("get_tab_info", tab_id=tab_id)
        info_result = asyncio.run(system_tool.execute(info_call, None, mock_authorization))

        info = json.loads(info_result.content)
        assert info["tab_id"] == tab_id
        assert info["type"] == "editor"
        assert info["title"] == "test.py"

    def test_get_info_no_current_tab(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call
    ) -> None:
        """Test that get_tab_info with no tab_id and no current tab fails."""
        tool_call = make_tool_call("get_tab_info")

        with pytest.raises(AIToolExecutionError, match="No current tab"):
            asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

    def test_get_info_nonexistent_tab(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call
    ) -> None:
        """Test that getting info for a non-existent tab fails."""
        tool_call = make_tool_call("get_tab_info", tab_id="nonexistent-id")

        with pytest.raises(AIToolExecutionError, match="No tab found"):
            asyncio.run(system_tool.execute(tool_call, None, mock_authorization))


class TestCloseTab:
    """Test the close_tab operation."""

    def test_close_existing_tab(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test closing an existing tab."""
        mindspace, _ = temp_mindspace
        tool_call = make_tool_call("open_editor_tab", file_path="test.py")
        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))
        tab_id = result.content.split("tab ID: ")[1]

        close_call = make_tool_call("close_tab", tab_id=tab_id)
        close_result = asyncio.run(system_tool.execute(close_call, None, mock_authorization))

        assert close_result.content == "Closed tab"
        assert len(mindspace.contexts()) == 0

    def test_close_nonexistent_tab(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call
    ) -> None:
        """Test that closing a non-existent tab fails."""
        tool_call = make_tool_call("close_tab", tab_id="nonexistent-id")

        with pytest.raises(AIToolExecutionError, match="No tab found"):
            asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

    def test_close_modified_tab_denied(
        self, system_tool: SystemAITool, mock_authorization_denied, make_tool_call, temp_mindspace
    ) -> None:
        """Test that closing a modified tab without authorization is denied."""
        mindspace, _ = temp_mindspace
        tool_call = make_tool_call("open_editor_tab", file_path="test.py")
        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization_denied))
        tab_id = result.content.split("tab ID: ")[1]

        mindspace.contexts().update(tab_id, is_modified=True)

        close_call = make_tool_call("close_tab", tab_id=tab_id)
        with pytest.raises(AIToolAuthorizationDenied):
            asyncio.run(system_tool.execute(close_call, None, mock_authorization_denied))


class TestListTabs:
    """Test the list_tabs operation."""

    def test_list_no_tabs(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call
    ) -> None:
        """Test listing when no tabs are open."""
        tool_call = make_tool_call("list_tabs")

        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        assert result.content == "No tabs are currently open."

    def test_list_with_tabs(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test listing with multiple tabs open."""
        mindspace, _ = temp_mindspace
        asyncio.run(system_tool.execute(
            make_tool_call("open_editor_tab", file_path="test.py"), None, mock_authorization
        ))
        asyncio.run(system_tool.execute(
            make_tool_call("new_terminal_tab"), None, mock_authorization
        ))

        tool_call = make_tool_call("list_tabs")
        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        data = json.loads(result.content)
        assert data["total_tabs"] == 2
        assert data["total_columns"] >= 1
        assert len(data["tabs"]) == 2


class TestMoveTab:
    """Test the move_tab operation."""

    def test_move_to_valid_column(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test moving a tab to a valid column."""
        mindspace, _ = temp_mindspace
        open_call = make_tool_call("open_editor_tab", file_path="test.py")
        result = asyncio.run(system_tool.execute(open_call, None, mock_authorization))
        tab_id = result.content.split("tab ID: ")[1]

        move_call = make_tool_call("move_tab", tab_id=tab_id, target_column=2)
        move_result = asyncio.run(system_tool.execute(move_call, None, mock_authorization))

        assert "Moved tab" in move_result.content
        assert "column 2" in move_result.content
        info = mindspace.contexts().get(tab_id)
        assert info is not None
        assert info.column == 2

    def test_move_nonexistent_tab(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call
    ) -> None:
        """Test that moving a non-existent tab fails."""
        tool_call = make_tool_call("move_tab", tab_id="nonexistent", target_column=1)

        with pytest.raises(AIToolExecutionError, match="Context not found"):
            asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

    def test_move_to_invalid_column(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test that moving to an out-of-range column fails."""
        _, _ = temp_mindspace
        open_call = make_tool_call("open_editor_tab", file_path="test.py")
        result = asyncio.run(system_tool.execute(open_call, None, mock_authorization))
        tab_id = result.content.split("tab ID: ")[1]

        move_call = make_tool_call("move_tab", tab_id=tab_id, target_column=99)
        with pytest.raises(AIToolExecutionError, match="Column must be"):
            asyncio.run(system_tool.execute(move_call, None, mock_authorization))


class TestGetSystemInfo:
    """Test the get_system_info operation."""

    def test_system_info(
        self, system_tool: SystemAITool, mock_authorization, make_tool_call, temp_mindspace
    ) -> None:
        """Test that system info returns expected fields."""
        _, mindspace_path = temp_mindspace
        tool_call = make_tool_call("get_system_info")

        result = asyncio.run(system_tool.execute(tool_call, None, mock_authorization))

        data = json.loads(result.content)
        assert "system" in data
        assert "mindspace" in data
        assert "ai" in data
        assert "shell" in data
        assert data["mindspace"]["path"] == mindspace_path


class TestContextRegistryLayout:
    """Test the ContextRegistry layout features that SystemAITool relies on."""

    def test_move_updates_column(self, temp_mindspace: Any) -> None:
        """Test that move() updates the column on ContextInfo."""
        mindspace, _ = temp_mindspace
        context_id = mindspace.contexts().open(
            context_type="editor", path="/test/file.py", title="file.py"
        )

        mindspace.contexts().move(context_id, 3)

        info = mindspace.contexts().get(context_id)
        assert info is not None
        assert info.column == 3

    def test_num_columns_empty(self, temp_mindspace: Any) -> None:
        """Test num_columns returns 0 when no contexts are open."""
        mindspace, _ = temp_mindspace
        assert mindspace.contexts().num_columns() == 0

    def test_num_columns_with_contexts(self, temp_mindspace: Any) -> None:
        """Test num_columns returns the correct count."""
        mindspace, _ = temp_mindspace
        mindspace.contexts().open(context_type="editor", column=0)
        mindspace.contexts().open(context_type="terminal", column=2)
        mindspace.contexts().open(context_type="preview", column=4)

        assert mindspace.contexts().num_columns() == 5

    def test_make_permanent(self, temp_mindspace: Any) -> None:
        """Test that make_permanent flips is_ephemeral to False."""
        mindspace, _ = temp_mindspace
        context_id = mindspace.contexts().open(
            context_type="editor", is_ephemeral=True
        )

        info = mindspace.contexts().get(context_id)
        assert info is not None
        assert info.is_ephemeral is True

        mindspace.contexts().make_permanent(context_id)

        info = mindspace.contexts().get(context_id)
        assert info is not None
        assert info.is_ephemeral is False

    def test_make_permanent_already_permanent_is_noop(self, temp_mindspace: Any) -> None:
        """Test that make_permanent on a permanent context is a no-op."""
        mindspace, _ = temp_mindspace
        context_id = mindspace.contexts().open(
            context_type="editor", is_ephemeral=False
        )

        mindspace.contexts().make_permanent(context_id)

        info = mindspace.contexts().get(context_id)
        assert info is not None
        assert info.is_ephemeral is False

    def test_current_context_id(self, temp_mindspace: Any) -> None:
        """Test that focus stores the current context id."""
        mindspace, _ = temp_mindspace
        context_id = mindspace.contexts().open(context_type="editor")

        assert mindspace.contexts().current_context_id() is None

        mindspace.contexts().focus(context_id)

        assert mindspace.contexts().current_context_id() == context_id

    def test_close_clears_current_context_id(self, temp_mindspace: Any) -> None:
        """Test that closing the current context clears the current id."""
        mindspace, _ = temp_mindspace
        context_id = mindspace.contexts().open(context_type="editor")
        mindspace.contexts().focus(context_id)

        mindspace.contexts().close(context_id)

        assert mindspace.contexts().current_context_id() is None

    def test_moved_event_emitted(self, temp_mindspace: Any) -> None:
        """Test that MOVED event is emitted on move."""
        from context.context_registry import ContextEvent
        mindspace, _ = temp_mindspace
        context_id = mindspace.contexts().open(context_type="editor")

        events: list[tuple[str, int]] = []
        mindspace.contexts().register_callback(
            ContextEvent.MOVED,
            lambda cid, col: events.append((cid, col))
        )

        mindspace.contexts().move(context_id, 2)

        assert len(events) == 1
        assert events[0] == (context_id, 2)
