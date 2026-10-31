"""Tests for Mindspace AI settings broadcasting and session state persistence."""
import os

import pytest

from ai import AIConversationSettings
from context.context_registry import ContextRegistry
from conversation_context.conversation_context import ConversationContext
from mindspace.mindspace import Mindspace
from mindspace.mindspace_error import MindspaceError, MindspaceHumbugPathError
from mindspace.mindspace_settings import MindspaceSettings


def _new_mindspace() -> Mindspace:
    """Construct a Mindspace with nothing open."""
    return Mindspace(
        on_settings_changed=lambda: None,
        on_interactions_updated=lambda: None,
    )


def _make_mindspace(tmp_path, monkeypatch) -> Mindspace:
    """Construct a Mindspace with an open mindspace at a temp path."""
    mindspace = _new_mindspace()
    path = os.path.join(str(tmp_path), "ms")
    os.makedirs(os.path.join(path, ".humbug"))
    monkeypatch.setattr(mindspace, "_path", path)
    monkeypatch.setattr(mindspace, "_settings", MindspaceSettings(enabled_tools={}))
    return mindspace


class _FakeTranscript:
    """Minimal stand-in for AITranscriptConversation that records settings updates."""

    def __init__(self) -> None:
        self.applied: list[object] = []

    def update_conversation_settings(self, settings: object) -> None:
        self.applied.append(settings)


def _register_conversation(registry: ContextRegistry, transcript: _FakeTranscript) -> str:
    """Open a conversation context registered with the given transcript."""
    cid = registry.open(context_type="conversation")
    context = ConversationContext(
        context_id=cid,
        ai_transcript_conversation=transcript,  # type: ignore[arg-type]
    )
    registry.register_model(cid, context)
    return cid


class TestApplyAiSettingsToAll:
    """Tests for Mindspace.apply_ai_settings_to_all."""

    def test_broadcasts_to_all_conversation_contexts(self, tmp_path, monkeypatch) -> None:
        """Applying settings broadcasts to every open conversation context."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        registry = mindspace.contexts()
        transcripts = [_FakeTranscript(), _FakeTranscript()]
        for transcript in transcripts:
            _register_conversation(registry, transcript)

        settings = AIConversationSettings(model="m", provider="p", temperature=0.5)
        mindspace.apply_ai_settings_to_all(settings)

        for transcript in transcripts:
            assert transcript.applied == [settings]

    def test_skips_non_conversation_contexts(self, tmp_path, monkeypatch) -> None:
        """Applying settings leaves non-conversation contexts untouched."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        registry = mindspace.contexts()
        registry.open(context_type="editor")
        transcript = _FakeTranscript()
        _register_conversation(registry, transcript)

        settings = AIConversationSettings(model="m", provider="p", temperature=0.5)
        mindspace.apply_ai_settings_to_all(settings)

        assert transcript.applied == [settings]

    def test_persists_ai_settings_as_mindspace_default(self, tmp_path, monkeypatch) -> None:
        """Applying settings persists the AI fields as the mindspace default."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        settings = AIConversationSettings(
            model="m", provider="p", temperature=0.5, reasoning_effort="high"
        )

        mindspace.apply_ai_settings_to_all(settings)

        current = mindspace.settings()
        assert current is not None
        assert current.model == "m"
        assert current.provider == "p"
        assert current.temperature == 0.5
        assert current.reasoning_effort == "high"


class TestSessionState:
    """Tests for Mindspace session state persistence."""

    def test_save_and_load_round_trips_state(self, tmp_path, monkeypatch) -> None:
        """Session state survives a save/load round trip."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        state = {
            "contexts": [
                {
                    "layout": {
                        "context_id": "c1",
                        "context_type": "editor",
                        "path": "",
                        "title": "file.py",
                        "is_ephemeral": False,
                        "column": 0,
                        "position": 0,
                    },
                    "content": {"path": ""},
                    "frontend": {"cursor": {"line": 3, "column": 1}},
                }
            ],
            "focused_id": "c1",
            "num_columns": 1,
        }

        mindspace.save_mindspace_state(state)
        loaded = mindspace.load_mindspace_state()

        assert loaded["focused_id"] == "c1"
        assert loaded["num_columns"] == 1
        assert loaded["contexts"][0]["layout"]["context_id"] == "c1"
        assert loaded["contexts"][0]["frontend"] == {"cursor": {"line": 3, "column": 1}}

    def test_save_stores_paths_relative_to_mindspace(self, tmp_path, monkeypatch) -> None:
        """Absolute paths inside the mindspace are stored relative."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        mindspace_path = mindspace.mindspace_path()
        absolute = os.path.join(mindspace_path, "src", "file.py")
        state = {
            "contexts": [
                {
                    "layout": {
                        "context_id": "c1",
                        "context_type": "editor",
                        "path": absolute,
                        "title": "file.py",
                        "is_ephemeral": False,
                        "column": 0,
                        "position": 0,
                    },
                    "content": {},
                    "frontend": {},
                }
            ],
            "focused_id": None,
            "num_columns": 1,
        }

        mindspace.save_mindspace_state(state)
        loaded = mindspace.load_mindspace_state()

        assert loaded["contexts"][0]["layout"]["path"] == absolute

    def test_load_resolves_relative_paths_to_absolute(self, tmp_path, monkeypatch) -> None:
        """Relative paths in the saved file are resolved against the mindspace."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        state = {
            "contexts": [
                {
                    "layout": {
                        "context_id": "c1",
                        "context_type": "editor",
                        "path": os.path.join("src", "file.py"),
                        "title": "file.py",
                        "is_ephemeral": False,
                        "column": 0,
                        "position": 0,
                    },
                    "content": {},
                    "frontend": {},
                }
            ],
            "focused_id": None,
            "num_columns": 1,
        }

        mindspace.save_mindspace_state(state)
        loaded = mindspace.load_mindspace_state()

        expected = os.path.join(mindspace.mindspace_path(), "src", "file.py")
        assert loaded["contexts"][0]["layout"]["path"] == expected

    def test_load_returns_empty_when_no_state_saved(self, tmp_path, monkeypatch) -> None:
        """Loading a mindspace with no saved session returns an empty dictionary."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)

        assert mindspace.load_mindspace_state() == {}

    def test_save_keeps_paths_outside_mindspace_absolute(self, tmp_path, monkeypatch) -> None:
        """Paths outside the mindspace are stored unchanged."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        outside = os.path.join(str(tmp_path), "elsewhere", "file.py")
        state = {
            "contexts": [
                {
                    "layout": {
                        "context_id": "c1",
                        "context_type": "editor",
                        "path": outside,
                        "title": "file.py",
                        "is_ephemeral": False,
                        "column": 0,
                        "position": 0,
                    },
                    "content": {},
                    "frontend": {},
                }
            ],
            "focused_id": None,
            "num_columns": 1,
        }

        mindspace.save_mindspace_state(state)
        loaded = mindspace.load_mindspace_state()

        assert loaded["contexts"][0]["layout"]["path"] == outside


class TestHumbugPathExclusion:
    """Tests for the tool-facing .humbug/ exclusion in Mindspace."""

    def test_is_humbug_path_true_for_humbug_dir(self, tmp_path, monkeypatch) -> None:
        """Paths inside .humbug/ are recognised as Humbug-internal."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        inside = os.path.join(mindspace.mindspace_path(), ".humbug", "settings.json")

        assert mindspace.is_humbug_path(inside) is True

    def test_is_humbug_path_false_for_regular_file(self, tmp_path, monkeypatch) -> None:
        """Paths elsewhere in the mindspace are not Humbug-internal."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        regular = os.path.join(mindspace.mindspace_path(), "src", "file.py")

        assert mindspace.is_humbug_path(regular) is False

    def test_is_humbug_path_false_for_sibling_with_prefix(self, tmp_path, monkeypatch) -> None:
        """A sibling directory whose name starts with .humbug is not treated as .humbug/."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        sibling = os.path.join(mindspace.mindspace_path(), ".humbug_backup", "file.py")

        assert mindspace.is_humbug_path(sibling) is False

    def test_resolve_tool_path_allows_regular_file(self, tmp_path, monkeypatch) -> None:
        """A path inside the mindspace but outside .humbug/ resolves successfully."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        target = os.path.join(mindspace.mindspace_path(), "src", "file.py")

        abs_path, relative_path = mindspace.resolve_tool_path(target)

        assert abs_path == os.path.realpath(target)
        assert relative_path == os.path.join("src", "file.py")

    def test_resolve_tool_path_rejects_humbug_dir(self, tmp_path, monkeypatch) -> None:
        """A path inside .humbug/ is rejected."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        target = os.path.join(mindspace.mindspace_path(), ".humbug", "settings.json")

        with pytest.raises(MindspaceHumbugPathError):
            mindspace.resolve_tool_path(target)

    def test_resolve_tool_path_rejects_outside_mindspace(self, tmp_path, monkeypatch) -> None:
        """A path outside the mindspace is rejected."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        target = os.path.join(str(tmp_path), "elsewhere", "file.py")

        with pytest.raises(MindspaceError):
            mindspace.resolve_tool_path(target)

    def test_resolve_tool_path_rejects_symlink_into_humbug(self, tmp_path, monkeypatch) -> None:
        """A symlink inside the mindspace pointing into .humbug/ is rejected."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        mindspace_path = mindspace.mindspace_path()
        secret = os.path.join(mindspace_path, ".humbug", "settings.json")
        with open(secret, "w", encoding="utf-8") as f:
            f.write("{}")
        link = os.path.join(mindspace_path, "link.json")
        os.symlink(secret, link)

        with pytest.raises(MindspaceHumbugPathError):
            mindspace.resolve_tool_path(link)

    def test_get_mindspace_relative_path_still_accepts_humbug_dir(self, tmp_path, monkeypatch) -> None:
        """The internal relative-path helper still resolves .humbug/ paths for internal use."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        target = os.path.join(mindspace.mindspace_path(), ".humbug", "conversations", "c1.json")

        relative_path = mindspace.get_mindspace_relative_path(target)

        assert relative_path == os.path.join(".humbug", "conversations", "c1.json")



class TestNestedHumbugExclusion:
    """Tests that a mindspace inside or above another cannot reach the other's .humbug/."""

    def test_a_nested_mindspace_humbug_dir_is_excluded(self, tmp_path, monkeypatch) -> None:
        """A folder inside this mindspace may be a mindspace too; its state is not ours to read."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        inner = os.path.join(mindspace.mindspace_path(), "projects", "inner", ".humbug", "system.json")

        assert mindspace.is_humbug_path(inner) is True

    def test_a_nested_humbug_dir_is_rejected_by_resolve_tool_path(self, tmp_path, monkeypatch) -> None:
        """Every AI tool resolves paths here, so this is what keeps them out."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        inner = os.path.join(mindspace.mindspace_path(), "inner", ".humbug")
        os.makedirs(inner)

        with pytest.raises(MindspaceHumbugPathError):
            mindspace.resolve_tool_path(os.path.join(inner, "system.json"))

    def test_a_symlink_into_a_nested_humbug_dir_is_rejected(self, tmp_path, monkeypatch) -> None:
        """A link elsewhere in the mindspace cannot reach a nested .humbug/ either."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        inner = os.path.join(mindspace.mindspace_path(), "inner", ".humbug")
        os.makedirs(inner)
        secret = os.path.join(inner, "system.json")
        with open(secret, "w", encoding="utf-8") as f:
            f.write("{}")

        link = os.path.join(mindspace.mindspace_path(), "link.json")
        os.symlink(secret, link)

        with pytest.raises(MindspaceHumbugPathError):
            mindspace.resolve_tool_path(link)

    def test_a_name_merely_containing_humbug_is_not_excluded(self, tmp_path, monkeypatch) -> None:
        """Only a path component that is exactly .humbug counts."""
        mindspace = _make_mindspace(tmp_path, monkeypatch)
        regular = os.path.join(mindspace.mindspace_path(), "my.humbug", "notes.md")

        assert mindspace.is_humbug_path(regular) is False

    def test_a_mindspace_under_a_humbug_folder_is_not_wholly_excluded(self, tmp_path, monkeypatch) -> None:
        """Only components below the mindspace root are checked."""
        mindspace = _new_mindspace()
        path = os.path.join(str(tmp_path), ".humbug", "ms")
        os.makedirs(os.path.join(path, ".humbug"))
        monkeypatch.setattr(mindspace, "_path", path)
        monkeypatch.setattr(mindspace, "_settings", MindspaceSettings(enabled_tools={}))

        assert mindspace.is_humbug_path(os.path.join(path, "src", "file.py")) is False


class TestKeepingHumbugOutOfGit:
    """Tests that .humbug/ ignores itself in any git repository it sits in."""

    def test_a_new_mindspace_ignores_its_humbug_dir(self, tmp_path) -> None:
        """A mindspace created inside a git repository must not have its state committed."""
        _new_mindspace().create_mindspace(str(tmp_path), [])

        with open(os.path.join(str(tmp_path), ".humbug", ".gitignore"), encoding="utf-8") as f:
            assert f.read() == "*\n"

    def test_an_existing_mindspace_gets_the_file_when_opened(self, tmp_path) -> None:
        """Mindspaces created before the file existed are brought up to date."""
        mindspace = _new_mindspace()
        mindspace.create_mindspace(str(tmp_path), [])
        gitignore = os.path.join(str(tmp_path), ".humbug", ".gitignore")
        os.remove(gitignore)

        mindspace.open_mindspace(str(tmp_path))

        with open(gitignore, encoding="utf-8") as f:
            assert f.read() == "*\n"

    def test_an_edited_file_is_left_alone(self, tmp_path) -> None:
        """A user who has changed the file keeps their change."""
        mindspace = _new_mindspace()
        mindspace.create_mindspace(str(tmp_path), [])
        gitignore = os.path.join(str(tmp_path), ".humbug", ".gitignore")
        with open(gitignore, "w", encoding="utf-8") as f:
            f.write("usage.json\n")

        mindspace.open_mindspace(str(tmp_path))

        with open(gitignore, encoding="utf-8") as f:
            assert f.read() == "usage.json\n"
