"""Tests for Mindspace AI settings broadcasting and session state persistence."""
import os

import pytest

from ai import AIConversationSettings
from context.context_registry import ContextRegistry
from conversation_context.conversation_context import ConversationContext
from mindspace.mindspace import Mindspace
from mindspace.mindspace_error import MindspaceError, MindspaceHumbugPathError
from mindspace.mindspace_settings import MindspaceSettings


def _make_mindspace(tmp_path, monkeypatch) -> Mindspace:
    """Construct a Mindspace with an open mindspace at a temp path."""
    mindspace = Mindspace(
        on_settings_changed=lambda: None,
        on_interactions_updated=lambda: None,
    )
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
