"""Tests for Mindspace AI settings broadcasting."""
import os

from ai import AIConversationSettings
from context.context_registry import ContextRegistry
from conversation_context.conversation_context import ConversationContext
from mindspace.mindspace import Mindspace
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
