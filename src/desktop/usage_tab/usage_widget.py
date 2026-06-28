"""Token usage widget."""

import logging
from collections import defaultdict

from PySide6.QtCore import Qt, Signal
from PySide6.QtWidgets import (
    QFrame,
    QGridLayout,
    QLabel,
    QLayout,
    QProgressBar,
    QPushButton,
    QSizePolicy,
    QVBoxLayout,
    QHBoxLayout,
    QWidget,
)

from ai.ai_conversation_settings import AIConversationSettings
from desktop.language.language_manager import LanguageManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.style_manager import StyleManager
from desktop.widgets import Accordion, ElidedLabel
from mindspace.mindspace_usage import ModelUsageEntry


def _fmt(n: int) -> str:
    """Format an integer count with K/M suffixes for compact display."""
    if n >= 1_000_000:
        return f"{n / 1_000_000:.2f}M"

    if n >= 1_000:
        return f"{n / 1_000:.1f}K"

    return f"{n:,}"


def _count_label(count: int, singular: str, plural: str) -> str:
    """Return a count with the singular or plural noun as appropriate."""
    word = singular if count == 1 else plural
    return f"{count} {word}"


class UsageWidget(QWidget):
    """Widget showing per-mindspace token usage by provider and model."""

    refreshed = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._logger = logging.getLogger("UsageWidget")
        self.setObjectName("UsageWidget")
        self._mindspace_manager = MindspaceManager()
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()

        # Providers whose accordion section is expanded, remembered across refreshes.
        self._expanded_providers: set[str] = set()

        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        self._body = QVBoxLayout(self)
        self._body.setSpacing(0)
        self._body.setContentsMargins(0, 0, 0, 0)

        self._summary = QWidget()
        self._summary.setObjectName("UsageSummaryContainer")
        self._summary.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        self._summary.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self._summary_layout = QVBoxLayout(self._summary)
        self._summary_layout.setContentsMargins(0, 0, 0, 0)
        self._summary_layout.setSpacing(0)
        self._summary_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self._body.addWidget(self._summary)

        self._details = QWidget()
        self._details.setObjectName("UsageDetailsContainer")
        self._details.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        self._details.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._details_container_layout = QVBoxLayout(self._details)
        self._details_container_layout.setContentsMargins(0, 0, 0, 0)
        self._details_container_layout.setSpacing(0)

        self._details_container_layout.addSpacing(int(self._style_manager.message_bubble_spacing()))
        self._details_label = self._section_label("Details")
        self._details_container_layout.addWidget(self._details_label)

        self._details_layout = QVBoxLayout()
        self._details_layout.setContentsMargins(0, 0, 0, 0)
        self._details_layout.setSpacing(int(self._style_manager.message_bubble_spacing()))
        self._details_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self._details_container_layout.addLayout(self._details_layout, 1)

        self._body.addWidget(self._details, 1)

        reset_row = QWidget()
        reset_row.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        rl = QHBoxLayout(reset_row)
        rl.setContentsMargins(0, 0, 0, 0)
        self._reset_btn = QPushButton("Reset Usage")
        self._reset_btn.setObjectName("UsageResetBtn")
        self._reset_btn.setEnabled(False)
        self._reset_btn.clicked.connect(self._on_reset)
        rl.addWidget(self._reset_btn)
        rl.addStretch()
        self._body.addWidget(reset_row)

        self._mindspace_manager.usage_updated.connect(self.refresh)
        self._language_manager.language_changed.connect(self.refresh)

        self.refresh()

    def apply_style(self) -> None:
        """Apply current style settings."""
        s = int(self._style_manager.message_bubble_spacing())
        self._body.setContentsMargins(s, s, s, s)
        self._body.setSpacing(s)
        self._summary.setFixedHeight(self._summary_height())
        self._details_container_layout.setSpacing(s)
        self._details_layout.setSpacing(s)
        self.refreshed.emit()

    def refresh(self) -> None:
        """Rebuild the usage dashboard content from current mindspace usage data."""
        self._clear(self._summary_layout)
        self._clear(self._details_layout)
        sl = self._summary_layout
        dl = self._details_layout

        if not self._mindspace_manager.has_mindspace():
            sl.addWidget(self._empty_state("No mindspace open", "Open a mindspace to track model token usage."))
            self._details.setVisible(False)
            self._reset_btn.setEnabled(False)
            self.refreshed.emit()
            return

        usage = self._mindspace_manager.mindspace().usage()
        total_in = usage.total_input_tokens()
        total_out = usage.total_output_tokens()
        total_cw = usage.total_cache_write_tokens()
        total_cr = usage.total_cache_read_tokens()
        entries = usage.entries()

        model_colors = self._style_manager.model_colors()

        if not entries:
            sl.addWidget(self._hero_card(total_in, total_out, 0, 0))
            sl.addSpacing(int(self._style_manager.message_bubble_spacing()))
            sl.addWidget(self._empty_state(
                "No usage recorded yet",
                "Complete an AI response in this mindspace to populate this dashboard."
            ))
            self._details.setVisible(False)
            self._reset_btn.setEnabled(False)
            self.refreshed.emit()
            return

        self._details.setVisible(True)
        sl.addWidget(self._hero_card(total_in, total_out, len(entries), len({e.provider for e in entries})))
        sl.addSpacing(int(self._style_manager.message_bubble_spacing()) * 2)

        sl.addWidget(self._section_label("Token mix"))
        sl.addSpacing(int(self._style_manager.message_bubble_spacing()))

        stat_defs = [
            ("Input", _fmt(total_in), "Prompt and context tokens"),
            ("Output", _fmt(total_out), "Generated response tokens"),
        ]
        if total_cr > 0:
            stat_defs.append(("Cache hits", _fmt(total_cr), "Tokens read from cache"))

        if total_cw > 0:
            stat_defs.append(("Cache writes", _fmt(total_cw), "Tokens added to cache"))

        cards_w = QWidget()
        cards_w.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        grid = QGridLayout(cards_w)
        grid.setContentsMargins(0, 0, 0, 0)
        grid.setSpacing(int(self._style_manager.message_bubble_spacing()))
        cols = 2
        for c in range(cols):
            grid.setColumnStretch(c, 1)

        for idx, (label, value, note) in enumerate(stat_defs):
            r, c = divmod(idx, cols)
            grid.addWidget(self._stat_card(label, value, note), r, c)

        sl.addWidget(cards_w)
        sl.addStretch()

        color_map: dict[str, str] = {}
        for i, e in enumerate(entries):
            color_map[f"{e.provider}/{e.model}"] = model_colors[i % len(model_colors)]

        by_provider: dict[str, list[ModelUsageEntry]] = defaultdict(list)
        for e in entries:
            by_provider[e.provider].append(e)

        dl.addWidget(self._details_section(
            by_provider, color_map, model_colors, max(total_in + total_out, 1),
        ))
        dl.addStretch()

        self._reset_btn.setEnabled(self._mindspace_manager.has_mindspace())

        self.refreshed.emit()

    def _summary_height(self) -> int:
        """Return the fixed height reserved for non-details usage content."""
        zoom = self._style_manager.zoom_factor()
        return max(300, int(330 * zoom))

    def _section_label(self, text: str) -> QLabel:
        """Create an uppercased styled section label widget."""
        lbl = QLabel(text.upper())
        lbl.setObjectName("UsageSectionLabel")
        lbl.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        return lbl

    def _hero_card(
        self,
        total_in: int,
        total_out: int,
        model_count: int,
        provider_count: int,
    ) -> QFrame:
        """Return the top-level token usage summary."""
        card = QFrame()
        card.setObjectName("UsageHeroCard")
        card.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        hl = QHBoxLayout(card)
        hl.setContentsMargins(0, 0, 0, 0)
        hl.setSpacing(0)

        pane = QWidget()
        pane.setObjectName("UsageHeroPane")
        pv = QVBoxLayout(pane)
        s = int(self._style_manager.message_bubble_spacing())
        pv.setContentsMargins(s, s, s, s)
        pv.setSpacing(s // 2)

        tok_lbl = QLabel("Total Tokens")
        tok_lbl.setObjectName("UsageHeroPaneLabel")
        pv.addWidget(tok_lbl)

        tok_val = QLabel(_fmt(total_in + total_out))
        tok_val.setObjectName("UsageHeroTokens")
        pv.addWidget(tok_val)

        tok_sub = QLabel(
            f"in {_fmt(total_in)} / out {_fmt(total_out)}  \u00b7  "
            f"{_count_label(provider_count, 'provider', 'providers')} / "
            f"{_count_label(model_count, 'model', 'models')}"
        )
        tok_sub.setObjectName("UsageHeroSub")
        pv.addWidget(tok_sub)
        pv.addStretch()

        hl.addWidget(pane, stretch=1)

        return card

    def _stat_card(self, label: str, value: str, note: str) -> QFrame:
        """Create a stat card widget showing a label, value, and note."""
        card = QFrame()
        card.setObjectName("UsageCard")
        card.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        s = int(self._style_manager.message_bubble_spacing())
        vl = QVBoxLayout(card)
        vl.setContentsMargins(s, s, s, s)
        vl.setSpacing(s // 2)

        lbl = QLabel(label)
        lbl.setObjectName("UsageStatLabel")
        vl.addWidget(lbl)

        val = QLabel(value)
        val.setObjectName("UsageStatValue")
        vl.addWidget(val)

        note_lbl = QLabel(note)
        note_lbl.setObjectName("UsageStatNote")
        note_lbl.setWordWrap(True)
        vl.addWidget(note_lbl)

        return card

    def _sorted_providers(
        self,
        by_provider: dict[str, list[ModelUsageEntry]],
    ) -> list[tuple[str, list[ModelUsageEntry]]]:
        """Return provider entries sorted by total token usage, descending."""
        return sorted(
            by_provider.items(),
            key=lambda item: sum(e.input_tokens + e.output_tokens for e in item[1]),
            reverse=True,
        )

    def _details_section(
        self,
        by_provider: dict[str, list[ModelUsageEntry]],
        color_map: dict[str, str],
        model_colors: list[str],
        total_tokens: int,
    ) -> QWidget:
        """Build the per-provider details section with accordion cards."""
        container = QWidget()
        container.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        cl = QVBoxLayout(container)
        cl.setContentsMargins(0, 0, 0, 0)
        cl.setSpacing(int(self._style_manager.message_bubble_spacing()))

        sorted_providers = self._sorted_providers(by_provider)

        for provider, models in sorted_providers:
            cl.addWidget(self._provider_detail_card(
                provider, models, color_map, model_colors, total_tokens,
            ))

        return container

    def _provider_detail_card(
        self,
        provider: str,
        entries: list[ModelUsageEntry],
        color_map: dict[str, str],
        model_colors: list[str],
        total_tokens: int,
    ) -> QFrame:
        """Create an accordion card showing per-model usage for one provider."""
        s = int(self._style_manager.message_bubble_spacing())
        zoom = self._style_manager.zoom_factor()

        tokens = sum(e.input_tokens + e.output_tokens for e in entries)

        sorted_models = sorted(
            entries,
            key=lambda e: e.input_tokens + e.output_tokens,
            reverse=True,
        )

        accordion = Accordion(expanded=provider in self._expanded_providers)
        accordion.toggled.connect(
            lambda expanded, name=provider: self._on_provider_toggled(name, expanded)
        )

        header = accordion.header_layout()
        name = ElidedLabel()
        name.setText(provider.upper())
        name.setObjectName("UsageDetailProviderName")
        name.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Preferred)
        header.addWidget(name, 1)

        models_lbl = QLabel(_count_label(len(entries), "model", "models"))
        models_lbl.setObjectName("UsageDetailProviderMeta")
        models_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        models_lbl.setFixedWidth(max(models_lbl.sizeHint().width(), int(76 * zoom)))
        header.addWidget(models_lbl)

        tokens_lbl = QLabel(f"{_fmt(tokens)} tokens")
        tokens_lbl.setObjectName("UsageDetailProviderTokens")
        tokens_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        tokens_lbl.setFixedWidth(max(tokens_lbl.sizeHint().width(), int(112 * zoom)))
        header.addWidget(tokens_lbl)

        body = accordion.body_layout()
        s_pad = int(s * 0.5)
        body.setContentsMargins(0, s_pad, 0, s_pad)

        for m_idx, entry in enumerate(sorted_models):
            key = f"{entry.provider}/{entry.model}"
            color = color_map.get(key, model_colors[0])
            body.addWidget(self._model_row(entry, color, total_tokens))
            if m_idx < len(sorted_models) - 1:
                body.addWidget(self._row_separator(indent=s))

        return accordion

    def _on_provider_toggled(self, provider: str, expanded: bool) -> None:
        """Track which provider accordions are expanded across refreshes."""
        if expanded:
            self._expanded_providers.add(provider)

        else:
            self._expanded_providers.discard(provider)

    def _model_row(self, entry: ModelUsageEntry, color: str, total_tokens: int) -> QWidget:
        """Create a widget showing a single model's token usage with a share bar."""
        row = QWidget()
        row.setObjectName("UsageModelRow")
        row.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)

        s = int(self._style_manager.message_bubble_spacing())
        vl = QVBoxLayout(row)
        vl.setContentsMargins(s, s, s, s)
        vl.setSpacing(s // 2)

        top = QWidget()
        top.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        hl = QHBoxLayout(top)
        hl.setContentsMargins(0, 0, 0, 0)
        hl.setSpacing(0)

        name = QLabel(AIConversationSettings.get_display_name(entry.model, entry.provider))
        name.setObjectName("UsageModelName")
        name.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        hl.addWidget(name, stretch=3)

        model_tokens = entry.input_tokens + entry.output_tokens
        tok_lbl = QLabel(_fmt(model_tokens))
        tok_lbl.setObjectName("UsageModelTokens")
        tok_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        hl.addWidget(tok_lbl, stretch=2)

        vl.addWidget(top)

        parts = [f"in {_fmt(entry.input_tokens)}", f"out {_fmt(entry.output_tokens)}"]
        if entry.cache_read_tokens:
            parts.append(f"cached {_fmt(entry.cache_read_tokens)}")

        if entry.cache_write_tokens:
            parts.append(f"cache write {_fmt(entry.cache_write_tokens)}")

        detail = QLabel(" / ".join(parts))
        detail.setObjectName("UsageModelDetail")
        detail.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        vl.addWidget(detail)

        share_bar = QProgressBar()
        share_bar.setObjectName("UsageSpendBar")
        share_bar.setRange(0, 1000)
        share_bar.setValue(round((model_tokens / total_tokens) * 1000))
        share_bar.setTextVisible(False)
        share_bar.setFixedHeight(6)
        share_bar.setStyleSheet(
            "QProgressBar#UsageSpendBar {"
            "  background: transparent;"
            "  border: none;"
            "}"
            "QProgressBar#UsageSpendBar::chunk {"
            f"  background-color: {color};"
            "  border-radius: 3px;"
            "}"
        )
        vl.addWidget(share_bar)

        return row

    def _row_separator(self, indent: int = 0) -> QFrame:
        """Create a thin horizontal separator frame with optional indent."""
        line = QFrame()
        line.setObjectName("UsageRowSep")
        line.setFrameShape(QFrame.Shape.HLine)
        line.setFrameShadow(QFrame.Shadow.Plain)
        if indent:
            line.setContentsMargins(indent, 0, 0, 0)

        return line

    def _empty_state(self, title: str, message: str) -> QFrame:
        """Create an empty-state card with a title and message."""
        card = QFrame()
        card.setObjectName("UsageEmptyCard")
        card.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        s = int(self._style_manager.message_bubble_spacing())
        layout = QVBoxLayout(card)
        layout.setContentsMargins(s, s, s, s)
        layout.setSpacing(s)

        title_label = QLabel(title)
        title_label.setObjectName("UsageEmptyTitle")
        layout.addWidget(title_label)

        message_label = QLabel(message)
        message_label.setObjectName("UsageEmptyMessage")
        message_label.setWordWrap(True)
        layout.addWidget(message_label)

        return card

    def _clear(self, layout: QLayout) -> None:
        """Remove and delete all widgets from a layout."""
        while layout.count():
            item = layout.takeAt(0)
            if item:
                widget = item.widget()
                if widget:
                    widget.deleteLater()

    def _on_reset(self) -> None:
        """Reset usage data for the current mindspace."""
        if self._mindspace_manager.has_mindspace():
            self._mindspace_manager.mindspace().reset_usage()

    def expanded_providers(self) -> set[str]:
        """Return the set of provider names whose accordion is currently expanded."""
        return set(self._expanded_providers)

    def restore_expanded_providers(self, providers: set[str]) -> None:
        """
        Replace the expanded-provider set and rebuild the display.

        Used by UsageTab to restore ephemeral expand/collapse state when the
        tab is moved between columns.
        """
        self._expanded_providers = set(providers)
        self.refresh()
