"""Token usage widget."""

import logging
from collections import defaultdict

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QFrame,
    QGridLayout,
    QHBoxLayout,
    QLabel,
    QProgressBar,
    QPushButton,
    QScrollArea,
    QSizePolicy,
    QVBoxLayout,
    QWidget,
)

from ai.ai_conversation_settings import AIConversationSettings
from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.style_manager import StyleManager
from mindspace.mindspace_usage import ModelUsageEntry

_PAGE_SIZE = 5


def _fmt(n: int) -> str:
    if n >= 1_000_000:
        return f"{n / 1_000_000:.2f}M"

    if n >= 1_000:
        return f"{n / 1_000:.1f}K"

    return f"{n:,}"


def _count_label(count: int, singular: str, plural: str) -> str:
    word = singular if count == 1 else plural
    return f"{count} {word}"


class UsageWidget(QWidget):
    """Widget showing per-mindspace token usage by provider and model."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._logger = logging.getLogger("UsageWidget")
        self._mindspace_manager = MindspaceManager()
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._provider_page = 0
        self._model_page = 0

        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        self._scroll = QScrollArea(self)
        self._scroll.setWidgetResizable(True)
        self._scroll.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._scroll.setFrameShape(QFrame.Shape.NoFrame)

        # Wrap scroll area in a centring container so the scrollbar sits
        # adjacent to the content rather than at the far right of the column.
        scroll_container = QWidget()
        scroll_container.setObjectName("UsageScrollContainer")
        scroll_container_layout = QHBoxLayout(scroll_container)
        scroll_container_layout.setContentsMargins(0, 0, 0, 0)
        scroll_container_layout.setSpacing(0)
        scroll_container_layout.setAlignment(Qt.AlignmentFlag.AlignHCenter)
        scroll_container_layout.addWidget(self._scroll)
        root.addWidget(scroll_container)

        self._body_widget = QWidget()
        self._body_widget.setObjectName("UsageTabBody")
        self._body_widget.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self._body_widget.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self._scroll.setWidget(self._body_widget)

        self._body = QVBoxLayout(self._body_widget)
        self._body.setSpacing(0)
        self._body.setAlignment(Qt.AlignmentFlag.AlignTop)

        self._content = QWidget()
        self._content.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        self._content_layout = QVBoxLayout(self._content)
        self._content_layout.setContentsMargins(0, 0, 0, 0)
        self._content_layout.setSpacing(0)
        self._content_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self._body.addWidget(self._content)
        self._body.addSpacing(int(self._style_manager.message_bubble_spacing() * 2))

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
        self._body.addStretch()

        self._mindspace_manager.usage_updated.connect(self.refresh)
        self._language_manager.language_changed.connect(self.refresh)
        self._style_manager.style_changed.connect(self.apply_style)

        self.refresh()

    def apply_style(self) -> None:
        """Apply current style settings."""
        s = int(self._style_manager.message_bubble_spacing())
        self._body.setContentsMargins(s, s, s, s)
        zoom = self._style_manager.zoom_factor()
        self._scroll.setMaximumWidth(int(self._style_manager.nice_tab_width() * zoom))
        self._apply_stylesheet()

    def refresh(self) -> None:
        """Rebuild the usage dashboard content from current mindspace usage data."""
        self._clear(self._content_layout)
        cl = self._content_layout

        if not self._mindspace_manager.has_mindspace():
            cl.addWidget(self._empty_state("No mindspace open", "Open a mindspace to track model token usage."))
            self._reset_btn.setEnabled(False)
            self._apply_stylesheet()
            return

        usage = self._mindspace_manager.mindspace().usage()
        total_in = usage.total_input_tokens()
        total_out = usage.total_output_tokens()
        total_cw = usage.total_cache_write_tokens()
        total_cr = usage.total_cache_read_tokens()
        entries = usage.entries()

        model_colors = self._style_manager.model_colors()

        if not entries:
            cl.addWidget(self._hero_card(total_in, total_out, 0, 0))
            cl.addSpacing(int(self._style_manager.message_bubble_spacing()))
            cl.addWidget(self._empty_state(
                "No usage recorded yet",
                "Complete an AI response in this mindspace to populate this dashboard."
            ))
            self._reset_btn.setEnabled(False)
            self._apply_stylesheet()
            return

        cl.addWidget(self._hero_card(total_in, total_out, len(entries), len({e.provider for e in entries})))
        cl.addSpacing(int(self._style_manager.message_bubble_spacing()) * 2)

        cl.addWidget(self._section_label("Token mix"))
        cl.addSpacing(int(self._style_manager.message_bubble_spacing()))

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

        cl.addWidget(cards_w)
        cl.addSpacing(int(self._style_manager.message_bubble_spacing()) * 2)

        color_map: dict[str, str] = {}
        for i, e in enumerate(entries):
            color_map[f"{e.provider}/{e.model}"] = model_colors[i % len(model_colors)]

        by_provider: dict[str, list[ModelUsageEntry]] = defaultdict(list)
        for e in entries:
            by_provider[e.provider].append(e)

        provider_count = len(by_provider)
        model_count = len(entries)
        self._provider_page = self._clamped_page(self._provider_page, provider_count)
        self._model_page = self._clamped_page(self._model_page, model_count)

        cl.addWidget(self._section_label("By provider"))
        cl.addSpacing(int(self._style_manager.message_bubble_spacing()))
        cl.addWidget(self._provider_grid(by_provider))
        if provider_count > _PAGE_SIZE:
            cl.addSpacing(int(self._style_manager.message_bubble_spacing()))
            cl.addWidget(self._pagination_row("providers", self._provider_page, provider_count))

        cl.addSpacing(int(self._style_manager.message_bubble_spacing()) * 2)

        cl.addWidget(self._section_label("Model detail"))
        cl.addSpacing(int(self._style_manager.message_bubble_spacing()))
        cl.addWidget(self._model_table(by_provider, color_map, model_colors, max(total_in + total_out, 1)))
        if model_count > _PAGE_SIZE:
            cl.addSpacing(int(self._style_manager.message_bubble_spacing()))
            cl.addWidget(self._pagination_row("models", self._model_page, model_count))

        self._reset_btn.setEnabled(self._mindspace_manager.has_mindspace())

        self._apply_stylesheet()

    def _section_label(self, text: str) -> QLabel:
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
            f"in {_fmt(total_in)}  /  out {_fmt(total_out)}  ·  "
            f"{_count_label(provider_count, 'provider', 'providers')}  /  "
            f"{_count_label(model_count, 'model', 'models')}"
        )
        tok_sub.setObjectName("UsageHeroSub")
        pv.addWidget(tok_sub)
        pv.addStretch()

        hl.addWidget(pane, stretch=1)

        return card

    def _stat_card(self, label: str, value: str, note: str) -> QFrame:
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

    def _provider_grid(self, by_provider: dict[str, list[ModelUsageEntry]]) -> QWidget:
        widget = QWidget()
        widget.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        grid = QGridLayout(widget)
        grid.setContentsMargins(0, 0, 0, 0)
        grid.setSpacing(int(self._style_manager.message_bubble_spacing()))
        cols = 2
        for c in range(cols):
            grid.setColumnStretch(c, 1)

        providers = self._sorted_providers(by_provider)
        page_providers = providers[self._provider_page * _PAGE_SIZE:(self._provider_page + 1) * _PAGE_SIZE]
        for idx, (provider, entries) in enumerate(page_providers):
            r, c = divmod(idx, cols)
            grid.addWidget(self._provider_card(provider, entries), r, c)

        return widget

    def _sorted_providers(
        self,
        by_provider: dict[str, list[ModelUsageEntry]],
    ) -> list[tuple[str, list[ModelUsageEntry]]]:
        return sorted(
            by_provider.items(),
            key=lambda item: sum(e.input_tokens + e.output_tokens for e in item[1]),
            reverse=True,
        )

    def _provider_card(self, provider: str, entries: list[ModelUsageEntry]) -> QFrame:
        card = QFrame()
        card.setObjectName("UsageProviderCard")
        card.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        tokens = sum(e.input_tokens + e.output_tokens for e in entries)

        s = int(self._style_manager.message_bubble_spacing())
        layout = QVBoxLayout(card)
        layout.setContentsMargins(s, s, s, s)
        layout.setSpacing(s)

        top = QWidget()
        top.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        hl = QHBoxLayout(top)
        hl.setContentsMargins(0, 0, 0, 0)
        hl.setSpacing(s)

        name = QLabel(provider.upper())
        name.setObjectName("UsageProviderName")
        hl.addWidget(name)
        hl.addStretch()

        token_total = QLabel(_fmt(tokens))
        token_total.setObjectName("UsageProviderTokens")
        hl.addWidget(token_total)
        layout.addWidget(top)

        detail = QLabel(f"tokens across {_count_label(len(entries), 'model', 'models')}")
        detail.setObjectName("UsageProviderDetail")
        layout.addWidget(detail)

        return card

    def _model_table(
        self,
        by_provider: dict[str, list[ModelUsageEntry]],
        color_map: dict[str, str],
        model_colors: list[str],
        total_tokens: int,
    ) -> QFrame:
        table = QFrame()
        table.setObjectName("UsageTableCard")
        table.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        vl = QVBoxLayout(table)
        vl.setContentsMargins(0, 0, 0, 0)
        vl.setSpacing(0)

        vl.addWidget(self._table_header())
        vl.addWidget(self._row_separator())

        visible_entries = self._paginated_models(by_provider)
        visible_by_provider: dict[str, list[ModelUsageEntry]] = defaultdict(list)
        for entry in visible_entries:
            visible_by_provider[entry.provider].append(entry)

        providers = self._sorted_providers(visible_by_provider)
        s = int(self._style_manager.message_bubble_spacing())
        for p_idx, (provider, models) in enumerate(providers):
            ph = QWidget()
            ph_hl = QHBoxLayout(ph)
            ph_hl.setContentsMargins(s, s, s, s // 2)
            provider_lbl = QLabel(provider.upper())
            provider_lbl.setObjectName("UsageProviderLabel")
            ph_hl.addWidget(provider_lbl)
            ph_hl.addStretch()
            vl.addWidget(ph)

            for m_idx, entry in enumerate(models):
                key = f"{entry.provider}/{entry.model}"
                color = color_map.get(key, model_colors[0])
                vl.addWidget(self._model_row(entry, color, total_tokens))
                if m_idx < len(models) - 1:
                    vl.addWidget(self._row_separator(indent=s))

            if p_idx < len(providers) - 1:
                vl.addWidget(self._row_separator())

        return table

    def _paginated_models(
        self,
        by_provider: dict[str, list[ModelUsageEntry]],
    ) -> list[ModelUsageEntry]:
        entries = [
            entry
            for _, models in self._sorted_providers(by_provider)
            for entry in sorted(models, key=lambda e: e.input_tokens + e.output_tokens, reverse=True)
        ]
        start = self._model_page * _PAGE_SIZE
        return entries[start:start + _PAGE_SIZE]

    def _table_header(self) -> QWidget:
        row = QWidget()
        row.setObjectName("UsageTableHeader")
        hl = QHBoxLayout(row)
        s = int(self._style_manager.message_bubble_spacing())
        hl.setContentsMargins(s, s, s, s)
        hl.setSpacing(0)

        def _h(text: str, align: Qt.AlignmentFlag = Qt.AlignmentFlag.AlignLeft) -> QLabel:
            lbl = QLabel(text)
            lbl.setObjectName("UsageTableHeaderCell")
            lbl.setAlignment(align)
            return lbl

        hl.addWidget(_h("Model"), stretch=3)
        hl.addWidget(_h("Tokens", Qt.AlignmentFlag.AlignRight), stretch=2)
        return row

    def _model_row(self, entry: ModelUsageEntry, color: str, total_tokens: int) -> QWidget:
        row = QWidget()
        row.setObjectName("UsageModelRow")
        row.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

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

        detail = QLabel("  /  ".join(parts))
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

    def _pagination_row(self, item_name: str, page: int, item_count: int) -> QWidget:
        row = QWidget()
        row.setObjectName("UsagePager")
        row.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)

        layout = QHBoxLayout(row)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(int(self._style_manager.message_bubble_spacing()))

        page_count = self._page_count(item_count)
        start = page * _PAGE_SIZE + 1
        end = min((page + 1) * _PAGE_SIZE, item_count)

        label = QLabel(f"{start}-{end} of {item_count} {item_name}")
        label.setObjectName("UsagePagerLabel")
        layout.addWidget(label)
        layout.addStretch()

        previous_button = QPushButton("Previous")
        previous_button.setObjectName("UsagePagerButton")
        previous_button.setEnabled(page > 0)
        previous_button.clicked.connect(
            self._previous_provider_page if item_name == "providers" else self._previous_model_page
        )
        layout.addWidget(previous_button)

        page_label = QLabel(f"Page {page + 1} of {page_count}")
        page_label.setObjectName("UsagePagerPage")
        page_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(page_label)

        next_button = QPushButton("Next")
        next_button.setObjectName("UsagePagerButton")
        next_button.setEnabled(page < page_count - 1)
        next_button.clicked.connect(
            self._next_provider_page if item_name == "providers" else self._next_model_page
        )
        layout.addWidget(next_button)

        return row

    def _page_count(self, item_count: int) -> int:
        return max((item_count + _PAGE_SIZE - 1) // _PAGE_SIZE, 1)

    def _clamped_page(self, page: int, item_count: int) -> int:
        return min(max(page, 0), self._page_count(item_count) - 1)

    def _row_separator(self, indent: int = 0) -> QFrame:
        line = QFrame()
        line.setObjectName("UsageRowSep")
        line.setFrameShape(QFrame.Shape.HLine)
        line.setFrameShadow(QFrame.Shadow.Plain)
        if indent:
            line.setContentsMargins(indent, 0, 0, 0)
        return line

    def _empty_state(self, title: str, message: str) -> QFrame:
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

    def _clear(self, layout: QVBoxLayout) -> None:
        while layout.count():
            item = layout.takeAt(0)
            if item:
                widget = item.widget()
                if widget:
                    widget.deleteLater()

    def _on_reset(self) -> None:
        if self._mindspace_manager.has_mindspace():
            self._provider_page = 0
            self._model_page = 0
            self._mindspace_manager.mindspace().reset_usage()

    def _previous_provider_page(self) -> None:
        self._provider_page = max(self._provider_page - 1, 0)
        self.refresh()

    def _next_provider_page(self) -> None:
        self._provider_page += 1
        self.refresh()

    def _previous_model_page(self) -> None:
        self._model_page = max(self._model_page - 1, 0)
        self.refresh()

    def _next_model_page(self) -> None:
        self._model_page += 1
        self.refresh()

    def _apply_stylesheet(self) -> None:
        zoom = self._style_manager.zoom_factor()
        base = self._style_manager.base_font_size()
        fs = base * zoom

        soft_bg = self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        msg_bg = self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)
        border = self._style_manager.get_color_str(ColorRole.MESSAGE_USER_BORDER)
        sep = self._style_manager.get_color_str(ColorRole.MESSAGE_BORDER)
        hover_qc = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        hover = f"rgba({hover_qc.red()}, {hover_qc.green()}, {hover_qc.blue()}, 0.06)"
        text = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        dim = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
        heading = self._style_manager.get_color_str(ColorRole.TEXT_HEADING)

        mono = self._style_manager.make_monospace_font().family()

        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            #UsageScrollContainer {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BAR_BACKGROUND)};
            }}

            QScrollArea {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            }}

            QLabel#UsageSectionLabel {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: bold;
                letter-spacing: 0;
                background: transparent;
            }}

            QFrame#UsageHeroCard {{
                background-color: {soft_bg};
                border: 1px solid {border};
                border-radius: 8px;
            }}
            QFrame#UsageHeroCard QWidget,
            QFrame#UsageHeroCard QLabel {{
                background: transparent;
            }}
            QWidget#UsageHeroPane {{
                background: transparent;
            }}
            QLabel#UsageHeroPaneLabel {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: 600;
                letter-spacing: 0;
                background: transparent;
            }}
            QLabel#UsageHeroTokens {{
                color: {heading};
                font-size: {round(fs * 2.0)}pt;
                font-weight: bold;
                font-family: "{mono}";
                background: transparent;
            }}
            QLabel#UsageHeroSub {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
                background: transparent;
            }}

            QFrame#UsageCard {{
                background-color: {soft_bg};
                border: 1px solid {border};
                border-radius: 8px;
            }}
            QFrame#UsageCard QWidget, QFrame#UsageCard QLabel {{
                background: transparent;
            }}

            QLabel#UsageStatLabel {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: 600;
                background: transparent;
            }}
            QLabel#UsageStatValue {{
                color: {text};
                font-size: {round(fs * 1.6)}pt;
                font-weight: bold;
                font-family: "{mono}";
                background: transparent;
            }}
            QLabel#UsageStatNote {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
                background: transparent;
            }}

            QFrame#UsageProviderCard {{
                background-color: {msg_bg};
                border: 1px solid {sep};
                border-radius: 8px;
            }}
            QFrame#UsageProviderCard QWidget,
            QFrame#UsageProviderCard QLabel {{
                background: transparent;
            }}
            QLabel#UsageProviderName {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: bold;
                letter-spacing: 0;
            }}
            QLabel#UsageProviderTokens {{
                color: {text};
                font-size: {round(fs * 1.18)}pt;
                font-weight: bold;
                font-family: "{mono}";
            }}
            QLabel#UsageProviderDetail {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
            }}

            QFrame#UsageTableCard {{
                background-color: {msg_bg};
                border: 1px solid {sep};
                border-radius: 8px;
            }}
            QFrame#UsageTableCard QWidget,
            QFrame#UsageTableCard QLabel {{
                background: transparent;
            }}

            QWidget#UsageTableHeader {{
                background: transparent;
            }}
            QLabel#UsageTableHeaderCell {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: bold;
                letter-spacing: 0;
                background: transparent;
            }}

            QLabel#UsageProviderLabel {{
                color: {dim};
                font-size: {round(fs * 0.9)}pt;
                font-weight: bold;
                letter-spacing: 0;
                background: transparent;
            }}

            QWidget#UsageModelRow {{
                background: transparent;
                border-radius: 6px;
            }}
            QWidget#UsageModelRow:hover {{
                background-color: {hover};
            }}
            QLabel#UsageModelName {{
                color: {text};
                font-size: {round(fs)}pt;
                font-weight: 500;
                background: transparent;
            }}
            QLabel#UsageModelTokens {{
                color: {dim};
                font-size: {round(fs)}pt;
                font-family: "{mono}";
                background: transparent;
            }}
            QLabel#UsageModelDetail {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
                font-family: "{mono}";
                background: transparent;
            }}

            QWidget#UsagePager {{
                background: transparent;
            }}
            QLabel#UsagePagerLabel,
            QLabel#UsagePagerPage {{
                color: {dim};
                font-size: {round(fs * 0.85)}pt;
                background: transparent;
            }}
            QLabel#UsagePagerPage {{
                min-width: {round(86 * zoom)}px;
            }}
            QPushButton#UsagePagerButton {{
                background: transparent;
                color: {dim};
                border: 1px solid {border};
                border-radius: 7px;
                padding: {round(5 * zoom)}px {round(12 * zoom)}px;
                font-size: {round(fs * 0.85)}pt;
                font-weight: 500;
            }}
            QPushButton#UsagePagerButton:hover {{
                background-color: {hover};
                color: {text};
                border-color: {dim};
            }}
            QPushButton#UsagePagerButton:pressed {{
                background-color: {hover};
            }}
            QPushButton#UsagePagerButton:disabled {{
                color: {dim};
                border-color: {sep};
            }}

            QFrame#UsageRowSep {{
                color: {sep};
                border: none;
                border-top: 1px solid {sep};
                max-height: 1px;
                background: transparent;
            }}

            QFrame#UsageEmptyCard {{
                background-color: {soft_bg};
                border: 1px solid {border};
                border-radius: 8px;
            }}
            QFrame#UsageEmptyCard QLabel {{
                background: transparent;
            }}
            QLabel#UsageEmptyTitle {{
                color: {text};
                font-size: {round(fs * 1.08)}pt;
                font-weight: bold;
            }}
            QLabel#UsageEmptyMessage {{
                color: {dim};
                font-size: {round(fs)}pt;
            }}

            QPushButton#UsageResetBtn {{
                background: transparent;
                color: {dim};
                border: 1px solid {border};
                border-radius: 7px;
                padding: {round(7 * zoom)}px {round(20 * zoom)}px;
                font-size: {round(fs)}pt;
                font-weight: 500;
            }}
            QPushButton#UsageResetBtn:hover {{
                background-color: {hover};
                color: {text};
                border-color: {dim};
            }}
            QPushButton#UsageResetBtn:pressed {{
                background-color: {hover};
            }}
            QPushButton#UsageResetBtn:disabled {{
                color: {dim};
                border-color: {sep};
            }}

            {self._style_manager.get_scrollbar_stylesheet()}
        """)
