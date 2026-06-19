"""Cost/Tokens usage tab — enterprise dashboard design."""

import logging
from collections import defaultdict
from typing import Any

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QFrame,
    QGridLayout,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QScrollArea,
    QSizePolicy,
    QVBoxLayout,
    QWidget,
)

from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.status_message import StatusMessage
from desktop.style_manager import StyleManager
from desktop.tab import TabBase, TabState

_MODEL_COLORS = [
    "#4C9BE8",
    "#4EC994",
    "#F5A623",
    "#E85D75",
    "#9B8AF5",
    "#F5724D",
    "#50C8D8",
    "#A8CC5E",
]


def _fmt(n: int) -> str:
    if n >= 1_000_000:
        return f"{n / 1_000_000:.2f}M"
    if n >= 1_000:
        return f"{n / 1_000:.1f}K"
    return f"{n:,}"


class UsageTab(TabBase):
    """Tab showing per-mindspace token and cost usage by provider and model."""

    def __init__(self, tab_id: str, parent: QWidget | None = None) -> None:
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("UsageTab")
        self._mindspace_manager = MindspaceManager()
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()

        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(0)

        self._scroll = QScrollArea(self)
        self._scroll.setWidgetResizable(True)
        self._scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._scroll.setFrameShape(QFrame.Shape.NoFrame)
        root.addWidget(self._scroll)

        self.setObjectName("UsageTabRoot")
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        self._body_widget = QWidget()
        self._body_widget.setObjectName("UsageTabBody")
        self._body_widget.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self._scroll.setWidget(self._body_widget)

        self._body = QVBoxLayout(self._body_widget)
        self._body.setContentsMargins(28, 28, 28, 28)
        self._body.setSpacing(0)
        self._body.setAlignment(Qt.AlignmentFlag.AlignTop)

        self._content = QWidget()
        self._content.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        self._content_layout = QVBoxLayout(self._content)
        self._content_layout.setContentsMargins(0, 0, 0, 0)
        self._content_layout.setSpacing(0)
        self._content_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self._body.addWidget(self._content)
        self._body.addStretch()

        self._mindspace_manager.usage_updated.connect(self.refresh)
        self._language_manager.language_changed.connect(self.refresh)

        self.refresh()

    # ── TabBase ──────────────────────────────────────────────────────────────

    def tool_name(self) -> str:
        return "usage"

    def tab_title_from_path(self) -> str:
        return "Cost/Tokens"

    def on_path_renamed(self, new_path: str) -> None:
        pass

    def set_active(self, widget: QWidget, active: bool) -> None:
        if active:
            self.refresh()
            self.activated.emit()

    def activate(self) -> None:
        self.refresh()

    def set_path(self, path: str) -> None:
        pass

    def can_close_tab(self) -> bool:
        return True

    def close_tab(self) -> None:
        pass

    def can_save(self) -> bool:
        return False

    def save(self) -> bool:
        return True

    def can_save_as(self) -> bool:
        return False

    def save_as(self) -> bool:
        return True

    def can_undo(self) -> bool:
        return False

    def undo(self) -> None:
        pass

    def can_redo(self) -> bool:
        return False

    def redo(self) -> None:
        pass

    def can_cut(self) -> bool:
        return False

    def cut(self) -> None:
        pass

    def can_copy(self) -> bool:
        return False

    def copy(self) -> None:
        pass

    def can_paste(self) -> bool:
        return False

    def paste(self) -> None:
        pass

    def can_submit(self) -> bool:
        return False

    def submit(self) -> None:
        pass

    def show_find(self) -> None:
        pass

    def update_status(self) -> None:
        self.status_message.emit(StatusMessage("Cost/Tokens"))

    def preferred_width(self) -> int | None:
        return int(self._style_manager.nice_tab_width() * self._style_manager.zoom_factor())

    def apply_style(self) -> None:
        self._apply_stylesheet()

    def get_state(self, temp_state: bool = False) -> TabState:
        return TabState(type=self.tool_name(), tab_id=self._tab_id, path="", metadata={})

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> "UsageTab":
        return cls(state.tab_id, parent)

    # ── Refresh ──────────────────────────────────────────────────────────────

    def refresh(self) -> None:
        self._clear(self._content_layout)
        cl = self._content_layout

        if not self._mindspace_manager.has_mindspace():
            cl.addWidget(self._lbl("No mindspace open", "UsageDim"))
            self._apply_stylesheet()
            return

        usage = self._mindspace_manager.mindspace().usage()
        total_cost = usage.total_cost()
        total_in = usage.total_input_tokens()
        total_out = usage.total_output_tokens()
        total_cw = usage.total_cache_write_tokens()
        total_cr = usage.total_cache_read_tokens()
        entries = usage.entries()

        # ── Hero ─────────────────────────────────────────────────────────────
        cl.addWidget(self._section_label("Overview"))
        cl.addSpacing(10)
        cl.addWidget(self._hero_card(total_cost, total_in, total_out))
        cl.addSpacing(32)

        # ── Token stats ───────────────────────────────────────────────────────
        cl.addWidget(self._section_label("Token Usage"))
        cl.addSpacing(10)

        stat_defs = [
            ("Input Tokens",       _fmt(total_in),  "#4C9BE8"),
            ("Output Tokens",      _fmt(total_out), "#4EC994"),
        ]
        if total_cr > 0:
            stat_defs.append(("Cached Tokens",     _fmt(total_cr), "#F5A623"))
        if total_cw > 0:
            stat_defs.append(("Cache Writes",      _fmt(total_cw), "#9B8AF5"))

        cards_w = QWidget()
        cards_w.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        grid = QGridLayout(cards_w)
        grid.setContentsMargins(0, 0, 0, 0)
        grid.setSpacing(12)
        cols = 2
        for c in range(cols):
            grid.setColumnStretch(c, 1)
        for idx, (label, value, accent) in enumerate(stat_defs):
            r, c = divmod(idx, cols)
            grid.addWidget(self._stat_card(label, value, accent), r, c)

        cl.addWidget(cards_w)
        cl.addSpacing(32)

        # ── By model ─────────────────────────────────────────────────────────
        if entries:
            cl.addWidget(self._section_label("By Model"))
            cl.addSpacing(10)

            color_map: dict[str, str] = {}
            for i, e in enumerate(entries):
                color_map[f"{e.provider}/{e.model}"] = _MODEL_COLORS[i % len(_MODEL_COLORS)]

            by_provider: dict[str, list] = defaultdict(list)
            for e in entries:
                by_provider[e.provider].append(e)

            cl.addWidget(self._model_table(by_provider, color_map))
            cl.addSpacing(28)

        # ── Reset ─────────────────────────────────────────────────────────────
        reset_row = QWidget()
        reset_row.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        rl = QHBoxLayout(reset_row)
        rl.setContentsMargins(0, 0, 0, 0)
        btn = QPushButton("Reset Usage")
        btn.setObjectName("UsageResetBtn")
        btn.setEnabled(self._mindspace_manager.has_mindspace())
        btn.clicked.connect(self._on_reset)
        rl.addWidget(btn)
        rl.addStretch()
        cl.addWidget(reset_row)

        self._apply_stylesheet()

    # ── Widget builders ───────────────────────────────────────────────────────

    def _section_label(self, text: str) -> QLabel:
        lbl = QLabel(text.upper())
        lbl.setObjectName("UsageSectionLabel")
        lbl.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        return lbl

    def _hero_card(self, total_cost: float, total_in: int, total_out: int) -> QFrame:
        """Split card: left = cost, right = token summary, separated by a divider."""
        card = QFrame()
        card.setObjectName("UsageHeroCard")
        card.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        hl = QHBoxLayout(card)
        hl.setContentsMargins(0, 0, 0, 0)
        hl.setSpacing(0)

        # Left pane — cost
        left = QWidget()
        left.setObjectName("UsageHeroPane")
        lv = QVBoxLayout(left)
        lv.setContentsMargins(24, 22, 24, 22)
        lv.setSpacing(5)

        cost_lbl = QLabel("Total Spend")
        cost_lbl.setObjectName("UsageHeroPaneLabel")
        lv.addWidget(cost_lbl)

        cost_val = QLabel(f"${total_cost:.4f}")
        cost_val.setObjectName("UsageHeroValue")
        lv.addWidget(cost_val)

        cost_sub = QLabel("Estimated spend for this mindspace")
        cost_sub.setObjectName("UsageHeroSub")
        lv.addWidget(cost_sub)
        lv.addStretch()

        hl.addWidget(left, stretch=1)

        # Divider
        div = QFrame()
        div.setObjectName("UsageHeroDivider")
        div.setFrameShape(QFrame.Shape.VLine)
        div.setFrameShadow(QFrame.Shadow.Plain)
        div.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Expanding)
        hl.addWidget(div)

        # Right pane — tokens
        right = QWidget()
        right.setObjectName("UsageHeroPane")
        rv = QVBoxLayout(right)
        rv.setContentsMargins(24, 22, 24, 22)
        rv.setSpacing(5)

        tok_lbl = QLabel("Total Tokens")
        tok_lbl.setObjectName("UsageHeroPaneLabel")
        rv.addWidget(tok_lbl)

        tok_val = QLabel(_fmt(total_in + total_out))
        tok_val.setObjectName("UsageHeroTokens")
        rv.addWidget(tok_val)

        tok_sub = QLabel(f"in {_fmt(total_in)}  ·  out {_fmt(total_out)}")
        tok_sub.setObjectName("UsageHeroSub")
        rv.addWidget(tok_sub)
        rv.addStretch()

        hl.addWidget(right, stretch=1)

        return card

    def _stat_card(self, label: str, value: str, accent: str) -> QFrame:
        card = QFrame()
        card.setObjectName("UsageCard")
        card.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        card.setProperty("accent", accent)

        vl = QVBoxLayout(card)
        vl.setContentsMargins(20, 18, 20, 18)
        vl.setSpacing(6)

        lbl = QLabel(label)
        lbl.setObjectName("UsageStatLabel")
        vl.addWidget(lbl)

        val = QLabel(value)
        val.setObjectName("UsageStatValue")
        vl.addWidget(val)

        return card

    def _model_table(
        self,
        by_provider: dict[str, list],
        color_map: dict[str, str],
    ) -> QFrame:
        table = QFrame()
        table.setObjectName("UsageTableCard")
        table.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        vl = QVBoxLayout(table)
        vl.setContentsMargins(0, 0, 0, 0)
        vl.setSpacing(0)

        vl.addWidget(self._table_header())
        vl.addWidget(self._row_separator())

        providers = list(by_provider.items())
        for p_idx, (provider, models) in enumerate(providers):
            ph = QWidget()
            ph_hl = QHBoxLayout(ph)
            ph_hl.setContentsMargins(20, 10, 20, 6)
            provider_lbl = QLabel(provider.upper())
            provider_lbl.setObjectName("UsageProviderLabel")
            ph_hl.addWidget(provider_lbl)
            ph_hl.addStretch()
            vl.addWidget(ph)

            for m_idx, entry in enumerate(models):
                key = f"{entry.provider}/{entry.model}"
                color = color_map.get(key, _MODEL_COLORS[0])
                vl.addWidget(self._model_row(entry, color))
                if m_idx < len(models) - 1:
                    vl.addWidget(self._row_separator(indent=20))

            if p_idx < len(providers) - 1:
                vl.addWidget(self._row_separator())

        return table

    def _table_header(self) -> QWidget:
        row = QWidget()
        row.setObjectName("UsageTableHeader")
        hl = QHBoxLayout(row)
        hl.setContentsMargins(20, 12, 20, 12)
        hl.setSpacing(0)

        def _h(text: str, align: Qt.AlignmentFlag = Qt.AlignmentFlag.AlignLeft) -> QLabel:
            lbl = QLabel(text)
            lbl.setObjectName("UsageTableHeaderCell")
            lbl.setAlignment(align)
            return lbl

        hl.addWidget(_h("Model"), stretch=3)
        hl.addWidget(_h("Tokens", Qt.AlignmentFlag.AlignRight), stretch=2)
        hl.addWidget(_h("Cost", Qt.AlignmentFlag.AlignRight), stretch=1)
        return row

    def _model_row(self, entry: Any, color: str) -> QWidget:
        row = QWidget()
        row.setObjectName("UsageModelRow")
        row.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        vl = QVBoxLayout(row)
        vl.setContentsMargins(20, 12, 20, 12)
        vl.setSpacing(4)

        top = QWidget()
        top.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        hl = QHBoxLayout(top)
        hl.setContentsMargins(0, 0, 0, 0)
        hl.setSpacing(0)

        name = QLabel(entry.model)
        name.setObjectName("UsageModelName")
        hl.addWidget(name, stretch=3)

        tok_lbl = QLabel(_fmt(entry.input_tokens + entry.output_tokens))
        tok_lbl.setObjectName("UsageModelTokens")
        tok_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        hl.addWidget(tok_lbl, stretch=2)

        cost_text = f"${entry.cost_usd:.4f}" if entry.cost_usd > 0 else "—"
        cost_lbl = QLabel(cost_text)
        cost_lbl.setObjectName("UsageModelCost")
        cost_lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        hl.addWidget(cost_lbl, stretch=1)

        vl.addWidget(top)

        parts = [f"in {_fmt(entry.input_tokens)}", f"out {_fmt(entry.output_tokens)}"]
        if entry.cache_read_tokens:
            parts.append(f"cached {_fmt(entry.cache_read_tokens)}")
        if entry.cache_write_tokens:
            parts.append(f"cache write {_fmt(entry.cache_write_tokens)}")

        detail = QLabel("  ·  ".join(parts))
        detail.setObjectName("UsageModelDetail")
        detail.setIndent(16)
        detail.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        vl.addWidget(detail)

        return row

    def _row_separator(self, indent: int = 0) -> QFrame:
        line = QFrame()
        line.setObjectName("UsageRowSep")
        line.setFrameShape(QFrame.Shape.HLine)
        line.setFrameShadow(QFrame.Shadow.Plain)
        if indent:
            line.setContentsMargins(indent, 0, 0, 0)
        return line

    def _lbl(self, text: str, obj: str) -> QLabel:
        lbl = QLabel(text)
        lbl.setObjectName(obj)
        lbl.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, False)
        return lbl

    # ── Helpers ───────────────────────────────────────────────────────────────

    def _clear(self, layout: QVBoxLayout) -> None:
        while layout.count():
            item = layout.takeAt(0)
            if item and item.widget():
                item.widget().deleteLater()

    def _on_reset(self) -> None:
        if self._mindspace_manager.has_mindspace():
            self._mindspace_manager.mindspace().reset_usage()

    # ── Stylesheet ────────────────────────────────────────────────────────────

    def _apply_stylesheet(self) -> None:
        zoom = self._style_manager.zoom_factor()
        base = self._style_manager.base_font_size()
        fs = base * zoom

        bg = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        card_bg = self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)
        border = self._style_manager.get_color_str(ColorRole.CODE_BORDER)
        sep = self._style_manager.get_color_str(ColorRole.MESSAGE_BORDER)
        # Subtle hover tint instead of a flat gray fill
        hover_qc = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        hover = f"rgba({hover_qc.red()}, {hover_qc.green()}, {hover_qc.blue()}, 0.06)"
        text = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        dim = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)

        mono = self._style_manager.make_monospace_font().family()

        # Stat cards: per-card inline style so each can carry its own accent
        # colour on the left border (can't express per-instance in a shared sheet).
        for card in self._body_widget.findChildren(QFrame, "UsageCard"):
            accent = card.property("accent")
            card.setStyleSheet(
                f"QFrame#UsageCard {{"
                f"  background-color: {card_bg};"
                f"  border: 1px solid {border};"
                f"  border-left: 4px solid {accent};"
                f"  border-radius: 10px;"
                f"}}"
                f"QFrame#UsageCard QWidget, QFrame#UsageCard QLabel {{"
                f"  background: transparent;"
                f"}}"
            )

        self.setStyleSheet(f"""
            /* Base: paint every surface on this page with the page bg.
               Cards and labels override this below.  A bare QWidget rule is
               the only thing that reliably paints the scroll viewport (object
               -name selectors leave the native window colour showing through). */
            QWidget {{
                background-color: {bg};
            }}
            QScrollArea {{
                border: none;
            }}

            /* ── Section labels ── */
            QLabel#UsageSectionLabel {{
                color: {dim};
                font-size: {round(fs * 1.05)}pt;
                font-weight: bold;
                letter-spacing: 1px;
                background: transparent;
            }}

            /* ── Hero card (no card bg, just a thin border) ── */
            QFrame#UsageHeroCard {{
                background: transparent;
                border: 1px solid {border};
                border-radius: 10px;
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
                font-size: {round(fs * 0.86)}pt;
                font-weight: 600;
                letter-spacing: 0.5px;
                background: transparent;
            }}
            QLabel#UsageHeroValue {{
                color: {text};
                font-size: {round(fs * 2.0)}pt;
                font-weight: bold;
                font-family: "{mono}";
                background: transparent;
            }}
            QLabel#UsageHeroTokens {{
                color: {text};
                font-size: {round(fs * 2.0)}pt;
                font-weight: bold;
                font-family: "{mono}";
                background: transparent;
            }}
            QLabel#UsageHeroSub {{
                color: {dim};
                font-size: {round(fs * 0.83)}pt;
                background: transparent;
            }}
            QFrame#UsageHeroDivider {{
                color: {border};
                border: none;
                border-left: 1px solid {border};
                background: transparent;
                max-width: 1px;
            }}

            /* ── Stat cards ── */
            QLabel#UsageStatLabel {{
                color: {dim};
                font-size: {round(fs * 0.83)}pt;
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

            /* ── Model table ── */
            QFrame#UsageTableCard {{
                background-color: {card_bg};
                border: 1px solid {border};
                border-radius: 10px;
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
                font-size: {round(fs * 0.79)}pt;
                font-weight: bold;
                letter-spacing: 0.5px;
                background: transparent;
            }}

            QLabel#UsageProviderLabel {{
                color: {dim};
                font-size: {round(fs * 0.79)}pt;
                font-weight: bold;
                letter-spacing: 1px;
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
            QLabel#UsageModelCost {{
                color: {text};
                font-size: {round(fs)}pt;
                font-weight: bold;
                font-family: "{mono}";
                background: transparent;
            }}
            QLabel#UsageModelDetail {{
                color: {dim};
                font-size: {round(fs * 0.82)}pt;
                font-family: "{mono}";
                background: transparent;
            }}

            /* ── Separators ── */
            QFrame#UsageRowSep {{
                color: {sep};
                border: none;
                border-top: 1px solid {sep};
                max-height: 1px;
                background: transparent;
            }}

            /* ── Empty state ── */
            QLabel#UsageDim {{
                color: {dim};
                font-size: {round(fs)}pt;
                background: transparent;
            }}

            /* ── Reset button (transparent, outlined) ── */
            QPushButton#UsageResetBtn {{
                background: transparent;
                color: {dim};
                border: 1px solid {border};
                border-radius: 7px;
                padding: {round(7 * zoom)}px {round(20 * zoom)}px;
                font-size: {round(fs * 0.9)}pt;
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
        """)
