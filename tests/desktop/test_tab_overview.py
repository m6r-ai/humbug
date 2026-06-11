"""Tests for the tab overview overlay (grid of swipeable thumbnail cards)."""
# pylint: disable=protected-access, missing-class-docstring, missing-function-docstring

from PySide6.QtCore import QEvent, QPoint, QRect, Qt
from PySide6.QtGui import QKeyEvent, QPixmap

from desktop.tab_manager.tab_overview import TabOverviewWidget, aspect_fill_source_rect


def make_overview(host, entries):
    overview = TabOverviewWidget(host)
    overview.setGeometry(host.rect())
    overview.set_entries(entries)
    overview.show()
    return overview


def press_key(qapp, widget, key, modifier=Qt.KeyboardModifier.NoModifier):
    qapp.sendEvent(widget, QKeyEvent(QEvent.Type.KeyPress, key, modifier))


class TestAspectFillSourceRect:
    def test_full_pixmap_when_aspect_matches(self):
        pixmap = QPixmap(800, 600)
        crop = aspect_fill_source_rect(pixmap, QRect(0, 0, 400, 300))
        assert crop == QRect(0, 0, 800, 600)

    def test_wide_source_cropped_horizontally_centred(self):
        pixmap = QPixmap(1600, 600)
        crop = aspect_fill_source_rect(pixmap, QRect(0, 0, 400, 300))
        assert crop.height() == 600
        assert crop.width() == 800
        assert crop.left() == 400

    def test_tall_source_cropped_to_top(self):
        pixmap = QPixmap(400, 1200)
        crop = aspect_fill_source_rect(pixmap, QRect(0, 0, 400, 300))
        assert crop.top() == 0
        assert crop.width() == 400
        assert crop.height() == 300

    def test_degenerate_inputs_give_empty_rect(self):
        assert aspect_fill_source_rect(QPixmap(), QRect(0, 0, 100, 100)).isEmpty()
        assert aspect_fill_source_rect(QPixmap(10, 10), QRect(0, 0, 0, 0)).isEmpty()


class TestSelection:
    def test_initial_selection_is_current_tab(self, host, make_entries):
        overview = make_overview(host, make_entries(current=2))
        assert overview.selected_tab_id() == "t2"

    def test_cycle_forward_wraps(self, host, make_entries):
        overview = make_overview(host, make_entries(count=3, current=2))
        overview.cycle_selection()
        assert overview.selected_tab_id() == "t0"

    def test_cycle_backward_wraps(self, host, make_entries):
        overview = make_overview(host, make_entries(count=3, current=0))
        overview.cycle_selection(-1)
        assert overview.selected_tab_id() == "t2"

    def test_removing_selected_card_repairs_selection(self, host, make_entries):
        overview = make_overview(host, make_entries(count=3, current=1))
        overview.remove_card("t1")
        assert overview.card_count() == 2
        assert overview.selected_tab_id() == "t0"

    def test_key_navigation_and_activation(self, qapp, host, make_entries):
        overview = make_overview(host, make_entries(count=4, current=0))
        activated = []
        overview.tab_activated.connect(activated.append)

        press_key(qapp, overview, Qt.Key.Key_Right)
        press_key(qapp, overview, Qt.Key.Key_Tab)
        assert overview.selected_tab_id() == "t2"

        press_key(qapp, overview, Qt.Key.Key_Left)
        press_key(qapp, overview, Qt.Key.Key_Backtab)
        assert overview.selected_tab_id() == "t0"

        press_key(qapp, overview, Qt.Key.Key_Return)
        assert activated == ["t0"]

    def test_escape_dismisses(self, qapp, host, make_entries):
        overview = make_overview(host, make_entries())
        dismissed = []
        overview.dismissed.connect(lambda: dismissed.append(True))
        press_key(qapp, overview, Qt.Key.Key_Escape)
        assert dismissed


class TestCardInteraction:
    def test_click_activates(self, qapp, host, make_entries, mouse):
        overview = make_overview(host, make_entries())
        qapp.processEvents()
        activated = []
        overview.tab_activated.connect(activated.append)

        card = overview._cards["t0"]
        centre = card.rect().center()
        mouse(card, QEvent.Type.MouseButtonPress, centre)
        mouse(card, QEvent.Type.MouseButtonRelease, centre)
        assert activated == ["t0"]

    def test_close_button_emits_close(self, qapp, host, make_entries, mouse):
        overview = make_overview(host, make_entries())
        qapp.processEvents()
        closed = []
        overview.tab_close_requested.connect(closed.append)

        card = overview._cards["t1"]
        pos = card._close_rect().center()
        mouse(card, QEvent.Type.MouseButtonPress, pos)
        mouse(card, QEvent.Type.MouseButtonRelease, pos)
        assert closed == ["t1"]

    def test_swipe_up_closes(self, qapp, host, make_entries, mouse, settle):
        overview = make_overview(host, make_entries())
        qapp.processEvents()
        closed = []
        overview.tab_close_requested.connect(closed.append)

        card = overview._cards["t0"]
        start = card.rect().center()
        mouse(card, QEvent.Type.MouseButtonPress, start)
        mouse(card, QEvent.Type.MouseMove, start + QPoint(0, -card.height()))
        mouse(card, QEvent.Type.MouseButtonRelease, start + QPoint(0, -card.height()))
        settle()
        assert closed == ["t0"]

    def test_small_swipe_springs_back(self, qapp, host, make_entries, mouse, settle):
        overview = make_overview(host, make_entries())
        qapp.processEvents()
        closed = []
        overview.tab_close_requested.connect(closed.append)

        card = overview._cards["t0"]
        start = card.rect().center()
        mouse(card, QEvent.Type.MouseButtonPress, start)
        mouse(card, QEvent.Type.MouseMove, start + QPoint(0, -20))
        mouse(card, QEvent.Type.MouseButtonRelease, start + QPoint(0, -20))
        settle()
        assert not closed


class TestRendering:
    def test_retina_thumbnail_fills_card_body(self, qapp, host, make_entries):
        overview = make_overview(host, make_entries(width=1600, height=1200, dpr=2.0))
        qapp.processEvents()

        card = overview._cards["t0"]
        image = card.grab().toImage()
        corner = image.pixelColor(image.width() - 6, image.height() - 6)
        assert corner.red() > 200
        assert corner.blue() < 80

    def test_set_entries_replaces_cards(self, host, make_entries):
        overview = make_overview(host, make_entries(count=5))
        assert overview.card_count() == 5
        overview.set_entries(make_entries(count=2))
        assert overview.card_count() == 2
