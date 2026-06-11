"""Tests for the tab carousel overlay (scrollable centred card strip)."""
# pylint: disable=protected-access, missing-class-docstring, missing-function-docstring

from PySide6.QtCore import QEvent, QPoint, Qt
from PySide6.QtGui import QKeyEvent

from desktop.tab_manager.tab_carousel import TabCarouselWidget


def make_carousel(host, entries):
    carousel = TabCarouselWidget(host)
    carousel.setGeometry(host.rect())
    carousel.set_entries(entries)
    carousel.show()
    return carousel


def press_key(qapp, widget, key):
    qapp.sendEvent(widget, QKeyEvent(QEvent.Type.KeyPress, key, Qt.KeyboardModifier.NoModifier))


def centre_of(carousel) -> QPoint:
    return QPoint(carousel.width() // 2, carousel.height() // 2)


class TestNavigation:
    def test_opens_centred_on_current(self, host, make_entries):
        carousel = make_carousel(host, make_entries(current=2))
        assert carousel.current_tab_id() == "t2"

    def test_select_next_wraps(self, host, make_entries):
        carousel = make_carousel(host, make_entries(count=3, current=2))
        carousel.select_next()
        assert carousel.current_tab_id() == "t0"

    def test_arrow_keys_navigate(self, qapp, host, make_entries, settle):
        carousel = make_carousel(host, make_entries(current=2))
        press_key(qapp, carousel, Qt.Key.Key_Right)
        settle()
        assert carousel.current_tab_id() == "t3"

        press_key(qapp, carousel, Qt.Key.Key_Left)
        settle()
        assert carousel.current_tab_id() == "t2"

    def test_enter_activates_centred_card(self, qapp, host, make_entries):
        carousel = make_carousel(host, make_entries(current=1))
        activated = []
        carousel.tab_activated.connect(activated.append)
        press_key(qapp, carousel, Qt.Key.Key_Return)
        assert activated == ["t1"]

    def test_escape_dismisses(self, qapp, host, make_entries):
        carousel = make_carousel(host, make_entries())
        dismissed = []
        carousel.dismissed.connect(lambda: dismissed.append(True))
        press_key(qapp, carousel, Qt.Key.Key_Escape)
        assert dismissed

    def test_remove_entry_adjusts_position(self, host, make_entries, settle):
        carousel = make_carousel(host, make_entries(count=3, current=2))
        carousel.remove_entry("t2")
        settle()
        assert carousel.entry_count() == 2
        assert carousel.current_tab_id() == "t1"


class TestGestures:
    def test_drag_pages_without_activating(self, host, make_entries, mouse, settle):
        carousel = make_carousel(host, make_entries(current=2))
        activated = []
        carousel.tab_activated.connect(activated.append)

        start = centre_of(carousel)
        mouse(carousel, QEvent.Type.MouseButtonPress, start)
        mouse(carousel, QEvent.Type.MouseMove, start + QPoint(-150, 0))
        mouse(carousel, QEvent.Type.MouseButtonRelease, start + QPoint(-150, 0))
        settle()
        assert not activated
        assert carousel.current_tab_id() == "t3"

    def test_clean_click_activates(self, host, make_entries, mouse):
        carousel = make_carousel(host, make_entries(current=2))
        activated = []
        carousel.tab_activated.connect(activated.append)

        centre = centre_of(carousel)
        mouse(carousel, QEvent.Type.MouseButtonPress, centre)
        mouse(carousel, QEvent.Type.MouseButtonRelease, centre)
        assert activated == ["t2"]

    def test_click_outside_cards_dismisses(self, host, make_entries, mouse):
        carousel = make_carousel(host, make_entries(count=1, current=0))
        dismissed = []
        carousel.dismissed.connect(lambda: dismissed.append(True))

        corner = QPoint(5, carousel.height() - 5)
        mouse(carousel, QEvent.Type.MouseButtonPress, corner)
        mouse(carousel, QEvent.Type.MouseButtonRelease, corner)
        assert dismissed

    def test_swipe_up_closes_centred_card(self, host, make_entries, mouse, settle):
        carousel = make_carousel(host, make_entries(current=2))
        closed = []
        carousel.tab_close_requested.connect(closed.append)

        start = centre_of(carousel)
        mouse(carousel, QEvent.Type.MouseButtonPress, start)
        mouse(carousel, QEvent.Type.MouseMove, start + QPoint(0, -300))
        mouse(carousel, QEvent.Type.MouseButtonRelease, start + QPoint(0, -300))
        settle()
        assert closed == ["t2"]

    def test_small_swipe_springs_back(self, host, make_entries, mouse, settle):
        carousel = make_carousel(host, make_entries(current=2))
        closed = []
        carousel.tab_close_requested.connect(closed.append)

        start = centre_of(carousel)
        mouse(carousel, QEvent.Type.MouseButtonPress, start)
        mouse(carousel, QEvent.Type.MouseMove, start + QPoint(0, -40))
        mouse(carousel, QEvent.Type.MouseButtonRelease, start + QPoint(0, -40))
        settle()
        assert not closed

    def test_horizontal_drag_with_vertical_wobble_scrolls(self, host, make_entries, mouse, settle):
        carousel = make_carousel(host, make_entries(current=2))
        closed = []
        carousel.tab_close_requested.connect(closed.append)

        start = centre_of(carousel)
        mouse(carousel, QEvent.Type.MouseButtonPress, start)
        mouse(carousel, QEvent.Type.MouseMove, start + QPoint(-150, -30))
        mouse(carousel, QEvent.Type.MouseButtonRelease, start + QPoint(-150, -30))
        settle()
        assert not closed
        assert carousel.current_tab_id() == "t3"

    def test_close_button_on_centred_card(self, host, make_entries, mouse):
        carousel = make_carousel(host, make_entries(current=2))
        closed = []
        carousel.tab_close_requested.connect(closed.append)

        card_rect = carousel._card_rect(0.0, 2)
        pos = carousel._close_rect(card_rect).center()
        mouse(carousel, QEvent.Type.MouseButtonPress, pos)
        mouse(carousel, QEvent.Type.MouseButtonRelease, pos)
        assert closed == ["t2"]


class TestGeometry:
    def test_card_adopts_thumbnail_aspect_ratio(self, host, make_entries):
        carousel = make_carousel(host, make_entries(width=1000, height=1000))
        rect = carousel._card_rect(0.0, 0)
        body_height = rect.height() - carousel._header_height()
        assert abs(rect.width() / body_height - 1.0) < 0.05

    def test_card_respects_maximum_width(self, host, make_entries):
        carousel = make_carousel(host, make_entries(width=4000, height=400))
        rect = carousel._card_rect(0.0, 0)
        assert rect.width() <= int(carousel.width() * 0.52) + 1
