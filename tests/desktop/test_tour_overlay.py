"""Tests for the onboarding tour overlay (dimmed spotlight + coach-mark card)."""
# pylint: disable=protected-access, missing-class-docstring, missing-function-docstring

from PySide6.QtCore import QAbstractAnimation, QEvent, QPoint, QPointF, Qt
from PySide6.QtGui import QKeyEvent, QMouseEvent
from PySide6.QtWidgets import QPushButton

from desktop.tour.tour_overlay import TourOverlay


def make_overlay(host):
    overlay = TourOverlay(host)
    overlay.setGeometry(host.rect())
    return overlay


def show_step(overlay, can_go_back=False, spotlight_target=None):
    overlay.show_step(
        title="Title",
        description="Description",
        step_label="Step 1 of 7",
        back_label="Back",
        skip_label="Skip Tour",
        next_label="Next",
        can_go_back=can_go_back,
        spotlight_target=spotlight_target,
    )


def press_key(qapp, widget, key):
    qapp.sendEvent(widget, QKeyEvent(QEvent.Type.KeyPress, key, Qt.KeyboardModifier.NoModifier))


class TestContent:
    def test_show_step_sets_text(self, host):
        overlay = make_overlay(host)
        show_step(overlay)
        assert overlay._title_label.text() == "Title"
        assert overlay._description_label.text() == "Description"
        assert overlay._step_label.text() == "Step 1 of 7"

    def test_back_button_hidden_on_first_step(self, host):
        overlay = make_overlay(host)
        show_step(overlay, can_go_back=False)
        assert not overlay._back_button.isVisible()

    def test_back_button_shown_when_not_first_step(self, qapp, host):
        host.show()
        overlay = make_overlay(host)
        overlay.show()
        qapp.processEvents()
        show_step(overlay, can_go_back=True)
        assert overlay._back_button.isVisible()


class TestKeyboardNavigation:
    def test_escape_skips(self, qapp, host):
        overlay = make_overlay(host)
        show_step(overlay)
        skipped = []
        overlay.skip_requested.connect(lambda: skipped.append(True))
        press_key(qapp, overlay, Qt.Key.Key_Escape)
        assert skipped

    def test_right_arrow_advances(self, qapp, host):
        overlay = make_overlay(host)
        show_step(overlay)
        advanced = []
        overlay.next_requested.connect(lambda: advanced.append(True))
        press_key(qapp, overlay, Qt.Key.Key_Right)
        assert advanced

    def test_left_arrow_goes_back_when_available(self, qapp, host):
        host.show()
        overlay = make_overlay(host)
        overlay.show()
        qapp.processEvents()
        show_step(overlay, can_go_back=True)
        went_back = []
        overlay.back_requested.connect(lambda: went_back.append(True))
        press_key(qapp, overlay, Qt.Key.Key_Left)
        assert went_back

    def test_left_arrow_ignored_on_first_step(self, qapp, host):
        overlay = make_overlay(host)
        show_step(overlay, can_go_back=False)
        went_back = []
        overlay.back_requested.connect(lambda: went_back.append(True))
        press_key(qapp, overlay, Qt.Key.Key_Left)
        assert not went_back


class TestMouseHandling:
    def test_click_on_background_does_not_skip(self, qapp, host):
        # Skip Tour is the only way to dismiss the tour early; a stray click on
        # the dimmed background must not close it.
        overlay = make_overlay(host)
        show_step(overlay)
        skipped = []
        overlay.skip_requested.connect(lambda: skipped.append(True))
        pos = QPoint(5, 5)
        event = QMouseEvent(
            QEvent.Type.MouseButtonPress, QPointF(pos), QPointF(pos), QPointF(pos),
            Qt.MouseButton.LeftButton, Qt.MouseButton.LeftButton, Qt.KeyboardModifier.NoModifier,
        )
        qapp.sendEvent(overlay, event)
        assert not skipped


class TestSpotlightResolution:
    def test_none_target_gives_empty_rect(self, host):
        overlay = make_overlay(host)
        show_step(overlay, spotlight_target=None)
        assert overlay._spotlight_rect.isEmpty()

    def test_invisible_target_gives_empty_rect(self, host):
        overlay = make_overlay(host)
        target = QPushButton(host)
        target.hide()
        show_step(overlay, spotlight_target=target)
        assert overlay._spotlight_rect.isEmpty()

    def test_visible_target_gives_matching_rect(self, qapp, host):
        host.show()
        overlay = make_overlay(host)
        target = QPushButton(host)
        target.setGeometry(30, 40, 100, 20)
        target.show()
        qapp.processEvents()
        show_step(overlay, spotlight_target=target)
        assert overlay._spotlight_rect == target.geometry()

    def test_card_centred_when_no_spotlight(self, host):
        overlay = make_overlay(host)
        show_step(overlay, spotlight_target=None)
        card_rect = overlay._card.geometry()
        assert abs(card_rect.center().x() - host.rect().center().x()) <= 1
        assert abs(card_rect.center().y() - host.rect().center().y()) <= 1

    def test_card_positioned_below_spotlight_when_room_available(self, host):
        overlay = make_overlay(host)
        target = QPushButton(host)
        target.setGeometry(100, 50, 100, 20)
        target.show()
        show_step(overlay, spotlight_target=target)
        assert overlay._card.geometry().top() > target.geometry().bottom()


class TestStepTransitionAnimation:
    def test_content_updates_immediately_not_after_a_delay(self, qapp, host):
        overlay = make_overlay(host)
        show_step(overlay)
        overlay.show_step(
            title="Title 2", description="Description 2", step_label="Step 2 of 7",
            back_label="Back", skip_label="Skip Tour", next_label="Next",
            can_go_back=True, spotlight_target=None,
        )
        assert overlay._title_label.text() == "Title 2"
        assert overlay._description_label.text() == "Description 2"
        assert overlay._step_label.text() == "Step 2 of 7"

    def test_card_stays_fully_opaque_across_a_step_transition(self, qapp, host, settle):
        overlay = make_overlay(host)
        show_step(overlay)
        settle(500)
        overlay.show_step(
            title="Title 2", description="Description 2", step_label="Step 2 of 7",
            back_label="Back", skip_label="Skip Tour", next_label="Next",
            can_go_back=True, spotlight_target=None,
        )
        assert overlay._opacity_effect.opacity() == 1.0

    def test_spotlight_glides_between_two_visible_targets(self, qapp, host, settle):
        host.show()
        overlay = make_overlay(host)
        overlay.show()
        target_a = QPushButton(host)
        target_a.setGeometry(30, 40, 100, 20)
        target_a.show()
        target_b = QPushButton(host)
        target_b.setGeometry(300, 200, 100, 20)
        target_b.show()
        qapp.processEvents()

        show_step(overlay, spotlight_target=target_a)
        show_step(overlay, spotlight_target=target_b)
        assert overlay._spotlight_animation is not None

        settle(800)
        assert overlay._spotlight_rect == target_b.geometry()

    def test_transition_from_no_spotlight_animates_card_and_ring(self, qapp, host, settle):
        host.show()
        overlay = make_overlay(host)
        overlay.show()
        target = QPushButton(host)
        target.setGeometry(30, 40, 100, 20)
        target.show()
        qapp.processEvents()

        show_step(overlay, spotlight_target=None)
        centred_pos = overlay._card.pos()

        show_step(overlay, spotlight_target=target)
        # The ring appears at its final position immediately (nothing to glide it from)...
        assert overlay._spotlight_animation is None
        assert overlay._spotlight_rect == target.geometry()
        # ...but fades in, and the card animates away from its old (centred) position, rather than jumping.
        assert overlay._spotlight_alpha_animation is not None
        assert overlay._card_position_animation is not None
        assert overlay._card.pos() == centred_pos

        settle(800)
        assert overlay._spotlight_alpha == 1.0
        assert overlay._card.pos() != centred_pos

    def test_transition_to_no_spotlight_animates_card_and_ring(self, qapp, host, settle):
        host.show()
        overlay = make_overlay(host)
        overlay.show()
        target = QPushButton(host)
        target.setGeometry(30, 40, 100, 20)
        target.show()
        qapp.processEvents()

        show_step(overlay, spotlight_target=target)
        spotlighted_pos = overlay._card.pos()

        show_step(overlay, spotlight_target=None)
        # The ring stays put (nothing to glide it towards) and fades out in place...
        assert overlay._spotlight_rect == target.geometry()
        assert overlay._spotlight_alpha_animation is not None
        # ...while the card animates towards the centre rather than jumping there.
        assert overlay._card_position_animation is not None
        assert overlay._card.pos() == spotlighted_pos

        settle(800)
        assert overlay._spotlight_alpha == 0.0
        assert overlay._spotlight_rect.isEmpty()
        assert overlay._card.pos() != spotlighted_pos


class TestOverlayFadeAndPulse:
    def test_overlay_starts_transparent_and_fades_in(self, qapp, host, settle):
        overlay = make_overlay(host)
        assert overlay._dim_alpha == 0.0
        show_step(overlay)
        settle(500)
        assert overlay._dim_alpha == 1.0

    def test_fade_out_calls_back_once_finished(self, qapp, host, settle):
        overlay = make_overlay(host)
        show_step(overlay)
        settle(500)

        finished = []
        overlay.fade_out(lambda: finished.append(True))
        assert not finished
        settle(500)
        assert finished
        assert overlay._dim_alpha == 0.0

    def test_pulse_runs_while_spotlight_is_shown(self, qapp, host):
        host.show()
        overlay = make_overlay(host)
        overlay.show()
        target = QPushButton(host)
        target.setGeometry(30, 40, 100, 20)
        target.show()
        qapp.processEvents()

        show_step(overlay, spotlight_target=target)
        assert overlay._pulse_animation.state() == QAbstractAnimation.State.Running

    def test_pulse_paused_when_there_is_no_spotlight(self, qapp, host):
        overlay = make_overlay(host)
        show_step(overlay, spotlight_target=None)
        assert overlay._pulse_animation.state() != QAbstractAnimation.State.Running
