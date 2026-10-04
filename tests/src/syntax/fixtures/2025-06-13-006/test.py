from abc import abstractmethod
import math

from PySide6 import QtGui, QtWidgets, QtCore

class SunburstOrgWidget(QtWidgets.QWidget):
    """
    A widget base class used to draw sunburst org charts.
    """
    person_clicked = QtCore.Signal(int)

    def __init__(self) -> None:
        super().__init__()
        self._locations = {}
        self._people = {}
        self._top_level_uen = 0
        self._uen = 0
        self._ring_width = 0
        self._max_width = 0
        self._max_height = 0
        self._spacing = 20
        self._zoom_factor = 1.0
        self._unknown_colour = [0x40, 0x40, 0x40]
        self._highlighted_person = 0
        self.setMouseTracking(True)

    def _recurse_find_person(self, target_depth, target_angle, supervisor_uen,
                             depth, start_angle, start_arc):
        # Work out which person corresponds to a specific depth in the org
        # and an angle from the centre of the sunburst.

        # If we've hit our target depth then we've found our person.
        if target_depth == depth:
            return supervisor_uen

        # We need to traverse deeper, so work out which one of the current
        # supervisor's direct reports covers the target angle we have.
        supervisor_person = self._people[supervisor_uen]

        angle = start_angle
        for i in supervisor_person.get_direct_reports(self._people):
            p = self._people[i]
            sf = p.get_supervisor_fraction(self._people)
            arc = sf * start_arc
            if (target_angle >= angle) and (target_angle < (angle + arc)):
                return self._recurse_find_person(
                    target_depth, target_angle, i, depth + 1, angle, arc
                )

            angle += arc

        return 0

    def _find_person_in_widget(self, pos):
        # Given a pixel position, work out which person is shown at that location.
        x = pos.x() - self._spacing
        if x < 0:
            return

        y = pos.y() - self._spacing
        if y < 0:
            return

        # Work out a position relative to the centre of the sunburst
        w = x - self._max_width
        h = self._max_height - y

        # Work out the depth of the org to which this click corresponds.
        mag = math.sqrt((h * h) + (w * w))
        depth = int(mag / self._ring_width)

        # Work out the angle of the click, relative to the centre of the
        # sunburst.
        #
        # We have to jump through a few hoops to get the angle associated with
        # the mouse position.
        if h == 0:
            if w >= 0:
                angle = 90
            else:
                angle = -90
        else:
            angle = math.atan(w / h) * 180 / math.pi

        if w >= 0:
            if h < 0:
                angle = 180 + angle
        else:
            if h >= 0:
                angle = 360 + angle
            else:
                angle = 180 + angle

        # Once we know where we're looking, go and find the person who was
        # clicked.  If we don't find one then do nothing.
        return self._recurse_find_person(depth, angle, self._uen, 0, 0, 360)

    def _get_tool_tip(self, uen):
        return self._people[uen].get_name()

    def _handle_tool_tip_event(self, event):
        pos = event.pos()
        person = self._find_person_in_widget(pos)
        if person == 0:
            QtWidgets.QToolTip.hideText()
        else:
            QtWidgets.QToolTip.showText(event.globalPos(), self._get_tool_tip(person), self)

        return True

    def event(self, e):
        if e.type() == QtCore.QEvent.Type.ToolTip:
            return self._handle_tool_tip_event(e)

        return super().event(e)

    def mousePressEvent(self, event: QtGui.QMouseEvent) -> None:
        # If this is anything but a left click then ignore it and let our
        # parent window deal with it.
        if event.button() != QtCore.Qt.LeftButton:
            event.ignore()
            return

        event.accept()

        pos = event.position()
        person = self._find_person_in_widget(pos)
        if person == 0:
            return

        # Generate a signal indidating that a specific person was clicked.
        self.person_clicked.emit(person)

    def mouseMoveEvent(self, event: QtGui.QMouseEvent) -> None:
        pos = event.pos()
        person = self._find_person_in_widget(pos)
        if person != self._highlighted_person:
            self._highlighted_person = person
            self.update()

        return super().mouseMoveEvent(event)

    def leaveEvent(self, event: QtCore.QEvent) -> None:
        if self._highlighted_person != 0:
            self._highlighted_person = 0
            self.update()

        return super().leaveEvent(event)

    def _scan_depth(self, supervisor):
        org_depth = self._people[supervisor].get_org_depth(self._people)
        for i in self._people[supervisor].get_direct_reports(self._people):
            d = self._scan_depth(i)
            if d > org_depth:
                org_depth = d

        return org_depth

    @abstractmethod
    def _get_brush_colour(self, uen):
        pass

    def _setup_brush(self, painter, uen):
        colour = self._get_brush_colour(uen)
        if uen == self._highlighted_person:
            if self._unknown_colour[0] + self._unknown_colour[1] >= 0xc0:
                colour = [0xff, 0xff, 0xff]
            else:
                colour = [0x30, 0x30, 0x30]

        brush = QtGui.QBrush(QtGui.QColor(colour[0], colour[1], colour[2], 0xff))
        painter.setBrush(brush)

    def _recurse_draw_widget(self, painter, supervisor_uen, depth, start_angle, start_arc):
        supervisor_person = self._people[supervisor_uen]

        angle = start_angle
        for i in supervisor_person.get_direct_reports(self._people):
            radius = (depth + 1) * self._ring_width
            p = self._people[i]
            sf = p.get_supervisor_fraction(self._people)
            arc = sf * start_arc
            self._recurse_draw_widget(painter, i, depth + 1, angle, arc)
            self._setup_brush(painter, i)

            # If our arc is less than a full circle then we're drawing a pie
            # segment, but if it's a full circle then draw it as an ellipse
            # so we don't end up drawing a chord.
            if arc < 359.9999:
                painter.drawPie(self._spacing + self._max_width - radius,
                                self._spacing + self._max_height - radius,
                                radius * 2, radius * 2,
                                (90 + angle) * 16, -arc * 16)
            else:
                painter.drawEllipse(self._spacing + self._max_width - radius,
                                    self._spacing + self._max_height - radius,
                                    radius * 2, radius * 2)
            angle -= arc

    def _draw_widget(self, painter):
        if len(self._people) == 0:
            return

        # Recursively draw the outer layers of the org chart.
        self._recurse_draw_widget(painter, self._uen, 1, 0, 360)

        # Then draw the inner-most node.
        self._setup_brush(painter, self._uen)
        painter.drawEllipse(
            self._spacing + self._max_width - self._ring_width,
            self._spacing + self._max_height - self._ring_width,
            self._ring_width * 2, self._ring_width * 2
        )

    def paintEvent(self, _):
        qp = QtGui.QPainter()
        qp.begin(self)
        self._draw_widget(qp)
        qp.end()

    def set_locations(self, locations):
        self._locations = locations

    def set_people(self, people, top_level_uen):
        self._people = people
        self._top_level_uen = top_level_uen

    def _set_sizing(self):
        uen = self._uen
        uen_org_depth = self._people[uen].get_org_depth(self._people)

        # Work out how many layers deep the org goes.
        max_org_depth = self._scan_depth(self._top_level_uen)
        view_depth = self._scan_depth(uen)
        self._ring_width = int(60 * self._zoom_factor)
        self._max_width = self._ring_width * (max_org_depth + 1)
        self._max_height = self._ring_width * (view_depth - uen_org_depth + 1)

        self.setMinimumSize(
            2 * (self._spacing + self._max_width) + 1, 2 * (self._spacing + self._max_height) + 1
        )
        self.update()

    def set_uen(self, uen):
        self._uen = uen
        self._set_sizing()

    def set_zoom(self, zoom_factor):
        self._zoom_factor = zoom_factor
        self._set_sizing()

    def set_unknown_colour(self, colour):
        self._unknown_colour = colour
