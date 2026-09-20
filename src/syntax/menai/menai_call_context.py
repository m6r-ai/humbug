from dataclasses import dataclass, field
from enum import Enum, auto

from syntax.lexer import Token, TokenType


class FrameKind(Enum):
    """
    The kind of list a parenthesis frame represents.

    Each kind determines whether an identifier appearing at a given element
    position is a function being called (operator position) or a name in a
    binding, parameter, field, export, pattern, or namespace context.
    """
    APPLICATION = auto()
    BINDING_LIST = auto()
    BINDING_PAIR = auto()
    PARAM_LIST = auto()
    FIELD_LIST = auto()
    EXPORT_LIST = auto()
    MATCH_ARM = auto()
    PATTERN = auto()
    NAMESPACE_ACCESS = auto()
    IMPORT = auto()


_BINDING_KEYWORDS = frozenset({'let', 'let*', 'letrec'})


@dataclass
class Frame:
    """
    A single open-parenthesis frame on the call-context stack.

    Attributes:
        kind: The kind of list this frame represents
        head_keyword: The special-form keyword heading the frame, if any
        element_index: How many elements have been seen in this frame
        quoted: Whether this frame's contents are quoted data
    """
    kind: FrameKind
    head_keyword: str | None = None
    element_index: int = 0
    quoted: bool = False


@dataclass
class CallContextState:
    """
    Persistent state for the call-context tracker across lines.

    Attributes:
        frames: The stack of open parenthesis frames
        quote_pending: Whether a quote token is awaiting its quoted expression
    """
    frames: list[Frame] = field(default_factory=list)
    quote_pending: bool = False

    def clone(self) -> 'CallContextState':
        """
        Return an independent copy of this state.

        The frame stack is copied so that the returned state shares no mutable
        objects with this one.  This is essential when a state is persisted
        between lines: the parser mutates its working state in place, so it
        must never do so on a state object that is still referenced elsewhere.

        Returns:
            A deep copy of this state
        """
        return CallContextState(
            frames=[Frame(
                kind=frame.kind,
                head_keyword=frame.head_keyword,
                element_index=frame.element_index,
                quoted=frame.quoted
            ) for frame in self.frames],
            quote_pending=self.quote_pending
        )


class MenaiCallContext:
    """
    Classifies identifiers in Menai token streams as function calls.

    Menai is a Lisp-like language in which the head of an ordinary form is the
    function being applied.  The head of a special form, and names in binding,
    parameter, field, export, pattern, and namespace positions, are not calls.
    This tracker maintains a stack of parenthesis frames so that an identifier
    in operator position can be retyped to FUNCTION_OR_METHOD, letting function
    calls stand out from plain identifiers.
    """

    def __init__(self) -> None:
        self._state = CallContextState()

    def save_state(self) -> CallContextState:
        """
        Return the current state so it can be persisted between lines.

        Returns:
            The current call-context state
        """
        return self._state.clone()

    def restore_state(self, state: CallContextState) -> None:
        """
        Restore previously saved state.

        Args:
            state: The state to restore
        """
        self._state = state.clone()

    def process_token(self, token: Token) -> None:
        """
        Update the context for a single token, retyping it if needed.

        Args:
            token: The token to process.  An IDENTIFIER in operator position is
                retyped in place to FUNCTION_OR_METHOD.
        """
        if token.type == TokenType.QUOTE:
            self._state.quote_pending = True
            return

        if token.type == TokenType.LPAREN:
            self._push_frame()
            return

        if token.type == TokenType.RPAREN:
            self._pop_frame()
            return

        self._process_element(token)

    def _push_frame(self) -> None:
        """Push a new frame for an opening parenthesis."""
        parent = self._state.frames[-1] if self._state.frames else None
        kind = self._frame_kind_for(parent)
        quoted = self._state.quote_pending or self._in_quoted_context(parent)
        self._state.frames.append(Frame(kind=kind, quoted=quoted))
        self._state.quote_pending = False

        # A nested list counts as a single element of its parent frame, so the
        # parent's element index advances when the list opens.
        if parent is not None:
            parent.element_index += 1

    def _pop_frame(self) -> None:
        """Pop the innermost frame for a closing parenthesis."""
        if self._state.frames:
            self._state.frames.pop()

        self._state.quote_pending = False

    def _process_element(self, token: Token) -> None:
        """
        Process a non-delimiter token as an element of the current frame.

        Args:
            token: The token to process
        """
        frame = self._state.frames[-1] if self._state.frames else None

        if frame is not None:
            if self._is_operator_position(frame, token):
                token.type = TokenType.FUNCTION_OR_METHOD

            if frame.element_index == 0 and token.type == TokenType.KEYWORD:
                frame.head_keyword = token.value.lower()

            frame.element_index += 1

        self._state.quote_pending = False

    def _is_operator_position(self, frame: Frame, token: Token) -> bool:
        """
        Determine whether a token sits in operator position.

        A token is in operator position when it is the head of an application
        frame, is not quoted, and is an identifier.

        Args:
            frame: The frame the token belongs to
            token: The token to check

        Returns:
            True if the token should be classified as a function call
        """
        if token.type != TokenType.IDENTIFIER:
            return False

        if frame.quoted:
            return False

        return frame.kind == FrameKind.APPLICATION and frame.element_index == 0

    def _in_quoted_context(self, parent: Frame | None) -> bool:
        """
        Determine whether the current position is inside quoted data.

        Args:
            parent: The enclosing frame, if any

        Returns:
            True if the position is quoted, either because the enclosing frame
            is quoted or because it is the quoted child of a quote form
        """
        if parent is None:
            return False

        if parent.quoted:
            return True

        return parent.head_keyword == 'quote' and parent.element_index == 1

    def _frame_kind_for(self, parent: Frame | None) -> FrameKind:
        """
        Determine the kind of frame an opening parenthesis introduces.

        Args:
            parent: The enclosing frame, if any

        Returns:
            The kind of the new frame
        """
        if parent is None:
            return FrameKind.APPLICATION

        if parent.kind == FrameKind.BINDING_LIST:
            return FrameKind.BINDING_PAIR

        if parent.kind == FrameKind.MATCH_ARM:
            return FrameKind.PATTERN if parent.element_index == 0 else FrameKind.APPLICATION

        if parent.kind == FrameKind.PATTERN:
            return FrameKind.PATTERN

        if parent.kind != FrameKind.APPLICATION:
            return FrameKind.APPLICATION

        keyword = parent.head_keyword

        if keyword in _BINDING_KEYWORDS and parent.element_index == 1:
            return FrameKind.BINDING_LIST

        if keyword == 'lambda' and parent.element_index == 1:
            return FrameKind.PARAM_LIST

        if keyword == 'struct' and parent.element_index == 1:
            return FrameKind.FIELD_LIST

        if keyword == 'export' and parent.element_index >= 1:
            return FrameKind.EXPORT_LIST

        if keyword == 'match' and parent.element_index >= 2:
            return FrameKind.MATCH_ARM

        if keyword == '::' and parent.element_index >= 1:
            return FrameKind.NAMESPACE_ACCESS

        if keyword == 'import' and parent.element_index >= 1:
            return FrameKind.IMPORT

        return FrameKind.APPLICATION
