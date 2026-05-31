import logging
from typing import Callable, Dict

from PySide6.QtWidgets import QWidget

from context.context_info import ContextInfo
from context.context_registry import ContextRegistry

from desktop.tab.tab_base import TabBase
from desktop.tab.tab_state import TabState

TabFactory = Callable[[TabState, QWidget], TabBase | None]
ContextFactory = Callable[[ContextInfo, ContextRegistry, QWidget], TabBase | None]


class TabFactoryRegistry:
    """Maps tool name strings to tab factory callables for session restore and live context opens."""

    def __init__(self) -> None:
        self._factories: Dict[str, TabFactory] = {}
        self._context_factories: Dict[str, ContextFactory] = {}
        self._logger = logging.getLogger("TabFactoryRegistry")

    def register(self, tool_name: str, factory: TabFactory) -> None:
        """Register a factory callable for the given tool name.

        Args:
            tool_name: The tool name string (e.g. 'editor', 'conversation').
            factory: Callable(state, parent) -> TabBase | None.  Should return
                None if the state cannot be restored (e.g. file missing).
        """
        self._factories[tool_name] = factory

    def register_context_factory(self, tool_name: str, factory: ContextFactory) -> None:
        """Register a context factory callable for the given tool name.

        Used by _on_context_opened to create and wire tabs from a live ContextInfo
        without the column manager needing any type-specific knowledge.

        Args:
            tool_name: The tool name string (e.g. 'editor', 'conversation').
            factory: Callable(info, registry, parent) -> TabBase | None.
        """
        self._context_factories[tool_name] = factory

    def can_restore(self, state: TabState) -> bool:
        """Return True if a tab for this state can be meaningfully restored.

        Args:
            state: The TabState to check.
        """
        factory = self._factories.get(state.type)
        if factory is None:
            return False

        # Delegate to a static can_restore on the factory's class if present.
        # By convention factories are bound classmethods so __self__ is the class.
        cls = getattr(factory, '__self__', None)
        if cls is not None:
            checker = getattr(cls, 'can_restore', None)
            if callable(checker):
                return checker(state.path)

        return True

    def restore(self, state: TabState, parent: QWidget) -> TabBase | None:
        """Create and restore a tab from state using the registered factory.

        Args:
            state: The TabState to restore from.
            parent: Parent widget for the new tab.

        Returns:
            The restored TabBase instance, or None if no factory is registered
            or the factory returns None.
        """
        factory = self._factories.get(state.type)
        if factory is None:
            self._logger.warning("No factory registered for tool '%s'", state.type)
            return None

        return factory(state, parent)

    def create_from_context(
        self, info: ContextInfo, registry: ContextRegistry, parent: QWidget
    ) -> TabBase | None:
        """Create and wire a tab from a live ContextInfo using the registered context factory.

        Args:
            info: The ContextInfo describing the context that was opened.
            registry: The active ContextRegistry, passed to the factory for model lookup.
            parent: Parent widget for the new tab.

        Returns:
            The created TabBase instance, or None if no factory is registered
            or the factory returns None.
        """
        factory = self._context_factories.get(info.context_type)
        if factory is None:
            self._logger.warning("No context factory registered for type '%s'", info.context_type)
            return None

        return factory(info, registry, parent)
