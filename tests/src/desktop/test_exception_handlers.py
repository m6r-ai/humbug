"""Tests that an exception nothing handled reaches the canary, wherever it happened."""
# pylint: disable=protected-access

import asyncio
import sys
import threading

import pytest

from desktop.__main__ import install_asyncio_exception_handler, install_global_exception_handler
from desktop.exception_notifier import ExceptionNotifier, get_exception_notifier


@pytest.fixture
def notified(qapp):
    """Record every canary notification, restoring the global hooks afterwards."""
    ExceptionNotifier._instance = None
    notifier = get_exception_notifier()
    seen: list[bool] = []
    notifier.exception_occurred.connect(lambda: seen.append(True))

    original_excepthook = sys.excepthook
    original_thread_hook = threading.excepthook

    yield seen

    sys.excepthook = original_excepthook
    threading.excepthook = original_thread_hook
    ExceptionNotifier._instance = None


class TestMainThreadExceptions:
    """Tests for exceptions on the main thread."""

    def test_an_uncaught_exception_lights_the_canary(self, notified):
        """The plainest case: something raised and nothing caught it."""
        install_global_exception_handler()

        try:
            raise RuntimeError("boom")

        except RuntimeError:
            sys.excepthook(*sys.exc_info())

        assert notified == [True]

    def test_a_keyboard_interrupt_does_not(self, notified):
        """Quitting with Ctrl+C is not a fault to report."""
        install_global_exception_handler()

        try:
            raise KeyboardInterrupt()

        except KeyboardInterrupt:
            sys.excepthook(*sys.exc_info())

        assert notified == []


class TestWorkerThreadExceptions:
    """Tests for exceptions that terminate a worker thread."""

    def test_a_thread_exception_lights_the_canary(self, notified, qapp):
        """A thread dying silently would otherwise leave the UI looking healthy."""
        install_global_exception_handler()

        def boom() -> None:
            raise RuntimeError("thread boom")

        thread = threading.Thread(target=boom)
        thread.start()
        thread.join()

        # The notifier lives on the main thread, so the signal is delivered when
        # the event loop next runs rather than inside the worker thread.
        qapp.processEvents()

        assert notified == [True]


class TestAsyncioExceptions:
    """Tests for failures in asyncio tasks, where most of Humbug's work happens."""

    def test_a_task_whose_exception_is_never_retrieved_lights_the_canary(self, notified, qapp):
        """An AI turn failing in the background must not pass unnoticed."""
        loop = asyncio.new_event_loop()
        try:
            install_asyncio_exception_handler(loop)

            async def boom() -> None:
                raise RuntimeError("task boom")

            async def main() -> None:
                loop.create_task(boom())
                await asyncio.sleep(0.05)

            loop.run_until_complete(main())
            # The handler fires when the task is garbage collected
            loop.run_until_complete(asyncio.sleep(0))

        finally:
            loop.close()

        qapp.processEvents()
        assert notified == [True]

    def test_a_cancelled_task_does_not(self, notified, qapp):
        """Cancellation is ordinary shutdown behaviour, not a fault."""
        loop = asyncio.new_event_loop()
        try:
            install_asyncio_exception_handler(loop)
            handler = loop.get_exception_handler()
            assert handler is not None
            handler(loop, {"message": "cancelled", "exception": asyncio.CancelledError()})

        finally:
            loop.close()

        qapp.processEvents()
        assert notified == []

    def test_an_error_with_no_exception_object_still_lights_the_canary(self, notified, qapp):
        """asyncio reports some failures as a message alone."""
        loop = asyncio.new_event_loop()
        try:
            install_asyncio_exception_handler(loop)
            handler = loop.get_exception_handler()
            assert handler is not None
            handler(loop, {"message": "Executor shutdown has been called"})

        finally:
            loop.close()

        qapp.processEvents()
        assert notified == [True]
