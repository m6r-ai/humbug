"""Tests for Menai VM cancellation."""

import threading
import time

import pytest

from menai import Menai
from menai.menai_error import MenaiCancelledException
from menai.menai_vm import _C_VM_AVAILABLE


# A Menai expression that loops for a very long time (infinite recursion with
# tail-call optimization so it never overflows the stack).
# The VM checks for cancellation every ~131K instructions, so this will be
# cancelled quickly once cancel() is called.
_LONG_RUNNING = """
(letrec ((loop (lambda (n) (loop (integer+ n 1)))))
  (loop 0))
"""


@pytest.fixture
def menai():
    """Create a fresh Menai instance for each test."""
    return Menai()


@pytest.mark.skipif(not _C_VM_AVAILABLE, reason="C VM not available")
class TestCVMCancellation:
    """Tests for the C VM's thread-safe cancellation mechanism."""

    def test_cancel_stops_long_running_expression(self, menai):
        """cancel() from another thread stops a long-running C VM execution."""
        result_holder: dict[str, object] = {}

        def run_expression():
            try:
                menai.evaluate(_LONG_RUNNING)
                result_holder["result"] = "completed"
            except MenaiCancelledException:
                result_holder["result"] = "cancelled"
            except Exception as e:  # pylint: disable=broad-except
                result_holder["result"] = f"error: {e}"

        thread = threading.Thread(target=run_expression, daemon=True)
        thread.start()

        # Give the VM time to start running.
        time.sleep(0.1)

        # Request cancellation from this (main) thread.
        menai.vm.cancel()

        # Wait for the thread to finish (should be quick after cancel).
        thread.join(timeout=5.0)

        assert not thread.is_alive(), "Thread did not finish after cancellation"
        assert result_holder.get("result") == "cancelled", (
            f"Expected cancellation, got: {result_holder.get('result')}"
        )

    def test_execute_works_after_cancellation(self, menai):
        """A stale cancellation flag does not block subsequent execute() calls."""
        result_holder: dict[str, object] = {}

        def run_expression():
            try:
                menai.evaluate(_LONG_RUNNING)
                result_holder["result"] = "completed"
            except MenaiCancelledException:
                result_holder["result"] = "cancelled"
            except Exception as e:  # pylint: disable=broad-except
                result_holder["result"] = f"error: {e}"

        # First: run and cancel.
        thread = threading.Thread(target=run_expression, daemon=True)
        thread.start()
        time.sleep(0.1)
        menai.vm.cancel()
        thread.join(timeout=5.0)
        assert result_holder.get("result") == "cancelled"

        # Second: a normal expression should work fine (stale flag is cleared).
        result = menai.evaluate("(integer+ 1 2)")
        assert result == 3

    def test_cancel_without_running_execute_is_harmless(self, menai):
        """Calling cancel() when no execution is running has no ill effects."""
        menai.vm.cancel()

        # The next execute() should clear the flag and run normally.
        result = menai.evaluate("(integer* 6 7)")
        assert result == 42

    def test_normal_expression_not_cancelled(self, menai):
        """A fast expression completes normally without being cancelled."""
        result = menai.evaluate("(integer+ 100 200)")
        assert result == 300
