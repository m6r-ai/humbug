import asyncio
import os
import sys
import time

from qasync import QEventLoop, QApplication

from humbug.ai.openai_backend import OpenAIBackend
from humbug.transcript.transcript_writer import TranscriptWriter
from humbug.gui.main_window import MainWindow


class HumbugApplication(QApplication):
    """Class for the application.  Specialized to do event time reporting."""
    def __init__(self, argv):
        super().__init__(argv)

        self._start_time = time.monotonic()

    def notify(self, receiver, event):
        event_type = event.type()
        receiver_name = receiver.objectName()
        start = time.monotonic()
        ret = QApplication.notify(self, receiver, event)
        end = time.monotonic()
        elapsed_time = (end - start) * 1000
        if elapsed_time > 2:
            rel_start = start - self._start_time
            print(f"{rel_start:.3f}: processing event type {event_type} for object {receiver_name} took {elapsed_time:.3f} msec")

        return ret


async def main():
    # Check for API key
    api_key = os.environ.get("OPENAI_API_KEY")
    if not api_key:
        print("Error: OPENAI_API_KEY environment variable not set")
        return 1

    # Initialize components
    ai_backend = OpenAIBackend(api_key)
    transcript = TranscriptWriter()

    # Create and show main window
    window = MainWindow(ai_backend, transcript)
    window.show()

    return 0

def run_app():
    # Create application first
    app = HumbugApplication(sys.argv)

    # Create and set event loop
    loop = QEventLoop(app)
    asyncio.set_event_loop(loop)

    # Run the main function
    try:
        with loop:
            exit_code = loop.run_until_complete(main())
            # Only run forever if main succeeded
            if exit_code == 0:
                loop.run_forever()
            return exit_code
    except KeyboardInterrupt:
        return 0

if __name__ == "__main__":
    sys.exit(run_app())
