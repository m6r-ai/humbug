import sys
import os
import asyncio
from qasync import QEventLoop, QApplication
from humbug.ai.openai import OpenAIBackend
from humbug.transcript.writer import TranscriptWriter
from humbug.gui.main_window import MainWindow

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
    app = QApplication(sys.argv)

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
