"""Transcript writer implementation for Humbug application."""

import asyncio
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
import json
import os
import shutil
import sys
from typing import List, Dict, Optional


class TranscriptWriter:
    """Handles writing conversation transcripts to files with rotation."""

    def __init__(self, max_size_mb: int = 50, max_files: int = 5):
        """Initialize transcript writer with configurable limits."""
        self.filename = self._get_filename()
        self.file_size = 0
        self.max_size = max_size_mb * 1024 * 1024  # Convert MB to bytes
        self.max_files = max_files
        self.backup_suffix = ".backup"
        self._executor = ThreadPoolExecutor(max_workers=1)
        self._initialize_file()

    def _get_filename(self, suffix: Optional[str] = None) -> str:
        """Generate a transcript filename based on current time."""
        timestamp = datetime.utcnow().strftime("%Y-%m-%d-%H-%M-%S")
        base_name = f"transcript-{timestamp}"
        if suffix:
            base_name = f"{base_name}-{suffix}"
        return f"{base_name}.json"

    def _initialize_file(self):
        """Initialize a new transcript file."""
        if not os.path.exists(self.filename):
            metadata = {
                "metadata": {
                    "timestamp": datetime.utcnow().isoformat(),
                    "version": "0.1"
                },
                "conversation": []
            }
            self._write_json_sync(metadata)

    def _write_json_sync(self, data: Dict):
        """Synchronous JSON write for initialization only."""
        with open(self.filename, 'w') as f:
            json.dump(data, f, indent=2)
        self.file_size = os.path.getsize(self.filename)

    async def _write_json(self, data: Dict):
        """Write JSON data to file with atomic operation and backup."""
        temp_file = f"{self.filename}.tmp"
        try:
            # Use ThreadPoolExecutor for file operations
            await asyncio.get_event_loop().run_in_executor(
                self._executor,
                self._write_json_sync_atomic,
                temp_file,
                data
            )
            # Atomic rename
            os.replace(temp_file, self.filename)
            self.file_size = os.path.getsize(self.filename)

        except Exception as e:
            # Create backup of existing file if possible
            backup_file = self._create_backup()
            error_msg = f"Error writing transcript: {str(e)}"
            if backup_file:
                error_msg += f"\nBackup created at: {backup_file}"
            print(error_msg, file=sys.stderr)

            # Clean up temp file
            if os.path.exists(temp_file):
                try:
                    os.unlink(temp_file)
                except Exception:
                    pass

            # Create new file
            self.filename = self._get_filename(suffix="recovery")
            self.file_size = 0
            self._initialize_file()
            raise

    def _write_json_sync_atomic(self, filename: str, data: Dict):
        """Atomic write operation for JSON data."""
        with open(filename, 'w') as f:
            json.dump(data, f, indent=2)

    async def _read_json(self, filename: str) -> Dict:
        """Read JSON data asynchronously."""
        def _read():
            with open(filename, 'r') as f:
                return json.load(f)
        return await asyncio.get_event_loop().run_in_executor(self._executor, _read)

    async def _rotate_file(self):
        """Rotate transcript file if size limit reached."""
        if self.file_size >= self.max_size:
            try:
                # List all transcript files
                transcript_dir = os.path.dirname(self.filename) or '.'
                files = [f for f in os.listdir(transcript_dir)
                        if f.startswith('transcript-') and
                        f.endswith('.json') and
                        not f.endswith(f'{self.backup_suffix}.json')]
                files.sort(key=lambda f: os.path.getctime(os.path.join(transcript_dir, f)))

                # Remove oldest files if we have too many
                while len(files) >= self.max_files:
                    try:
                        oldest = files.pop(0)
                        os.unlink(os.path.join(transcript_dir, oldest))
                    except Exception as e:
                        print(f"Error removing old transcript {oldest}: {e}", file=sys.stderr)

                # Start new file
                self.filename = os.path.join(transcript_dir, self._get_filename())
                self.file_size = 0
                self._initialize_file()
            except Exception as e:
                print(f"Error during transcript rotation: {e}", file=sys.stderr)

    def _create_backup(self) -> str:
        """Create a backup of the current transcript file."""
        backup_name = f"{os.path.splitext(self.filename)[0]}{self.backup_suffix}.json"
        try:
            if os.path.exists(self.filename):
                shutil.copy2(self.filename, backup_name)
            return backup_name
        except Exception as e:
            print(f"Error creating backup: {e}", file=sys.stderr)
            return ""

    async def write(self, messages: List[Dict]):
        """Write messages to the transcript file with retries."""
        for attempt in range(3):  # Max 3 retries
            try:
                try:
                    data = await self._read_json(self.filename)
                except FileNotFoundError:
                    self._initialize_file()
                    data = await self._read_json(self.filename)

                # Process messages
                for message in messages:
                    data["conversation"].append(message)

                await self._write_json(data)
                await self._rotate_file()
                break  # Success - exit retry loop

            except Exception as e:
                delay = 2 * (2 ** attempt)  # Exponential backoff: 2, 4, 8 seconds
                if attempt < 2:  # Less than max retries
                    error_msg = f"Failed to write transcript (attempt {attempt + 1}/3): {str(e)}"
                    print(error_msg, file=sys.stderr)
                    await asyncio.sleep(delay)
                    continue

                # Final attempt failed - create backup and new file
                error_msg = f"Failed to write transcript after 3 attempts: {str(e)}"
                print(error_msg, file=sys.stderr)

                backup_file = self._create_backup()
                if backup_file:
                    print(f"Created backup at: {backup_file}", file=sys.stderr)

                self.filename = self._get_filename(suffix="recovery")
                self.file_size = 0
                self._initialize_file()

                # One final try with the new file
                try:
                    data = await self._read_json(self.filename)
                    for message in messages:
                        data["conversation"].append(message)
                    await self._write_json(data)
                except Exception as e2:
                    print(f"Failed to recover transcript: {str(e2)}", file=sys.stderr)
