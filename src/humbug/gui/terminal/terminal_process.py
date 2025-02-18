import asyncio
import fcntl
import logging
import os
import pty
import signal
import sys
import termios
from typing import Optional, Tuple

class TerminalProcess:
    """Manages a terminal process with proper PTY setup."""

    def __init__(self):
        """Initialize terminal process manager."""
        self._logger = logging.getLogger("TerminalProcess")
        self._process_id: Optional[int] = None
        self._main_fd: Optional[int] = None
        self._running = True

    def _set_nonblocking(self, fd: int) -> None:
        """Set file descriptor to non-blocking mode.
        
        Args:
            fd: File descriptor to modify
            
        Raises:
            OSError: If fcntl call fails
        """
        flags = fcntl.fcntl(fd, fcntl.F_GETFL)
        fcntl.fcntl(fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)

    async def start(self, command: Optional[str] = None) -> Tuple[int, int]:
        """Start a new terminal process with proper PTY setup.
        
        Args:
            command: Optional command to run instead of shell
            
        Returns:
            Tuple of (process_id, master_fd)
            
        Raises:
            OSError: If process creation fails
        """
        main_fd, secondary_fd = pty.openpty()

        # Set raw mode on the PTY
        mode = termios.tcgetattr(secondary_fd)
        mode[3] &= ~(termios.ECHO | termios.ICANON)
        mode[3] |= termios.ISIG
        termios.tcsetattr(secondary_fd, termios.TCSAFLUSH, mode)

        # Set non-blocking mode on the master fd
        self._set_nonblocking(main_fd)

        # Fork the process
        pid = os.fork()

        if pid == 0:  # Child process
            try:
                # Close the master side of the PTY
                os.close(main_fd)

                # Create new session
                os.setsid()

                # Make the PTY our controlling terminal
                fcntl.ioctl(secondary_fd, termios.TIOCSCTTY, 0)

                # Duplicate the PTY to standard streams
                os.dup2(secondary_fd, 0)
                os.dup2(secondary_fd, 1)
                os.dup2(secondary_fd, 2)

                # Close the secondary fd if it's not one of the standard streams
                if secondary_fd > 2:
                    os.close(secondary_fd)

                # Reset signal handlers
                signal.signal(signal.SIGINT, signal.SIG_DFL)
                signal.signal(signal.SIGQUIT, signal.SIG_DFL)
                signal.signal(signal.SIGTSTP, signal.SIG_DFL)
                signal.signal(signal.SIGTTIN, signal.SIG_DFL)
                signal.signal(signal.SIGTTOU, signal.SIG_DFL)
                signal.signal(signal.SIGCHLD, signal.SIG_DFL)

                # Execute the shell or command
                shell = command if command else os.environ.get('SHELL', '/bin/sh')
                os.execvp(shell.split()[0], shell.split())

            except Exception as e:
                print(f"Child process failed: {e}", file=sys.stderr)
                os._exit(1)

        # Parent process
        os.close(secondary_fd)
        self._process_id = pid
        self._main_fd = main_fd

        return pid, main_fd

    def get_process_id(self) -> Optional[int]:
        """Get the process ID of the terminal process."""
        return self._process_id

    def get_main_fd(self) -> Optional[int]:
        """Get the master file descriptor."""
        return self._main_fd

    async def terminate(self) -> None:
        """Terminate the terminal process."""
        self._running = False

        if self._process_id:
            try:
                # Try graceful termination first
                os.killpg(os.getpgid(self._process_id), signal.SIGTERM)

                # Wait for process to terminate
                try:
                    await asyncio.get_event_loop().run_in_executor(
                        None,
                        os.waitpid,
                        self._process_id,
                        0
                    )
                except ChildProcessError:
                    pass  # Process already terminated

            except ProcessLookupError:
                pass  # Process already terminated

            self._process_id = None

        if self._main_fd is not None:
            try:
                os.close(self._main_fd)
            except OSError:
                pass
            self._main_fd = None

    def is_running(self) -> bool:
        """Check if the process is still running."""
        if not self._process_id:
            return False

        try:
            # Check if process exists and is our child
            os.kill(self._process_id, 0)
            return True
        except ProcessLookupError:
            return False
