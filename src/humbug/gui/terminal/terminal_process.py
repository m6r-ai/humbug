"""Platform-specific terminal process implementations using only standard library modules."""

import asyncio
import logging
import os
import signal
import struct
import sys
from abc import ABC, abstractmethod
from typing import Optional, Tuple

# Unix-specific imports
if sys.platform != 'win32':
    import pty
    import termios
    import fcntl

# Windows-specific imports
if sys.platform == 'win32':
    import ctypes
    import msvcrt
    from ctypes import windll, byref, pointer, c_void_p, c_ulong, c_int, Structure, Union
    from ctypes import c_char, c_wchar_p, c_size_t, c_char_p, POINTER, cast
    from ctypes.wintypes import HANDLE, DWORD, WORD, LPWSTR, BOOL, LPCWSTR, LPVOID, BYTE

if sys.platform == 'win32':
    # Windows Constants
    GENERIC_READ = 0x80000000
    GENERIC_WRITE = 0x40000000
    OPEN_EXISTING = 3
    FILE_FLAG_OVERLAPPED = 0x40000000
    PIPE_ACCESS_DUPLEX = 0x3
    PIPE_TYPE_BYTE = 0x0
    PIPE_READMODE_BYTE = 0x0
    PIPE_WAIT = 0x0
    PIPE_UNLIMITED_INSTANCES = 255
    ERROR_PIPE_BUSY = 231
    ERROR_MORE_DATA = 234
    INVALID_HANDLE_VALUE = HANDLE(-1)
    PROCESS_INFORMATION_CLASS = c_int
    EXTENDED_STARTUPINFO_PRESENT = 0x00080000
    CREATE_NEW_CONSOLE = 0x00000010
    CREATE_NO_WINDOW = 0x08000000
    STARTF_USESTDHANDLES = 0x00000100
    STILL_ACTIVE = 259
    INFINITE = 0xFFFFFFFF
    STATUS_PENDING = 0x103


    class COORD(Structure):
        """Windows COORD structure."""
        _fields_ = [
            ("X", WORD),
            ("Y", WORD)
        ]

    class STARTUPINFO(Structure):
        """Windows STARTUPINFO structure."""
        _fields_ = [
            ("cb", DWORD),
            ("lpReserved", LPWSTR),
            ("lpDesktop", LPWSTR),
            ("lpTitle", LPWSTR),
            ("dwX", DWORD),
            ("dwY", DWORD),
            ("dwXSize", DWORD),
            ("dwYSize", DWORD),
            ("dwXCountChars", DWORD),
            ("dwYCountChars", DWORD),
            ("dwFillAttribute", DWORD),
            ("dwFlags", DWORD),
            ("wShowWindow", WORD),
            ("cbReserved2", WORD),
            ("lpReserved2", POINTER(BYTE)),
            ("hStdInput", HANDLE),
            ("hStdOutput", HANDLE),
            ("hStdError", HANDLE)
        ]

    class STARTUPINFOEX(Structure):
        """Windows STARTUPINFOEX structure."""
        _fields_ = [
            ("StartupInfo", STARTUPINFO),
            ("lpAttributeList", LPVOID)
        ]

    class PROCESS_INFORMATION(Structure):
        """Windows PROCESS_INFORMATION structure."""
        _fields_ = [
            ("hProcess", HANDLE),
            ("hThread", HANDLE),
            ("dwProcessId", DWORD),
            ("dwThreadId", DWORD)
        ]


class TerminalProcessBase(ABC):
    """Abstract base class for platform-specific terminal process management."""

    def __init__(self):
        """Initialize terminal process manager."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._process_id: Optional[int] = None
        self._main_fd: Optional[int] = None
        self._running = True

    @abstractmethod
    async def start(self, command: Optional[str] = None) -> Tuple[int, int]:
        """
        Start a new terminal process.

        Args:
            command: Optional command to run instead of shell

        Returns:
            Tuple of (process_id, master_fd)

        Raises:
            OSError: If process creation fails
        """
        pass

    @abstractmethod
    async def terminate(self) -> None:
        """Terminate the terminal process."""
        pass

    @abstractmethod
    def is_running(self) -> bool:
        """Check if the process is still running."""
        pass

    def get_process_id(self) -> Optional[int]:
        """Get the process ID of the terminal process."""
        return self._process_id

    def get_main_fd(self) -> Optional[int]:
        """Get the master file descriptor."""
        return self._main_fd


if sys.platform != 'win32':
    class UnixTerminalProcess(TerminalProcessBase):
        """Unix-specific terminal process implementation using PTY."""

        def _set_nonblocking(self, fd: int) -> None:
            """Set file descriptor to non-blocking mode."""
            flags = fcntl.fcntl(fd, fcntl.F_GETFL)
            fcntl.fcntl(fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)

        async def start(self, command: Optional[str] = None) -> Tuple[int, int]:
            """Start a new terminal process with proper PTY setup."""
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


if sys.platform == 'win32':
    class WindowsTerminalProcess(TerminalProcessBase):
        """Windows-specific terminal process implementation using ConPTY."""

        def __init__(self):
            """Initialize Windows terminal process."""
            super().__init__()
            self._pty_handle = None
            self._process_handle = None
            self._thread_handle = None
            self._pipe_in = None
            self._pipe_out = None

            # Load ConPTY functions
            kernel32 = windll.kernel32
            self._CreatePseudoConsole = kernel32.CreatePseudoConsole
            self._CreatePseudoConsole.restype = DWORD
            self._CreatePseudoConsole.argtypes = [
                COORD,
                HANDLE,
                HANDLE,
                DWORD,
                POINTER(HANDLE)
            ]

            self._ClosePseudoConsole = kernel32.ClosePseudoConsole
            self._ClosePseudoConsole.argtypes = [HANDLE]

        async def start(self, command: Optional[str] = None) -> Tuple[int, int]:
            """Start a new terminal process using ConPTY.

            Args:
                command: Optional command to run instead of shell

            Returns:
                Tuple of (process_id, master_fd)
            """
            try:
                # Create pipes for ConPTY
                pipe_in_read = HANDLE()
                pipe_in_write = HANDLE()
                pipe_out_read = HANDLE()
                pipe_out_write = HANDLE()

                # Create the pipes
                if not windll.kernel32.CreatePipe(byref(pipe_in_read), byref(pipe_in_write), None, 0):
                    raise OSError("Failed to create input pipe")
                if not windll.kernel32.CreatePipe(byref(pipe_out_read), byref(pipe_out_write), None, 0):
                    raise OSError("Failed to create output pipe")

                # Create ConPTY
                coord = COORD(80, 24)  # Initial size
                pty_handle = HANDLE()
                result = self._CreatePseudoConsole(
                    coord,
                    pipe_in_read,
                    pipe_out_write,
                    0,  # No special flags
                    pointer(pty_handle)
                )

                if result != 0:
                    raise OSError(f"Failed to create ConPTY: {result}")

                # Store handles
                self._pty_handle = pty_handle
                self._pipe_in = pipe_in_write.value
                self._pipe_out = pipe_out_read.value

                # Close the pipe ends passed to ConPTY as we don't need them
                windll.kernel32.CloseHandle(pipe_in_read)
                windll.kernel32.CloseHandle(pipe_out_write)

                # Initialize startup info with ConPTY
                startup_info_ex = STARTUPINFOEX()
                startup_info_ex.StartupInfo.cb = ctypes.sizeof(STARTUPINFOEX)

                # Allocate attribute list
                size = DWORD()
                windll.kernel32.InitializeProcThreadAttributeList(None, 1, 0, byref(size))

                # Allocate memory for attribute list
                buffer = ctypes.create_string_buffer(size.value)
                startup_info_ex.lpAttributeList = ctypes.cast(buffer, LPVOID)

                # Initialize attribute list
                if not windll.kernel32.InitializeProcThreadAttributeList(
                    startup_info_ex.lpAttributeList, 1, 0, byref(size)
                ):
                    raise OSError("Failed to initialize attribute list")

                # Set ConPTY attribute
                if not windll.kernel32.UpdateProcThreadAttribute(
                    startup_info_ex.lpAttributeList,
                    0,
                    PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
                    pty_handle,
                    ctypes.sizeof(HANDLE),
                    None,
                    None
                ):
                    raise OSError("Failed to set pseudoconsole attribute")

                # Create the process
                process_info = PROCESS_INFORMATION()
                shell = command if command else os.environ.get('COMSPEC', 'cmd.exe')

                if not windll.kernel32.CreateProcessW(
                    None,                           # No module name (use command line)
                    shell,                          # Command line
                    None,                           # Process handle not inheritable
                    None,                           # Thread handle not inheritable
                    False,                          # Set handle inheritance to FALSE
                    EXTENDED_STARTUPINFO_PRESENT,   # Creation flags
                    None,                           # Use parent's environment block
                    None,                           # Use parent's starting directory
                    byref(startup_info_ex.StartupInfo),  # Pointer to STARTUPINFO
                    byref(process_info)             # Pointer to PROCESS_INFORMATION
                ):
                    raise OSError("Failed to create process")

                # Store process info
                self._process_handle = process_info.hProcess
                self._thread_handle = process_info.hThread
                self._process_id = process_info.dwProcessId

                # Convert the pipe handle to a file descriptor
                self._main_fd = msvcrt.open_osfhandle(self._pipe_out, os.O_RDWR)

                return self._process_id, self._main_fd

            except Exception as e:
                self._logger.exception("Failed to start ConPTY process: %s", e)
                self._cleanup_handles()
                raise

        def _cleanup_handles(self):
            """Clean up Windows handles."""
            if self._pty_handle:
                try:
                    self._ClosePseudoConsole(self._pty_handle)
                except Exception as e:
                    self._logger.warning("Error closing ConPTY: %s", e)
                self._pty_handle = None

            for handle in (self._process_handle, self._thread_handle):
                if handle:
                    try:
                        windll.kernel32.CloseHandle(handle)
                    except Exception as e:
                        self._logger.warning("Error closing handle: %s", e)

            self._process_handle = None
            self._thread_handle = None

            for handle in (self._pipe_in, self._pipe_out):
                if handle:
                    try:
                        windll.kernel32.CloseHandle(handle)
                    except Exception as e:
                        self._logger.warning("Error closing pipe handle: %s", e)

            self._pipe_in = None
            self._pipe_out = None
            self._main_fd = None

        async def terminate(self) -> None:
            """Terminate the terminal process."""
            self._running = False

            if self._process_handle:
                try:
                    windll.kernel32.TerminateProcess(self._process_handle, 1)
                except Exception as e:
                    self._logger.warning("Error terminating process: %s", e)

            self._process_id = None
            self._cleanup_handles()

        def is_running(self) -> bool:
            """Check if the process is still running."""
            if not self._process_handle:
                return False

            exit_code = DWORD()
            if not windll.kernel32.GetExitCodeProcess(self._process_handle, byref(exit_code)):
                return False

            return exit_code.value == 259  # STILL_ACTIVE


def create_terminal_process() -> TerminalProcessBase:
    """Create appropriate terminal process implementation for current platform."""
    if sys.platform == 'win32':
        return WindowsTerminalProcess()
    return UnixTerminalProcess()
