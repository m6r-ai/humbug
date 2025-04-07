# File: windows_terminal.py
"""Windows-specific terminal implementation."""

import asyncio
import ctypes
from ctypes import windll, byref, pointer, c_void_p, c_ulong, Structure
from ctypes import c_size_t, POINTER
from ctypes.wintypes import HANDLE, DWORD, WORD, LPWSTR, BOOL, LPVOID, BYTE
import msvcrt
import os
from typing import Optional, Tuple, cast

from humbug.terminal.terminal_base import TerminalBase


# Define DWORD_PTR for 64-bit compatibility
DWORD_PTR = c_ulong if ctypes.sizeof(c_void_p) == 4 else ctypes.c_uint64

# Windows Constants
EXTENDED_STARTUPINFO_PRESENT = 0x00080000
STILL_ACTIVE = 259


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


class WindowsTerminal(TerminalBase):
    """Windows-specific terminal implementation using ConPTY."""

    def __init__(self):
        """Initialize Windows terminal."""
        super().__init__()

        # Store handles
        self._pty_handle = None
        self._process_handle = None
        self._thread_handle = None
        self._pipe_in = None
        self._pipe_out = None
        self._main_fd = None

        # Load ConPTY functions
        self._load_conpty_functions()

    def _load_conpty_functions(self):
        """Load required ConPTY functions."""
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

        self._ResizePseudoConsole = kernel32.ResizePseudoConsole
        self._ResizePseudoConsole.argtypes = [HANDLE, COORD]
        self._ResizePseudoConsole.restype = DWORD

        self._InitializeProcThreadAttributeList = kernel32.InitializeProcThreadAttributeList
        self._InitializeProcThreadAttributeList.restype = BOOL
        self._InitializeProcThreadAttributeList.argtypes = [
            LPVOID,          # lpAttributeList
            DWORD,           # dwAttributeCount
            DWORD,           # dwFlags
            POINTER(c_size_t)  # lpSize
        ]

        self._UpdateProcThreadAttribute = kernel32.UpdateProcThreadAttribute
        self._UpdateProcThreadAttribute.restype = BOOL
        self._UpdateProcThreadAttribute.argtypes = [
            LPVOID,          # lpAttributeList
            DWORD,           # dwFlags
            DWORD_PTR,       # Attribute
            LPVOID,          # lpValue
            c_size_t,        # cbSize
            LPVOID,          # lpPreviousValue
            LPVOID           # lpReturnSize
        ]

    async def start(self, command: Optional[str] = None) -> Tuple[int, int]:
        """
        Start Windows terminal process using ConPTY.

        Args:
            command: Optional command to run. If None, uses default shell.

        Returns:
            Tuple of (process_id, file_descriptor)

        Raises:
            OSError: If terminal creation fails
        """
        try:
            # Create pipes
            pipe_in_read = HANDLE()
            pipe_in_write = HANDLE()
            pipe_out_read = HANDLE()
            pipe_out_write = HANDLE()

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
                0,
                pointer(pty_handle)
            )

            if result != 0:
                raise OSError(f"Failed to create ConPTY: {result}")

            # Store handles
            self._pty_handle = pty_handle
            self._pipe_in = pipe_in_write.value
            self._pipe_out = pipe_out_read.value

            # Create STARTUPINFOEX structure
            startup_info_ex = STARTUPINFOEX()
            startup_info_ex.StartupInfo.cb = ctypes.sizeof(STARTUPINFOEX)

            # Get required size
            size = c_size_t()
            self._InitializeProcThreadAttributeList(None, 1, 0, byref(size))

            # Allocate buffer
            raw_buffer = ctypes.create_string_buffer(size.value)
            startup_info_ex.lpAttributeList = ctypes.cast(raw_buffer, LPVOID)

            # Initialize list
            success = self._InitializeProcThreadAttributeList(
                startup_info_ex.lpAttributeList,
                1,
                0,
                byref(size)
            )

            if not success:
                raise OSError(f"InitializeProcThreadAttributeList failed: {ctypes.get_last_error()}")

            # Add the ConPTY attribute
            PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE = 0x20016
            success = self._UpdateProcThreadAttribute(
                startup_info_ex.lpAttributeList,
                0,
                PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
                pty_handle,
                ctypes.sizeof(HANDLE),
                None,
                None
            )

            if not success:
                raise OSError(f"UpdateProcThreadAttribute failed: {ctypes.get_last_error()}")

            # Create process
            process_info = PROCESS_INFORMATION()
            shell = command if command else os.environ.get('COMSPEC', 'cmd.exe')
            self._process_name = shell

            if not windll.kernel32.CreateProcessW(
                None,
                shell,
                None,
                None,
                False,
                EXTENDED_STARTUPINFO_PRESENT,
                None,
                None,
                byref(startup_info_ex.StartupInfo),
                byref(process_info)
            ):
                raise OSError("Failed to create process")

            # Close unused pipe ends
            windll.kernel32.CloseHandle(pipe_in_read)
            windll.kernel32.CloseHandle(pipe_out_write)

            # Store process info
            self._process_handle = process_info.hProcess
            self._thread_handle = process_info.hThread
            self._process_id = process_info.dwProcessId

            # Create Windows async file handle
            self._main_fd = msvcrt.open_osfhandle(self._pipe_out, os.O_RDWR)

            return self._process_id, self._main_fd

        except Exception as e:
            self._logger.exception("Failed to start ConPTY process: %s", str(e))
            self._cleanup_handles()
            raise

    async def terminate(self) -> None:
        """Terminate Windows terminal process."""
        self._running = False

        if self._process_handle:
            try:
                windll.kernel32.TerminateProcess(self._process_handle, 1)
            except Exception as e:
                self._logger.warning("Error terminating process: %s", str(e))

        self._process_id = None
        self._cleanup_handles()

    def _cleanup_handles(self):
        """Clean up Windows handles."""
        if self._pty_handle:
            try:
                self._ClosePseudoConsole(self._pty_handle)
            except Exception as e:
                self._logger.warning("Error closing ConPTY: %s", str(e))
            self._pty_handle = None

        for handle in (self._process_handle, self._thread_handle):
            if handle:
                try:
                    windll.kernel32.CloseHandle(handle)
                except Exception as e:
                    self._logger.warning("Error closing handle: %s", str(e))

        self._process_handle = None
        self._thread_handle = None

        for handle in (self._pipe_in, self._pipe_out):
            if handle:
                try:
                    windll.kernel32.CloseHandle(handle)
                except Exception as e:
                    self._logger.warning("Error closing pipe handle: %s", str(e))

        self._pipe_in = None
        self._pipe_out = None
        self._main_fd = None

    def is_running(self) -> bool:
        """Check if Windows process is running."""
        if not self._process_handle:
            return False

        exit_code = DWORD()
        if not windll.kernel32.GetExitCodeProcess(self._process_handle, byref(exit_code)):
            return False

        return exit_code.value == STILL_ACTIVE

    def update_window_size(self, rows: int, cols: int) -> None:
        """Update ConPTY window size."""
        if self._pty_handle:
            try:
                coord = COORD(cols, rows)
                result = self._ResizePseudoConsole(
                    self._pty_handle,
                    coord
                )
                if result != 0:
                    self._logger.warning("Failed to resize ConPTY: %s", repr(result))
            except Exception as e:
                self._logger.warning("Error updating window size: %s", str(e))

    async def read_data(self, size: int) -> bytes:
        """
        Read data from Windows terminal.

        Returns:
            bytes: Data read from terminal

        Raises:
            EOFError: If pipe is closed/broken
            OSError: On other errors
        """
        if not self._main_fd:
            raise EOFError("Terminal pipe closed")

        loop = asyncio.get_event_loop()
        try:
            data = await loop.run_in_executor(
                None,
                lambda: os.read(self._main_fd, size)
            )
            return data
        except OSError as e:
            if e.winerror == 109:  # ERROR_BROKEN_PIPE
                raise EOFError("Terminal pipe closed")
            if not self._running:
                return b''
            raise

    async def write_data(self, data: bytes) -> None:
        """Write data to Windows terminal."""
        if self._pipe_in is not None and self._running:
            loop = asyncio.get_event_loop()
            try:
                bytes_written = DWORD(0)
                await loop.run_in_executor(
                    None,
                    lambda: windll.kernel32.WriteFile(
                        self._pipe_in,
                        data,
                        len(data),
                        byref(bytes_written),
                        None
                    )
                )
            except OSError as e:
                if e.winerror == 109:  # ERROR_BROKEN_PIPE
                    return
                raise

    def transfer_to(self, other: 'TerminalBase') -> None:
        """Transfer Windows terminal ownership."""
        other_terminal = cast(WindowsTerminal, other)

        other_terminal._process_id = self._process_id
        other_terminal._process_name = self._process_name
        other_terminal._process_handle = self._process_handle
        other_terminal._thread_handle = self._thread_handle
        other_terminal._pty_handle = self._pty_handle
        other_terminal._pipe_in = self._pipe_in
        other_terminal._pipe_out = self._pipe_out
        other_terminal._main_fd = self._main_fd
        other_terminal._running = True

        # Clear our state without closing handles
        self._process_id = None
        self._process_name = ""
        self._process_handle = None
        self._thread_handle = None
        self._pty_handle = None
        self._pipe_in = None
        self._pipe_out = None
        self._main_fd = None
        self._running = False
