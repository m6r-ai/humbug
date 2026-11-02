"""
Launching a new Humbug instance with the same configuration as the current one.

A running instance can open another mindspace in a new window by starting a second
Humbug process.  The new process must be launched the same way the current one was —
which differs between a packaged build and a development run — so the current instance
passes an explicit launch context rather than relying on ambient process state.

The environment is deliberately not propagated.  Global settings, including API keys,
live in ``~/.humbug/user-settings.json``, which the child reads for itself.  Copying
the environment would duplicate secrets into a child process for no benefit.
"""

from dataclasses import dataclass
import logging
import os
import subprocess
import sys


@dataclass
class LaunchContext:
    """
    Everything needed to start a Humbug instance equivalent to the current one.

    Attributes:
        executable: The interpreter or frozen binary to run
        argv: Arguments to pass, excluding the program name
        working_directory: The directory the new process should start in
    """

    executable: str
    argv: list[str]
    working_directory: str

    @classmethod
    def current(cls) -> "LaunchContext":
        """
        Build the launch context describing how this process was started.

        A packaged build runs the application binary directly, so its own argv is the
        right thing to pass on.  A development run is ``python -m desktop``, so the
        module invocation must be reconstructed: the interpreter is re-run with the
        module name rather than this process's script path.

        Returns:
            The launch context for this process
        """
        if getattr(sys, 'frozen', False):
            return cls(
                executable=sys.executable,
                argv=[],
                working_directory=os.getcwd()
            )

        return cls(
            executable=sys.executable,
            argv=["-m", "desktop"],
            working_directory=os.getcwd()
        )

    def spawn(self, mindspace_path: str) -> int:
        """
        Start a new Humbug instance that opens the given mindspace.

        Args:
            mindspace_path: Absolute path to the mindspace the new instance should open

        Returns:
            The process id of the new instance

        Raises:
            OSError: If the new process cannot be started
        """
        argv = [self.executable, *self.argv, "--mindspace", mindspace_path]

        # The environment is inherited deliberately as-is rather than copied and
        # modified.  Nothing extra is added: the child reads global settings from
        # ~/.humbug/user-settings.json, and adding secrets to the environment would
        # be both unnecessary and a disclosure risk.
        process = subprocess.Popen(  # pylint: disable=consider-using-with
            argv,
            cwd=self.working_directory,
            close_fds=True,
            start_new_session=True
        )

        logging.getLogger("LaunchContext").info(
            "Started Humbug instance %d for mindspace %s", process.pid, mindspace_path
        )
        return process.pid
