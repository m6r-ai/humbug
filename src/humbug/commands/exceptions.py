"""Exceptions for command processing operations."""


class CommandNotFoundError(Exception):
    """
    Unknown command errors.

    Raised when the user attempts to execute a command that doesn't exist
    in the registered command set.
    """


class CommandExecutionError(Exception):
    """
    Command execution failure.
    
    Raised by commands when they encounter unrecoverable errors during
    execution. Commands should wrap their specific errors in this exception
    with appropriate context.
    """
