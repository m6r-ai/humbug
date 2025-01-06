import json
import logging
import os
import shutil
from typing import Dict, List, Optional


class WorkspaceError(Exception):
    """Base exception for workspace-related errors."""


class WorkspaceNotFoundError(WorkspaceError):
    """Raised when attempting to access a non-existent workspace."""


class WorkspaceExistsError(WorkspaceError):
    """Raised when attempting to create a workspace that already exists."""
