"""
Python Module Dependency Checker

A tool for enforcing inter-module dependency rules in Python projects.
"""

__version__ = "1.0.0"
__author__ = "Dependency Checker Tool"

from .validator import DependencyValidator
from .config import DependencyConfig
from .parser import ImportParser
from .reporter import DependencyReporter

__all__ = [
    "DependencyValidator",
    "DependencyConfig", 
    "ImportParser",
    "DependencyReporter"
]
