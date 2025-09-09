"""
Import statement parser for Python files.
"""

import ast
from pathlib import Path
from typing import List, Set, Optional
from dataclasses import dataclass


@dataclass
class ImportInfo:
    """Information about an import statement."""

    module: str  # The imported module name
    line_number: int  # Line number in source file
    import_type: str  # 'import' or 'from'
    imported_names: List[str]  # Names imported (for 'from' imports)
    is_relative: bool  # Whether it's a relative import
    raw_statement: str  # Original import statement


class ImportParser:
    """Parser for extracting import statements from Python files."""

    def __init__(self, src_root: str = "src"):
        self.src_root = Path(src_root)
        self.stdlib_modules = self._get_stdlib_modules()

    def _get_stdlib_modules(self) -> Set[str]:
        """Get a set of Python standard library module names."""
        # Comprehensive list of Python standard library modules
        stdlib = {
            # Built-in modules
            'builtins', '__builtin__', '__main__', 'sys', 'os', 'io', 'time', 'datetime',
            're', 'math', 'random', 'statistics', 'decimal', 'fractions', 'numbers', 'struct', 'zoneinfo',

            # Text processing
            'string', 'textwrap', 'unicodedata', 'stringprep', 'readline', 'rlcompleter',

            # Data types
            'collections', 'array', 'weakref', 'types', 'copy', 'pprint', 'reprlib', 'enum',

            # Numeric and mathematical
            'cmath', 'itertools', 'functools', 'operator',

            # File and directory access
            'pathlib', 'os.path', 'fileinput', 'stat', 'filecmp', 'tempfile', 'glob', 'fnmatch',
            'linecache', 'shutil',

            # Data persistence
            'pickle', 'copyreg', 'shelve', 'marshal', 'dbm', 'sqlite3',

            # Data compression and archiving
            'zlib', 'gzip', 'bz2', 'lzma', 'zipfile', 'tarfile',

            # File formats
            'csv', 'configparser', 'netrc', 'xdrlib', 'plistlib',

            # Cryptographic services
            'hashlib', 'hmac', 'secrets',

            # Generic operating system services
            'argparse', 'getopt', 'logging', 'getpass', 'curses', 'platform', 'errno', 'ctypes',

            # Concurrent execution
            'threading', 'multiprocessing', 'concurrent', 'subprocess', 'sched', 'queue',
            '_thread', '_dummy_thread',

            # Networking and interprocess communication
            'asyncio', 'socket', 'ssl', 'select', 'selectors', 'asyncore', 'asynchat', 'signal',
            'mmap',

            # Internet data handling
            'email', 'json', 'mailcap', 'mailbox', 'mimetypes', 'base64', 'binhex', 'binascii',
            'quopri', 'uu',

            # Structured markup processing
            'html', 'xml', 'webbrowser',

            # Internet protocols and support
            'urllib', 'http', 'ftplib', 'poplib', 'imaplib', 'nntplib', 'smtplib', 'smtpd',
            'telnetlib', 'uuid', 'socketserver', 'xmlrpc',

            # Multimedia services
            'audioop', 'aifc', 'sunau', 'wave', 'chunk', 'colorsys', 'imghdr', 'sndhdr', 'ossaudiodev',

            # Internationalization
            'gettext', 'locale',

            # Program frameworks
            'turtle', 'cmd', 'shlex',

            # Graphical user interfaces
            'tkinter', 'tkinter.ttk', 'tkinter.tix', 'tkinter.scrolledtext',

            # Development tools
            'typing', 'pydoc', 'doctest', 'unittest', 'test', 'trace', 'tracemalloc',

            # Debugging and profiling
            'bdb', 'faulthandler', 'pdb', 'profile', 'pstats', 'timeit', 'cProfile',

            # Software packaging and distribution
            'distutils', 'ensurepip', 'venv', 'zipapp',

            # Python runtime services
            'warnings', 'dataclasses', 'contextlib', 'abc', 'atexit', 'traceback', 'gc',
            'inspect', 'site', 'code', 'codeop',

            # Custom Python interpreters
            'parser', 'ast', 'symtable', 'symbol', 'token', 'keyword', 'tokenize', 'tabnanny',
            'py_compile', 'compileall', 'dis', 'pickletools',

            # MS Windows specific
            'msilib', 'msvcrt', 'winreg', 'winsound',

            # Unix specific
            'posix', 'pwd', 'spwd', 'grp', 'crypt', 'termios', 'tty', 'pty', 'fcntl', 'pipes',
            'resource', 'nis', 'syslog',

            # Superseded modules (still part of stdlib)
            'optparse', 'imp'
        }
        return stdlib

    def parse_file(self, file_path: str) -> List[ImportInfo]:
        """Parse a Python file and extract all import statements."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            tree = ast.parse(content, filename=file_path)
            imports = []

            for node in ast.walk(tree):
                if isinstance(node, ast.Import):
                    for alias in node.names:
                        imports.append(ImportInfo(
                            module=alias.name,
                            line_number=node.lineno,
                            import_type='import',
                            imported_names=[alias.asname or alias.name],
                            is_relative=False,
                            raw_statement=f"import {alias.name}"
                        ))

                elif isinstance(node, ast.ImportFrom):
                    module_name = node.module or ''
                    is_relative = node.level > 0

                    # Handle relative imports
                    if is_relative:
                        module_name = '.' * node.level + (module_name or '')

                    imported_names = []
                    for alias in node.names:
                        imported_names.append(alias.asname or alias.name)

                    imports.append(ImportInfo(
                        module=module_name,
                        line_number=node.lineno,
                        import_type='from',
                        imported_names=imported_names,
                        is_relative=is_relative,
                        raw_statement=f"from {module_name} import {', '.join(imported_names)}"
                    ))

            return imports

        except Exception as e:
            print(f"Warning: Could not parse {file_path}: {e}")
            return []

    def resolve_module_name(self, import_info: ImportInfo, file_path: str, known_modules: Set[str]) -> Optional[str]:
        """Resolve an import to a known internal module name."""
        module_name = import_info.module

        # Handle relative imports
        if import_info.is_relative:
            file_path_obj = Path(file_path)
            current_module = self._get_module_from_path(file_path_obj)

            if current_module:
                # Calculate the target module for relative import
                levels_up = import_info.module.count('.')
                current_parts = current_module.split('.')

                if levels_up >= len(current_parts):
                    return None  # Invalid relative import

                base_parts = current_parts[:-levels_up] if levels_up > 0 else current_parts
                relative_part = module_name.lstrip('.')

                if relative_part:
                    resolved = '.'.join(base_parts + [relative_part])
                else:
                    resolved = '.'.join(base_parts)

                # Check if this resolves to a known module
                for known_module in known_modules:
                    if resolved.startswith(known_module):
                        return known_module

        # Handle absolute imports
        else:
            # Check if it's a direct module match
            if module_name in known_modules:
                return module_name

            # Check if it's a submodule of a known module
            for known_module in known_modules:
                if module_name.startswith(known_module + '.'):
                    return known_module

        return None

    def _get_module_from_path(self, file_path: Path) -> Optional[str]:
        """Get the module name from a file path."""
        try:
            rel_path = file_path.relative_to(self.src_root)
            parts = rel_path.parts[:-1]  # Remove filename
            if parts:
                return '.'.join(parts)
        except ValueError:
            pass
        return None

    def is_standard_library_import(self, module_name: str) -> bool:
        """Check if an import is from the Python standard library."""
        # Get the top-level module name
        top_level = module_name.split('.')[0]
        return top_level in self.stdlib_modules

    def is_third_party_import(self, module_name: str, known_internal_modules: Set[str]) -> bool:
        """Check if an import is from a third-party package."""
        # Not standard library and not internal module
        if self.is_standard_library_import(module_name):
            return False

        # Check if it's an internal module
        top_level = module_name.split('.')[0]
        if top_level in known_internal_modules:
            return False

        # Check if it's a submodule of an internal module
        for internal_module in known_internal_modules:
            if module_name.startswith(internal_module + '.'):
                return False

        return True

    def get_external_module_name(self, import_info: ImportInfo) -> str:
        """Get the external module name for dependency checking."""
        # For external modules, we typically care about the top-level package
        return import_info.module.split('.')[0]

    def get_python_files(self, directory: str, ignore_patterns: Optional[List[str]] = None) -> List[str]:
        """Get all Python files in a directory, respecting ignore patterns."""
        if ignore_patterns is None:
            ignore_patterns = []

        python_files: List[str] = []
        directory_path = Path(directory)

        if not directory_path.exists():
            return python_files

        for file_path in directory_path.rglob("*.py"):
            # Check ignore patterns
            should_ignore = False
            for pattern in ignore_patterns:
                if file_path.match(pattern) or str(file_path).find(pattern.replace('*', '')) != -1:
                    should_ignore = True
                    break

            if not should_ignore:
                python_files.append(str(file_path))

        return python_files
