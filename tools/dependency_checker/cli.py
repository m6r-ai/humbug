"""
Command-line interface for the dependency checker.
"""

import sys
import argparse
import os

from .config import DependencyConfig
from .validator import DependencyValidator
from .reporter import DependencyReporter
from .utils import create_dependency_graph, get_project_stats


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Python Module Dependency Checker",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s check                    # Check all modules
  %(prog)s check --module humbug    # Check specific module
  %(prog)s init                     # Create default config
  %(prog)s validate-config          # Validate configuration
  %(prog)s graph --output deps.dot  # Generate dependency graph
        """
    )

    subparsers = parser.add_subparsers(dest='command', help='Available commands')

    # Check command
    check_parser = subparsers.add_parser('check', help='Check dependencies')
    check_parser.add_argument('--config', '-c', default='dependency-rules.yaml',
                             help='Configuration file path')
    check_parser.add_argument('--module', '-m', help='Check specific module only')
    check_parser.add_argument('--format', '-f', choices=['text', 'json', 'csv'],
                             default='text', help='Output format')
    check_parser.add_argument('--output', '-o', help='Output file path')
    check_parser.add_argument('--verbose', '-v', action='store_true',
                             help='Verbose output')
    check_parser.add_argument('--src-root', default='src',
                             help='Source root directory')

    # Init command
    init_parser = subparsers.add_parser('init', help='Create default configuration')
    init_parser.add_argument('--config', '-c', default='dependency-rules.yaml',
                            help='Configuration file path')
    init_parser.add_argument('--src-root', default='src',
                            help='Source root directory')
    init_parser.add_argument('--force', action='store_true',
                            help='Overwrite existing configuration')

    # Validate config command
    validate_parser = subparsers.add_parser('validate-config', help='Validate configuration')
    validate_parser.add_argument('--config', '-c', default='dependency-rules.yaml',
                                help='Configuration file path')

    # Graph command
    graph_parser = subparsers.add_parser('graph', help='Generate dependency graph')
    graph_parser.add_argument('--config', '-c', default='dependency-rules.yaml',
                             help='Configuration file path')
    graph_parser.add_argument('--output', '-o', required=True,
                             help='Output file path for graph')
    graph_parser.add_argument('--format', choices=['dot'], default='dot',
                             help='Graph format')

    # Stats command
    stats_parser = subparsers.add_parser('stats', help='Show project statistics')
    stats_parser.add_argument('--src-root', default='src',
                             help='Source root directory')

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return 1

    try:
        if args.command == 'check':
            return handle_check(args)
        elif args.command == 'init':
            return handle_init(args)
        elif args.command == 'validate-config':
            return handle_validate_config(args)
        elif args.command == 'graph':
            return handle_graph(args)
        elif args.command == 'stats':
            return handle_stats(args)
        else:
            print(f"Unknown command: {args.command}")
            return 1

    except Exception as e:
        print(f"Error: {e}")
        return 1


def handle_check(args):
    """Handle the check command."""
    if not os.path.exists(args.config):
        print(f"Configuration file not found: {args.config}")
        print("Run 'dependency-checker init' to create a default configuration.")
        return 1

    config = DependencyConfig.load_from_file(args.config)
    config.src_root = args.src_root

    # Validate config first
    config_errors = config.validate()
    if config_errors:
        print("Configuration errors found:")
        for error in config_errors:
            print(f"  - {error}")
        return 1

    validator = DependencyValidator(config)
    reporter = DependencyReporter()

    # Run validation
    if args.module:
        if args.module not in config.get_all_modules():
            print(f"Module '{args.module}' not found in configuration.")
            return 1
        result = validator.validate_module(args.module)
    else:
        result = validator.validate_all()

    # Output results
    if args.output:
        reporter.save_results(result, args.output, args.format, args.verbose)
        print(f"Results saved to: {args.output}")
    else:
        reporter.print_results(result, args.format, args.verbose)

    return reporter.get_exit_code(result)


def handle_init(args):
    """Handle the init command."""
    if os.path.exists(args.config) and not args.force:
        print(f"Configuration file already exists: {args.config}")
        print("Use --force to overwrite.")
        return 1

    if not os.path.exists(args.src_root):
        print(f"Source root directory not found: {args.src_root}")
        return 1

    config = DependencyConfig.create_default(args.src_root)
    config.save_to_file(args.config)

    print(f"Created configuration file: {args.config}")
    print(f"Found modules: {', '.join(sorted(config.get_all_modules()))}")
    print("\\nEdit the configuration file to define dependency rules.")
    print("Example structure:")
    print("  modules:")
    print("    humbug:")
    print("      internal_dependencies:")
    print("        - ai")
    print("        - syntax")
    print("      external_dependencies:")
    print("        - \"standard_library\"")
    print("        - \"PySide6\"")
    print("    ai:")
    print("      internal_dependencies:")
    print("        - syntax")
    print("      external_dependencies:")
    print("        - \"standard_library\"")

    return 0


def handle_validate_config(args):
    """Handle the validate-config command."""
    if not os.path.exists(args.config):
        print(f"Configuration file not found: {args.config}")
        return 1

    try:
        config = DependencyConfig.load_from_file(args.config)
        errors = config.validate()

        if errors:
            print("Configuration validation failed:")
            for error in errors:
                print(f"  ✗ {error}")
            return 1
        else:
            print("✓ Configuration is valid")
            print(f"  Modules: {len(config.get_all_modules())}")

            # Count total dependencies
            total_internal = sum(len(module_config.internal_dependencies) for module_config in config.modules.values())
            total_external = sum(len(module_config.external_dependencies) for module_config in config.modules.values())

            print(f"  Total internal dependency rules: {total_internal}")
            print(f"  Total external dependency rules: {total_external}")
            return 0

    except Exception as e:
        print(f"Error loading configuration: {e}")
        return 1


def handle_graph(args):
    """Handle the graph command."""
    if not os.path.exists(args.config):
        print(f"Configuration file not found: {args.config}")
        return 1

    config = DependencyConfig.load_from_file(args.config)

    if args.format == 'dot':
        # Convert to format expected by create_dependency_graph
        modules_data = {}
        for module_name, module_config in config.modules.items():
            modules_data[module_name] = module_config.internal_dependencies

        graph_content = create_dependency_graph({'modules': modules_data})

        with open(args.output, 'w', encoding='utf-8') as f:
            f.write(graph_content)

        print(f"Dependency graph saved to: {args.output}")
        print("To render as image: dot -Tpng deps.dot -o deps.png")
        return 0

    return 1


def handle_stats(args):
    """Handle the stats command."""
    if not os.path.exists(args.src_root):
        print(f"Source root directory not found: {args.src_root}")
        return 1

    stats = get_project_stats(args.src_root)

    print("Project Statistics")
    print("=" * 18)
    print(f"Source root: {args.src_root}")
    print(f"Total files: {stats['total_files']}")
    print(f"Python files: {stats['python_files']}")
    print(f"Total lines of code: {stats['total_lines']:,}")
    print(f"Modules found: {len(stats['modules'])}")

    if stats['modules']:
        print(f"Module names: {', '.join(sorted(stats['modules']))}")
        if stats['largest_module']:
            print(f"Largest module: {stats['largest_module']} ({stats['largest_module_files']} files)")

    return 0


if __name__ == '__main__':
    sys.exit(main())
