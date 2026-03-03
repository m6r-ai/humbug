"""Menai Compiler - Orchestrates the complete compilation pipeline.

This is the main entry point for compiling Menai source code to bytecode.
It chains together all compilation passes in the correct order.
"""

from typing import List

from menai.menai_ast import MenaiASTNode
from menai.menai_ast_constant_folder import MenaiASTConstantFolder
from menai.menai_ast_optimization_pass import MenaiASTOptimizationPass
from menai.menai_bytecode import CodeObject
from menai.menai_cfg_builder import MenaiCFGBuilder
from menai.menai_desugarer import MenaiDesugarer
from menai.menai_ir_builder import MenaiIRBuilder
from menai.menai_ir_optimization_pass import MenaiIROptimizationPass
from menai.menai_ir_copy_propagator import MenaiIRCopyPropagator
from menai.menai_ir_optimizer import MenaiIROptimizer
from menai.menai_ir_inline_once import MenaiIRInlineOnce
from menai.menai_lexer import MenaiLexer
from menai.menai_module_resolver import MenaiModuleResolver, ModuleLoader
from menai.menai_parser import MenaiParser
from menai.menai_semantic_analyzer import MenaiSemanticAnalyzer
from menai.menai_vm_codegen import MenaiVMCodeGen


class MenaiCompiler:
    """
    Main compiler pass manager.
    """

    def __init__(
        self,
        optimize: bool = True,
        module_loader: ModuleLoader | None = None,
    ):
        """
        Initialize compiler with all passes.

        Args:
            optimize:    Enable optimization passes (AST and IR level).
            module_loader: Optional module loader for resolving imports.
        """
        self.optimize = optimize
        self.module_loader = module_loader

        # Initialize all passes
        self.lexer = MenaiLexer()
        self.parser = MenaiParser()
        self.semantic_analyzer = MenaiSemanticAnalyzer()
        self.module_resolver = MenaiModuleResolver(module_loader)
        self.desugarer = MenaiDesugarer()

        # AST optimization passes
        self.ast_passes: List[MenaiASTOptimizationPass] = []
        self.ir_passes: List[MenaiIROptimizationPass] = []
        if optimize:
            self.ast_passes = [
                MenaiASTConstantFolder(),
            ]
            self.ir_passes = [
                MenaiIRCopyPropagator(),
                MenaiIRInlineOnce(),
                MenaiIROptimizer(),
            ]

        self.ir_builder = MenaiIRBuilder()
        self.cfg_builder = MenaiCFGBuilder()
        self.vm_codegen = MenaiVMCodeGen()

    def compile_to_resolved_ast(self, source: str, source_file: str = "") -> MenaiASTNode:
        """
        Compile source to fully resolved AST.

        This runs the front-end compilation stages:
        - Lexing
        - Parsing
        - Semantic analysis
        - Module resolution (including recursive module compilation)

        The result is a fully resolved AST ready for desugaring and backend compilation.
        This method is used by the module system to compile imported modules.

        Args:
            source: Menai source code as a string
            source_file: Source file name for tracking origin of AST nodes

        Returns:
            Fully resolved AST (all imports replaced with module ASTs)
        """
        tokens = self.lexer.lex(source)
        ast = self.parser.parse(tokens, source, source_file)
        checked_ast = self.semantic_analyzer.analyze(ast, source)
        resolved_ast = self.module_resolver.resolve(checked_ast)
        return resolved_ast

    def compile(self, source: str, name: str = "<module>") -> CodeObject:
        """
        Compile Menai source code to bytecode.

        This is the main entry point that runs the complete pipeline.

        Args:
            source: Menai source code as a string
            name: Optional name for the code object (e.g. filename)

        Returns:
            Compiled bytecode ready for execution
        """
        # Front-end: lex, parse, semantic analysis, module resolution.
        resolved_ast = self.compile_to_resolved_ast(source, name)
        desugared_ast = self.desugarer.desugar(resolved_ast)

        for ast_pass in self.ast_passes:
            desugared_ast = ast_pass.optimize(desugared_ast)

        # IR construction and optimisation (shared by both backends).
        ir = self.ir_builder.build(desugared_ast)

        if self.ir_passes:
            changed = True
            while changed:
                changed = False
                for ir_pass in self.ir_passes:
                    ir, pass_changed = ir_pass.optimize(ir)
                    changed = changed or pass_changed

        cfg = self.cfg_builder.build(ir)
        bytecode = self.vm_codegen.generate(cfg, name)
        return bytecode
