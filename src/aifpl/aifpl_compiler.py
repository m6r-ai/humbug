"""AIFPL Compiler - Orchestrates the complete compilation pipeline.

This is the main entry point for compiling AIFPL source code to bytecode.
It chains together all compilation passes in the correct order.
"""

from typing import List, Optional

from aifpl.aifpl_ast import AIFPLASTNode
from aifpl.aifpl_bytecode import CodeObject
from aifpl.aifpl_codegen import AIFPLCodeGen
from aifpl.aifpl_constant_folder import AIFPLConstantFolder
from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_ir_builder import AIFPLIRBuilder
from aifpl.aifpl_ir_optimization_pass import AIFPLIROptimizationPass
from aifpl.aifpl_ir_optimizer import AIFPLIROptimizer
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_module_resolver import AIFPLModuleResolver, ModuleLoader
from aifpl.aifpl_optimization_pass import AIFPLOptimizationPass
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_semantic_analyzer import AIFPLSemanticAnalyzer


class AIFPLCompiler:
    """
    Main compiler pass manager.
    """

    def __init__(self, optimize: bool = True, module_loader: Optional[ModuleLoader] = None):
        """
        Initialize compiler with all passes.

        Args:
            optimize: Enable optimization passes (AST and IR level)
            module_loader: Optional module loader for resolving imports
        """
        self.optimize = optimize
        self.module_loader = module_loader

        # Initialize all passes
        self.lexer = AIFPLLexer()
        self.parser = AIFPLParser()
        self.semantic_analyzer = AIFPLSemanticAnalyzer()
        self.module_resolver = AIFPLModuleResolver(module_loader)
        self.desugarer = AIFPLDesugarer()

        # AST optimization passes
        self.ast_passes: List[AIFPLOptimizationPass] = []
        self.ir_passes: List[AIFPLIROptimizationPass] = []
        if optimize:
            self.ast_passes = [
                AIFPLConstantFolder(),
            ]
            self.ir_passes = [
                AIFPLIROptimizer(),
            ]

        self.ir_builder = AIFPLIRBuilder()
        self.codegen = AIFPLCodeGen()

    def compile_to_resolved_ast(self, source: str, source_file: str = "") -> AIFPLASTNode:
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
            source: AIFPL source code as a string
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
        Compile AIFPL source code to bytecode.

        This is the main entry point that runs the complete pipeline.

        Args:
            source: AIFPL source code as a string
            name: Optional name for the code object (e.g. filename)

        Returns:
            Compiled bytecode ready for execution
        """
        # Use the partial compilation to get resolved AST
        resolved_ast = self.compile_to_resolved_ast(source, name)
        desugared_ast = self.desugarer.desugar(resolved_ast)

        for ast_pass in self.ast_passes:
            desugared_ast = ast_pass.optimize(desugared_ast)

        ir = self.ir_builder.build(desugared_ast)

        # IR-level optimization: run each pass to fixed point, then repeat the
        # full sequence until no pass makes any further changes.
        if self.ir_passes:
            changed = True
            while changed:
                changed = False
                for ir_pass in self.ir_passes:
                    ir, pass_changed = ir_pass.optimize(ir)
                    changed = changed or pass_changed

        bytecode = self.codegen.generate(ir, name)

        return bytecode
