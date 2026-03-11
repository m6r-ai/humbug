"""Menai Compiler - Orchestrates the complete compilation pipeline.

This is the main entry point for compiling Menai source code to bytecode.
It chains together all compilation passes in the correct order.
"""

from typing import List

from menai.menai_ast import MenaiASTNode
from menai.menai_ast_builder import MenaiASTBuilder
from menai.menai_ast_constant_folder import MenaiASTConstantFolder
from menai.menai_ast_desugarer import MenaiASTDesugarer
from menai.menai_ast_module_resolver import MenaiASTModuleResolver, MenaiASTModuleLoader
from menai.menai_ast_optimization_pass import MenaiASTOptimizationPass
from menai.menai_ast_semantic_analyzer import MenaiASTSemanticAnalyzer
from menai.menai_bytecode import CodeObject
from menai.menai_bytecode_builder import MenaiBytecodeBuilder
from menai.menai_cfg_builder import MenaiCFGBuilder
from menai.menai_cfg_optimization_pass import MenaiCFGOptimizationPass
from menai.menai_cfg_branch_const_prop import MenaiCFGBranchConstProp
from menai.menai_cfg_simplify_blocks import MenaiCFGSimplifyBlocks
from menai.menai_cfg_collapse_phi_chains import MenaiCFGCollapsePhiChains
from menai.menai_vcode_builder import MenaiVCodeBuilder
from menai.menai_ir_builder import MenaiIRBuilder
from menai.menai_ir_optimization_pass import MenaiIROptimizationPass
from menai.menai_ir_optimizer import MenaiIROptimizer
from menai.menai_lexer import MenaiLexer


class MenaiCompiler:
    """
    Main compiler pass manager.
    """

    def __init__(
        self,
        module_loader: MenaiASTModuleLoader | None = None,
    ):
        """
        Initialize compiler with all passes.

        Args:
            module_loader: Optional module loader for resolving imports.
        """
        self.module_loader = module_loader

        self.lexer = MenaiLexer()
        self.ast_builder = MenaiASTBuilder()
        self.ast_semantic_analyzer = MenaiASTSemanticAnalyzer()
        self.ast_module_resolver = MenaiASTModuleResolver(module_loader)
        self.ast_desugarer = MenaiASTDesugarer()
        self.ast_passes: List[MenaiASTOptimizationPass] = [
            MenaiASTConstantFolder(),
        ]
        self.ir_builder = MenaiIRBuilder()
        self.ir_passes: List[MenaiIROptimizationPass] = [
            MenaiIROptimizer(),
        ]
        self.cfg_builder = MenaiCFGBuilder()
        self.cfg_passes: List[MenaiCFGOptimizationPass] = [
            MenaiCFGCollapsePhiChains(),
            MenaiCFGBranchConstProp(),
            MenaiCFGSimplifyBlocks(),
        ]
        self.vcode_builder = MenaiVCodeBuilder()
        self.bytecode_builder = MenaiBytecodeBuilder()


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
        ast = self.ast_builder.build(tokens, source, source_file)
        checked_ast = self.ast_semantic_analyzer.analyze(ast, source)
        resolved_ast = self.ast_module_resolver.resolve(checked_ast)
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
        resolved_ast = self.compile_to_resolved_ast(source, name)
        desugared_ast = self.ast_desugarer.desugar(resolved_ast)

        for ast_pass in self.ast_passes:
            desugared_ast = ast_pass.optimize(desugared_ast)

        ir = self.ir_builder.build(desugared_ast)

        for ir_pass in self.ir_passes:
            ir, _ = ir_pass.optimize(ir)

        cfg = self.cfg_builder.build(ir)
        for cfg_pass in self.cfg_passes:
            cfg, _ = cfg_pass.optimize(cfg)

        vcode = self.vcode_builder.build(cfg)
        bytecode = self.bytecode_builder.build(vcode, name)
        return bytecode
