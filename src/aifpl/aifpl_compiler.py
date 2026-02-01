"""AIFPL Compiler - Orchestrates the complete compilation pipeline.

This is the main entry point for compiling AIFPL source code to bytecode.
It chains together all compilation passes in the correct order.
"""

from typing import List

from aifpl.aifpl_bytecode import CodeObject
from aifpl.aifpl_codegen import AIFPLCodeGen
from aifpl.aifpl_constant_folding_pass import AIFPLConstantFoldingPass
from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_ir_builder import AIFPLIRBuilder
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_optimization_pass import AIFPLOptimizationPass
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_semantic_analyzer import AIFPLSemanticAnalyzer


class AIFPLCompiler:
    """
    Main compiler pass manager.
    """

    def __init__(self, optimize: bool = True):
        """
        Initialize compiler with all passes.

        Args:
            optimize: Enable optimization passes (AST and IR level)
        """
        self.optimize = optimize

        # Initialize all passes
        self.lexer = AIFPLLexer()
        self.parser = AIFPLParser()
        self.semantic_analyzer = AIFPLSemanticAnalyzer()
        self.desugarer = AIFPLDesugarer()

        # AST optimization passes
        self.ast_passes: List[AIFPLOptimizationPass] = []
        if optimize:
            self.ast_passes = [
                AIFPLConstantFoldingPass(),
                # Future: DeadCodeEliminationPass(),
            ]

        self.ir_builder = AIFPLIRBuilder()

        # Future: self.ir_optimizer = AIFPLIROptimizer() if optimize else None
        # Future: self.ir_passes = [...]
        self.codegen = AIFPLCodeGen()

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
        tokens = self.lexer.lex(source)
        ast = self.parser.parse(tokens, source)
        checked_ast = self.semantic_analyzer.analyze(ast)
        desugared_ast = self.desugarer.desugar(checked_ast)

        for ast_pass in self.ast_passes:
            desugared_ast = ast_pass.optimize(desugared_ast)

        ir = self.ir_builder.build(desugared_ast)

        bytecode = self.codegen.generate(ir, name)

        return bytecode
