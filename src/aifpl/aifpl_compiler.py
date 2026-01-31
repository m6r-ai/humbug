"""AIFPL Compiler - Orchestrates the complete compilation pipeline.

This is the main entry point for compiling AIFPL source code to bytecode.
It chains together all compilation passes in the correct order.
"""

from aifpl.aifpl_bytecode import CodeObject
from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_semantic_analyzer import AIFPLSemanticAnalyzer
from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_optimizer import AIFPLOptimizer
from aifpl.aifpl_ir_builder import AIFPLIRBuilder
from aifpl.aifpl_codegen import AIFPLCodeGenerator


class AIFPLCompiler:
    """
    Main compiler pass manager.

    Orchestrates the complete compilation pipeline from source to bytecode:
    1. Lexing - source text → tokens
    2. Parsing - tokens → AST
    3. Semantic analysis - validate AST
    4. Desugaring - expand syntactic sugar
    5. AST optimization - constant folding, dead code elimination
    6. IR building - AST → IR (intermediate representation)
    7. IR optimization - dead code, closure optimization (future)
    8. Code generation - IR → bytecode
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
        self.semantic_analyzer = AIFPLSemanticAnalyzer()
        self.desugarer = AIFPLDesugarer()
        self.ast_optimizer = AIFPLOptimizer() if optimize else None
        self.ir_builder = AIFPLIRBuilder()  # AST optimization done separately
        # Future: self.ir_optimizer = AIFPLIROptimizer() if optimize else None
        self.codegen = AIFPLCodeGenerator()

    def compile(self, source: str, name: str = "<module>") -> CodeObject:
        """
        Compile AIFPL source code to bytecode.

        This is the main entry point that runs the complete pipeline.

        Args:
            source: AIFPL source code as a string
            name: Name for the code object (for debugging)

        Returns:
            Compiled bytecode ready for execution
        """
        tokens = self.lexer.lex(source)
        parser = AIFPLParser(tokens, source)
        ast = parser.parse()
        ast = self.semantic_analyzer.analyze(ast)
        ast = self.desugarer.desugar(ast)
        if self.ast_optimizer:
            ast = self.ast_optimizer.optimize(ast)

        ir = self.ir_builder.build(ast, name)
        bytecode = self.codegen.generate(ir, name)

        return bytecode
