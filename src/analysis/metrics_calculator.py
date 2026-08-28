"""Metrics Calculator — cyclomatic, cognitive, and Halstead metrics from AST."""

from __future__ import annotations

import ast
import math
from dataclasses import dataclass
from typing import Any, Optional


@dataclass
class ComplexityMetrics:
    """Complete complexity metrics"""

    cyclomatic_complexity: int
    cognitive_complexity: int
    halstead_volume: float
    halstead_difficulty: float


@dataclass
class MaintainabilityIndex:
    """Maintainability index calculation"""

    index: float  # 0-100 scale
    grade: str  # A-F grade


_BRANCH_NODES = (
    ast.If,
    ast.For,
    ast.AsyncFor,
    ast.While,
    ast.ExceptHandler,
    ast.With,
    ast.AsyncWith,
    ast.Assert,
    ast.IfExp,
)


def _as_ast(ast_root: Any) -> Optional[ast.AST]:
    if ast_root is None:
        return None
    if isinstance(ast_root, ast.AST):
        return ast_root
    raw = getattr(ast_root, "raw_node", None)
    return raw if isinstance(raw, ast.AST) else None


class MetricsCalculator:
    """Code metrics calculated from a Python AST. Tiny files still grade A."""

    def calculate_complexity(self, ast_root) -> ComplexityMetrics:
        tree = _as_ast(ast_root)
        if tree is None:
            return ComplexityMetrics(1, 0, 0.0, 0.0)

        cyclomatic = 1
        cognitive = 0
        operators = 0
        operands = 0
        unique_ops: set = set()
        unique_operands: set = set()

        for node in ast.walk(tree):
            if isinstance(node, _BRANCH_NODES):
                cyclomatic += 1
                cognitive += 1
            if isinstance(node, ast.BoolOp):
                cyclomatic += max(0, len(node.values) - 1)
                cognitive += 1
            if isinstance(node, ast.comprehension) and node.ifs:
                cyclomatic += len(node.ifs)
            if isinstance(node, (ast.BinOp, ast.UnaryOp, ast.BoolOp, ast.Compare)):
                operators += 1
                unique_ops.add(type(node).__name__)
            if isinstance(node, ast.Name):
                operands += 1
                unique_operands.add(node.id)
            if isinstance(node, ast.Constant):
                operands += 1
                unique_operands.add(repr(node.value))

        n1 = max(1, len(unique_ops))
        n2 = max(1, len(unique_operands))
        N1 = max(1, operators)
        N2 = max(1, operands)
        vocab = n1 + n2
        length = N1 + N2
        volume = length * math.log2(vocab) if vocab > 0 else 0.0
        difficulty = (n1 / 2.0) * (N2 / n2)

        return ComplexityMetrics(
            cyclomatic_complexity=cyclomatic,
            cognitive_complexity=cognitive,
            halstead_volume=round(volume, 2),
            halstead_difficulty=round(difficulty, 2),
        )

    def calculate_maintainability(self, ast_root) -> MaintainabilityIndex:
        metrics = self.calculate_complexity(ast_root)
        loc = 1
        tree = _as_ast(ast_root)
        if tree is not None:
            last = getattr(tree, "end_lineno", None)
            loc = max(1, last or getattr(ast_root, "line_end", 1) or 1)
        volume = max(metrics.halstead_volume, 1.0)
        mi = (
            171.0
            - 5.2 * math.log(volume)
            - 0.23 * metrics.cyclomatic_complexity
            - 16.2 * math.log(loc)
        )
        index = max(0.0, min(100.0, mi))
        if index >= 85:
            grade = "A"
        elif index >= 70:
            grade = "B"
        elif index >= 50:
            grade = "C"
        elif index >= 30:
            grade = "D"
        else:
            grade = "F"
        return MaintainabilityIndex(index=round(index, 1), grade=grade)
