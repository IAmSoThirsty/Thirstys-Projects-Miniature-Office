"""Pattern Detector — AST-backed design patterns and anti-patterns."""

from __future__ import annotations

import ast
from dataclasses import dataclass
from enum import Enum
from typing import Any, Iterable, List, Optional


class PatternType(Enum):
    SINGLETON = "singleton"
    FACTORY = "factory"
    OBSERVER = "observer"
    DECORATOR = "decorator"
    STRATEGY = "strategy"


class AntiPatternType(Enum):
    GOD_CLASS = "god_class"
    LONG_METHOD = "long_method"
    MAGIC_NUMBERS = "magic_numbers"
    DEEP_NESTING = "deep_nesting"


@dataclass
class DesignPattern:
    """Detected design pattern"""

    pattern_type: PatternType
    location: int
    confidence: float


@dataclass
class AntiPattern:
    """Detected anti-pattern"""

    pattern_type: AntiPatternType
    location: int
    severity: str


def _as_ast(ast_root: Any) -> Optional[ast.AST]:
    if ast_root is None:
        return None
    if isinstance(ast_root, ast.AST):
        return ast_root
    raw = getattr(ast_root, "raw_node", None)
    return raw if isinstance(raw, ast.AST) else None


def _walk_custom(node: Any) -> Iterable[Any]:
    yield node
    for child in getattr(node, "children", []) or []:
        yield from _walk_custom(child)


class PatternDetector:
    """Detect a small, named set of patterns from a Python AST."""

    def detect_patterns(self, ast_root) -> List[DesignPattern]:
        tree = _as_ast(ast_root)
        found: List[DesignPattern] = []
        if tree is None:
            return found

        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                found.extend(self._class_patterns(node))
            elif isinstance(node, ast.FunctionDef):
                found.extend(self._function_patterns(node))
        return found

    def detect_antipatterns(self, ast_root) -> List[AntiPattern]:
        tree = _as_ast(ast_root)
        found: List[AntiPattern] = []
        if tree is None:
            return found

        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                methods = [
                    n
                    for n in node.body
                    if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef))
                ]
                if len(methods) >= 15:
                    found.append(
                        AntiPattern(
                            AntiPatternType.GOD_CLASS,
                            getattr(node, "lineno", 1),
                            "high",
                        )
                    )
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                end = getattr(node, "end_lineno", node.lineno) or node.lineno
                if (end - node.lineno) >= 40:
                    found.append(
                        AntiPattern(AntiPatternType.LONG_METHOD, node.lineno, "medium")
                    )
                if self._max_depth(node) >= 5:
                    found.append(
                        AntiPattern(AntiPatternType.DEEP_NESTING, node.lineno, "medium")
                    )
            if isinstance(node, ast.Constant) and isinstance(node.value, (int, float)):
                if node.value not in (0, 1, -1, 0.0, 1.0, -1.0) and abs(node.value) > 1:
                    found.append(
                        AntiPattern(
                            AntiPatternType.MAGIC_NUMBERS,
                            getattr(node, "lineno", 1),
                            "low",
                        )
                    )
        return found

    def _class_patterns(self, node: ast.ClassDef) -> List[DesignPattern]:
        found: List[DesignPattern] = []
        method_names = {
            n.name
            for n in node.body
            if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef))
        }
        has_instance = any(
            isinstance(n, ast.Assign)
            and any(isinstance(t, ast.Name) and t.id == "_instance" for t in n.targets)
            for n in node.body
        )
        if node.name.lower() == "singleton" or (
            has_instance and "__new__" in method_names
        ):
            found.append(DesignPattern(PatternType.SINGLETON, node.lineno, 0.85))
        if "factory" in node.name.lower() or "create" in method_names:
            found.append(DesignPattern(PatternType.FACTORY, node.lineno, 0.7))
        observer_hooks = {"subscribe", "unsubscribe", "notify", "attach", "detach"}
        if method_names & observer_hooks:
            found.append(DesignPattern(PatternType.OBSERVER, node.lineno, 0.75))
        if {"execute", "apply"} & method_names and "strategy" in node.name.lower():
            found.append(DesignPattern(PatternType.STRATEGY, node.lineno, 0.8))
        elif "strategy" in node.name.lower():
            found.append(DesignPattern(PatternType.STRATEGY, node.lineno, 0.6))
        return found

    def _function_patterns(self, node: ast.FunctionDef) -> List[DesignPattern]:
        found: List[DesignPattern] = []
        if node.name.lower().startswith("create_") or node.name.lower().endswith(
            "_factory"
        ):
            found.append(DesignPattern(PatternType.FACTORY, node.lineno, 0.65))
        nested = [n for n in node.body if isinstance(n, ast.FunctionDef)]
        if nested and any(isinstance(n, ast.Return) for n in node.body):
            found.append(DesignPattern(PatternType.DECORATOR, node.lineno, 0.6))
        return found

    def _max_depth(self, node: ast.AST, depth: int = 0) -> int:
        branch = (
            ast.If,
            ast.For,
            ast.AsyncFor,
            ast.While,
            ast.With,
            ast.AsyncWith,
            ast.Try,
        )
        deepest = depth
        for child in ast.iter_child_nodes(node):
            next_depth = depth + 1 if isinstance(child, branch) else depth
            deepest = max(deepest, self._max_depth(child, next_depth))
        return deepest
