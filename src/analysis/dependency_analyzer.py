"""Dependency Analyzer — import and inheritance graph from AST / project tree."""

from __future__ import annotations

import ast
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Set


class DependencyType(Enum):
    IMPORT = "import"
    FROM_IMPORT = "from_import"
    CALL = "call"
    INHERITANCE = "inheritance"


@dataclass
class ModuleRelation:
    """Relationship between modules"""

    source: str
    target: str
    relation_type: DependencyType
    line: int


@dataclass
class DependencyGraph:
    """Complete dependency graph"""

    nodes: Set[str] = field(default_factory=set)
    edges: List[ModuleRelation] = field(default_factory=list)

    def detect_cycles(self) -> List[List[str]]:
        """Detect circular dependencies via DFS."""
        adj: Dict[str, List[str]] = {n: [] for n in self.nodes}
        for edge in self.edges:
            adj.setdefault(edge.source, []).append(edge.target)
            adj.setdefault(edge.target, [])

        cycles: List[List[str]] = []
        visiting: Set[str] = set()
        visited: Set[str] = set()
        stack: List[str] = []

        def dfs(node: str) -> None:
            visiting.add(node)
            stack.append(node)
            for nxt in adj.get(node, []):
                if nxt in visiting:
                    if nxt in stack:
                        cycles.append(stack[stack.index(nxt) :] + [nxt])
                    continue
                if nxt not in visited:
                    dfs(nxt)
            stack.pop()
            visiting.remove(node)
            visited.add(node)

        for n in list(adj.keys()):
            if n not in visited:
                dfs(n)
        return cycles

    def get_transitive_dependencies(self, module: str) -> Set[str]:
        """Get all transitive dependencies of a module"""
        adj: Dict[str, List[str]] = {}
        for edge in self.edges:
            adj.setdefault(edge.source, []).append(edge.target)
        seen: Set[str] = set()
        stack = list(adj.get(module, []))
        while stack:
            cur = stack.pop()
            if cur in seen:
                continue
            seen.add(cur)
            stack.extend(adj.get(cur, []))
        return seen


def _as_ast(ast_root: Any) -> Optional[ast.AST]:
    if ast_root is None:
        return None
    if isinstance(ast_root, ast.AST):
        return ast_root
    raw = getattr(ast_root, "raw_node", None)
    return raw if isinstance(raw, ast.AST) else None


class DependencyAnalyzer:
    """Import / inheritance graph. Empty trees stay empty."""

    def analyze_dependencies(
        self, ast_root, source_name: str = "<module>"
    ) -> DependencyGraph:
        graph = DependencyGraph()
        tree = _as_ast(ast_root)
        if tree is None:
            return graph
        graph.nodes.add(source_name)
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                for alias in node.names:
                    target = alias.name.split(".")[0]
                    graph.nodes.add(target)
                    graph.edges.append(
                        ModuleRelation(
                            source_name,
                            target,
                            DependencyType.IMPORT,
                            getattr(node, "lineno", 0),
                        )
                    )
            elif isinstance(node, ast.ImportFrom):
                target = (node.module or "").split(".")[0] or "."
                graph.nodes.add(target)
                graph.edges.append(
                    ModuleRelation(
                        source_name,
                        target,
                        DependencyType.FROM_IMPORT,
                        getattr(node, "lineno", 0),
                    )
                )
            elif isinstance(node, ast.ClassDef):
                for base in node.bases:
                    name = (
                        ast.unparse(base)
                        if hasattr(ast, "unparse")
                        else getattr(base, "id", "")
                    )
                    if name:
                        graph.nodes.add(name)
                        graph.edges.append(
                            ModuleRelation(
                                node.name,
                                name,
                                DependencyType.INHERITANCE,
                                getattr(node, "lineno", 0),
                            )
                        )
        return graph

    def analyze_project_dependencies(self, project_root: str) -> DependencyGraph:
        """Analyze dependencies across entire project"""
        graph = DependencyGraph()
        root = Path(project_root)
        if not root.exists():
            return graph
        for path in root.rglob("*.py"):
            try:
                source = path.read_text(encoding="utf-8")
                tree = ast.parse(source)
            except (OSError, SyntaxError, UnicodeDecodeError):
                continue
            rel = str(path.relative_to(root)).replace("\\", "/")
            piece = self.analyze_dependencies(tree, source_name=rel)
            graph.nodes.update(piece.nodes)
            graph.edges.extend(piece.edges)
        return graph
