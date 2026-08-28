"""Flow Analyzer — control and data flow from a Python AST."""

from __future__ import annotations

import ast
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set


@dataclass
class ControlFlowGraph:
    """Control flow graph with all branching paths"""

    entry_node: str
    exit_nodes: Set[str] = field(default_factory=set)
    edges: Dict[str, List[str]] = field(default_factory=dict)


@dataclass
class DataFlowGraph:
    """Data flow graph tracking variable definitions and uses"""

    definitions: Dict[str, List[int]] = field(default_factory=dict)
    uses: Dict[str, List[int]] = field(default_factory=dict)


def _as_ast(ast_root: Any) -> Optional[ast.AST]:
    if ast_root is None:
        return None
    if isinstance(ast_root, ast.AST):
        return ast_root
    raw = getattr(ast_root, "raw_node", None)
    return raw if isinstance(raw, ast.AST) else None


class FlowAnalyzer:
    """Build a small CFG/DFG. Entry is always named 'start'."""

    def analyze_control_flow(self, ast_root) -> ControlFlowGraph:
        cfg = ControlFlowGraph(entry_node="start")
        tree = _as_ast(ast_root)
        if tree is None:
            return cfg

        edges: Dict[str, List[str]] = {"start": []}
        exits: Set[str] = set()
        prev = "start"
        counter = 0

        def node_id(prefix: str, node: ast.AST) -> str:
            nonlocal counter
            counter += 1
            return f"{prefix}:{getattr(node, 'lineno', counter)}"

        for node in ast.walk(tree):
            if isinstance(node, ast.If):
                nid = node_id("if", node)
                edges.setdefault(prev, []).append(nid)
                edges.setdefault(nid, [])
                prev = nid
            elif isinstance(node, (ast.For, ast.AsyncFor, ast.While)):
                nid = node_id("loop", node)
                edges.setdefault(prev, []).append(nid)
                edges.setdefault(nid, [nid])
                prev = nid
            elif isinstance(node, ast.Return):
                nid = node_id("return", node)
                edges.setdefault(prev, []).append(nid)
                exits.add(nid)
                prev = nid

        if prev != "start":
            exits.add(prev)
        else:
            exits.add("start")
        cfg.edges = edges
        cfg.exit_nodes = exits
        return cfg

    def analyze_data_flow(self, ast_root) -> DataFlowGraph:
        dfg = DataFlowGraph()
        tree = _as_ast(ast_root)
        if tree is None:
            return dfg

        for node in ast.walk(tree):
            if isinstance(node, ast.Assign):
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        dfg.definitions.setdefault(target.id, []).append(
                            getattr(node, "lineno", 0)
                        )
            elif isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name):
                dfg.definitions.setdefault(node.target.id, []).append(
                    getattr(node, "lineno", 0)
                )
            elif isinstance(node, ast.Name) and isinstance(node.ctx, ast.Load):
                dfg.uses.setdefault(node.id, []).append(getattr(node, "lineno", 0))
        return dfg
