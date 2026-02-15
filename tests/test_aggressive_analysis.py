"""
Tests for Aggressive Analysis System

MAXIMUM DETAIL TESTING MODE:
- Tests all AST parsing capabilities
- Tests all node type classifications
- Tests scope tracking and binding resolution
- Tests complexity metrics calculation
- Tests function/class/import extraction
- Tests comprehensive report generation

All test cases are explicit with no summarization.
"""

import pytest
import ast
from pathlib import Path
from src.analysis.ast_analyzer import (
    ASTAnalyzer,
    ASTNode,
    ASTNodeType,
    ASTVisitor,
)


class TestASTAnalyzer:
    """Test suite for AST analyzer with maximum coverage"""

    def test_parse_simple_function(self):
        """Test parsing a simple function definition"""
        source = """
def hello_world():
    return "Hello, World!"
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        assert root is not None
        assert root.node_type == ASTNodeType.MODULE

    def test_parse_function_with_arguments(self):
        """Test parsing function with type-annotated arguments"""
        source = """
def add(a: int, b: int) -> int:
    return a + b
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        functions = analyzer.extract_functions(root)
        assert len(functions) == 1
        func = functions[0]
        assert func.name == "add"
        assert len(func.arguments) == 2
        assert func.arguments[0] == ("a", "int")
        assert func.arguments[1] == ("b", "int")
        assert func.return_annotation == "int"

    def test_parse_class_definition(self):
        """Test parsing class with methods and inheritance"""
        source = """
class Animal:
    def __init__(self, name: str):
        self.name = name

class Dog(Animal):
    def bark(self):
        return "Woof!"
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        classes = analyzer.extract_classes(root)
        assert len(classes) == 2

        # Check Animal class
        animal_cls = classes[0]
        assert animal_cls.name == "Animal"
        assert len(animal_cls.base_classes) == 0

        # Check Dog class with inheritance
        dog_cls = classes[1]
        assert dog_cls.name == "Dog"
        assert "Animal" in dog_cls.base_classes

    def test_complexity_calculation(self):
        """Test cyclomatic complexity calculation"""
        source = """
def complex_function(x):
    if x > 0:
        if x > 10:
            return "large"
        else:
            return "small"
    elif x < 0:
        return "negative"
    else:
        return "zero"
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        functions = analyzer.extract_functions(root)
        assert len(functions) == 1
        func = functions[0]

        # Complexity = 1 + number of decision points
        # if (1) + nested if (1) + elif (1) = 3 + base 1 = 4
        assert func.cyclomatic_complexity >= 3

    def test_scope_tracking(self):
        """Test scope ID assignment and tracking"""
        source = """
x = 1

def outer():
    y = 2
    def inner():
        z = 3
    return inner
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        functions = analyzer.extract_functions(root)
        assert len(functions) == 2

        # Each function should have unique scope
        scopes = [f.scope_id for f in functions]
        assert len(scopes) == len(set(scopes))  # All unique

    def test_import_extraction(self):
        """Test extraction of import statements"""
        source = """
import os
import sys
from pathlib import Path
from typing import List, Dict
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        imports = analyzer.extract_imports(root)
        assert len(imports) == 4

        # Check import types
        import_types = [imp.node_type for imp in imports]
        assert ASTNodeType.IMPORT in import_types
        assert ASTNodeType.IMPORT_FROM in import_types

    def test_comprehensive_report_generation(self):
        """Test complete report generation with all details"""
        source = """
import sys
from typing import List

class Calculator:
    def add(self, a: int, b: int) -> int:
        return a + b

    def multiply(self, a: int, b: int) -> int:
        return a * b

def main():
    calc = Calculator()
    result = calc.add(1, 2)
    print(result)
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        report = analyzer.generate_report(root)

        # Verify report contains all expected fields
        assert 'total_lines' in report
        assert 'function_count' in report
        assert 'class_count' in report
        assert 'import_count' in report
        assert 'functions' in report
        assert 'classes' in report
        assert 'imports' in report
        assert 'max_nesting_depth' in report
        assert 'total_bindings' in report
        assert 'total_references' in report

        # Verify counts
        assert report['function_count'] == 3  # add, multiply, main
        assert report['class_count'] == 1
        assert report['import_count'] == 2

        # Verify function details
        assert len(report['functions']) == 3
        for func_info in report['functions']:
            assert 'name' in func_info
            assert 'line' in func_info
            assert 'complexity' in func_info
            assert 'arguments' in func_info

    def test_syntax_error_handling(self):
        """Test handling of syntax errors in source code"""
        source = """
def broken_function(
    # Missing closing parenthesis
    return "broken"
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert root is None
        assert error is not None
        assert "Syntax error" in error

    def test_decorators_extraction(self):
        """Test extraction of function and class decorators"""
        source = """
def my_decorator(func):
    return func

@my_decorator
def decorated_function():
    pass

@property
@my_decorator
def multi_decorated():
    pass
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        functions = analyzer.extract_functions(root)

        # Find decorated functions
        decorated = [f for f in functions if f.decorators]
        assert len(decorated) == 2

        # Check decorator lists
        single_dec = [f for f in decorated if len(f.decorators) == 1]
        multi_dec = [f for f in decorated if len(f.decorators) > 1]
        assert len(single_dec) == 1
        assert len(multi_dec) == 1

    def test_lambda_detection(self):
        """Test detection of lambda expressions"""
        source = """
lambda_func = lambda x: x * 2
another = lambda a, b: a + b
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None

        # Count lambda nodes
        lambda_count = 0

        def count_lambdas(node: ASTNode):
            nonlocal lambda_count
            if node.node_type == ASTNodeType.LAMBDA:
                lambda_count += 1
            for child in node.children:
                count_lambdas(child)

        count_lambdas(root)
        assert lambda_count == 2

    def test_comprehension_detection(self):
        """Test detection of list/dict/set comprehensions"""
        source = """
list_comp = [x * 2 for x in range(10)]
dict_comp = {k: v for k, v in enumerate(list_comp)}
set_comp = {x for x in list_comp if x % 2 == 0}
gen_exp = (x for x in range(10))
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None

        # Count comprehension types
        comp_types = []

        def find_comprehensions(node: ASTNode):
            if node.node_type in (
                ASTNodeType.LISTCOMP,
                ASTNodeType.DICTCOMP,
                ASTNodeType.SETCOMP,
                ASTNodeType.GENERATOREXP,
            ):
                comp_types.append(node.node_type)
            for child in node.children:
                find_comprehensions(child)

        find_comprehensions(root)
        assert ASTNodeType.LISTCOMP in comp_types
        assert ASTNodeType.DICTCOMP in comp_types
        assert ASTNodeType.SETCOMP in comp_types
        assert ASTNodeType.GENERATOREXP in comp_types

    def test_visitor_pattern(self):
        """Test custom visitor implementation"""
        source = """
def function_one():
    pass

def function_two():
    pass

class MyClass:
    pass
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None

        # Custom visitor that counts functions and classes
        class CountingVisitor(ASTVisitor):
            def __init__(self):
                self.function_count = 0
                self.class_count = 0
                self.module_count = 0

            def visit_module(self, node: ASTNode):
                self.module_count += 1
                return super().visit_module(node)

            def visit_function_def(self, node: ASTNode):
                self.function_count += 1
                return self.default_visit(node)

            def visit_class_def(self, node: ASTNode):
                self.class_count += 1
                return self.default_visit(node)

        visitor = CountingVisitor()
        visitor.visit(root)

        assert visitor.function_count == 2
        assert visitor.class_count == 1
        assert visitor.module_count == 1

    def test_empty_source_handling(self):
        """Test handling of empty source code"""
        source = ""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        assert root is not None
        assert root.node_type == ASTNodeType.MODULE

    def test_ast_node_repr(self):
        """Test ASTNode __repr__ method"""
        from src.analysis.ast_analyzer import ASTNode, ASTNodeType

        node = ASTNode(
            node_type=ASTNodeType.FUNCTION_DEF,
            name="test_func",
            line_start=10,
            line_end=20,
            col_start=0,
            col_end=10,
            scope_id="test",
        )

        repr_str = repr(node)
        assert "ASTNode" in repr_str
        assert "function" in repr_str.lower() or "test_func" in repr_str
        assert "10" in repr_str

    def test_bindings_and_references(self):
        """Test tracking of variable bindings and references"""
        source = """
x = 1
y = x + 2
z = y * 3
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        report = analyzer.generate_report(root)

        # Should have bindings for x, y, z
        assert report['total_bindings'] >= 3

        # Should have references to x, y
        assert report['total_references'] >= 2

    def test_file_parsing(self, tmp_path):
        """Test parsing from file"""
        # Create temporary Python file
        test_file = tmp_path / "test_module.py"
        test_file.write_text("""
def test_function():
    return True
""")

        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_file(test_file)

        assert error is None
        assert root is not None

        functions = analyzer.extract_functions(root)
        assert len(functions) == 1
        assert functions[0].name == "test_function"

    def test_unicode_handling(self, tmp_path):
        """Test handling of Unicode characters in source"""
        source = """
def greet():
    return "Hello 世界! 你好!"
"""
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)

        assert error is None
        assert root is not None


class TestSemanticAnalyzer:
    """Test suite for semantic analyzer"""

    def test_placeholder_analysis(self):
        """Test basic semantic analyzer initialization"""
        from src.analysis.semantic_analyzer import SemanticAnalyzer

        analyzer = SemanticAnalyzer()
        assert analyzer is not None
        assert analyzer.root_table is None
        assert analyzer.issues == []

    def test_symbol_table_lookup_in_scope(self):
        """Test symbol lookup in current scope"""
        from src.analysis.semantic_analyzer import (
            SymbolTable,
            Symbol,
            SymbolKind,
        )

        table = SymbolTable(scope_id="test")
        symbol = Symbol(
            name="x",
            kind=SymbolKind.VARIABLE,
            definition_line=1,
            definition_col=0,
            scope_id="test",
        )
        table.symbols["x"] = symbol

        result = table.lookup("x")
        assert result is symbol
        assert result.name == "x"

    def test_symbol_table_lookup_in_parent(self):
        """Test symbol lookup in parent scope"""
        from src.analysis.semantic_analyzer import (
            SymbolTable,
            Symbol,
            SymbolKind,
        )

        parent_table = SymbolTable(scope_id="parent")
        child_table = SymbolTable(scope_id="child", parent=parent_table)

        parent_symbol = Symbol(
            name="y",
            kind=SymbolKind.VARIABLE,
            definition_line=1,
            definition_col=0,
            scope_id="parent",
        )
        parent_table.symbols["y"] = parent_symbol

        result = child_table.lookup("y")
        assert result is parent_symbol

    def test_symbol_table_lookup_not_found(self):
        """Test symbol lookup returns None when not found"""
        from src.analysis.semantic_analyzer import SymbolTable

        table = SymbolTable(scope_id="test")
        result = table.lookup("nonexistent")
        assert result is None

    def test_type_inference_none(self):
        """Test type inference for None"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()
        assert infer.infer_literal_type(None) == "None"

    def test_type_inference_bool(self):
        """Test type inference for bool"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()
        assert infer.infer_literal_type(True) == "bool"
        assert infer.infer_literal_type(False) == "bool"

    def test_type_inference_int(self):
        """Test type inference for int"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()
        assert infer.infer_literal_type(42) == "int"

    def test_type_inference_float(self):
        """Test type inference for float"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()
        assert infer.infer_literal_type(3.14) == "float"

    def test_type_inference_str(self):
        """Test type inference for str"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()
        assert infer.infer_literal_type("hello") == "str"

    def test_type_inference_list(self):
        """Test type inference for list"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()
        assert infer.infer_literal_type([1, 2, 3]) == "list"

    def test_type_inference_tuple(self):
        """Test type inference for tuple"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()
        assert infer.infer_literal_type((1, 2, 3)) == "tuple"

    def test_type_inference_dict(self):
        """Test type inference for dict"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()
        assert infer.infer_literal_type({"a": 1}) == "dict"

    def test_type_inference_set(self):
        """Test type inference for set"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()
        assert infer.infer_literal_type({1, 2, 3}) == "set"

    def test_type_inference_unknown(self):
        """Test type inference for unknown types"""
        from src.analysis.semantic_analyzer import TypeInference

        infer = TypeInference()

        class CustomClass:
            pass

        assert infer.infer_literal_type(CustomClass()) == "unknown"

    def test_semantic_analyzer_analyze(self):
        """Test semantic analyzer analyze method"""
        from src.analysis.semantic_analyzer import SemanticAnalyzer
        from src.analysis.ast_analyzer import ASTAnalyzer

        # Parse simple source
        source = "x = 1"
        ast_analyzer = ASTAnalyzer()
        root, error = ast_analyzer.parse_source(source)
        assert error is None

        # Analyze semantics
        semantic_analyzer = SemanticAnalyzer()
        symbol_table = semantic_analyzer.analyze(root)

        assert symbol_table is not None
        assert symbol_table.scope_id == "module"
        assert semantic_analyzer.root_table is symbol_table

    def test_semantic_analyzer_get_issues(self):
        """Test getting semantic issues"""
        from src.analysis.semantic_analyzer import SemanticAnalyzer

        analyzer = SemanticAnalyzer()
        issues = analyzer.get_issues()
        assert issues == []


class TestMetricsCalculator:
    """Test suite for metrics calculator"""

    def test_placeholder_metrics(self):
        """Test basic metrics calculator"""
        from src.analysis.metrics_calculator import MetricsCalculator

        calc = MetricsCalculator()
        assert calc is not None

    def test_calculate_complexity(self):
        """Test complexity calculation"""
        from src.analysis.metrics_calculator import MetricsCalculator
        from src.analysis.ast_analyzer import ASTAnalyzer

        source = "def foo(): return 1"
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)
        assert error is None

        calc = MetricsCalculator()
        metrics = calc.calculate_complexity(root)
        assert metrics.cyclomatic_complexity >= 1
        assert metrics.cognitive_complexity >= 0
        assert metrics.halstead_volume >= 0.0
        assert metrics.halstead_difficulty >= 0.0

    def test_calculate_maintainability(self):
        """Test maintainability index calculation"""
        from src.analysis.metrics_calculator import MetricsCalculator
        from src.analysis.ast_analyzer import ASTAnalyzer

        source = "x = 1"
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)
        assert error is None

        calc = MetricsCalculator()
        index = calc.calculate_maintainability(root)
        assert 0.0 <= index.index <= 100.0
        assert index.grade in ["A", "B", "C", "D", "F"]


class TestPatternDetector:
    """Test suite for pattern detector"""

    def test_placeholder_patterns(self):
        """Test basic pattern detector"""
        from src.analysis.pattern_detector import PatternDetector

        detector = PatternDetector()
        assert detector is not None

    def test_detect_patterns(self):
        """Test pattern detection"""
        from src.analysis.pattern_detector import PatternDetector
        from src.analysis.ast_analyzer import ASTAnalyzer

        source = "class Singleton: pass"
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)
        assert error is None

        detector = PatternDetector()
        patterns = detector.detect_patterns(root)
        assert isinstance(patterns, list)

    def test_detect_antipatterns(self):
        """Test anti-pattern detection"""
        from src.analysis.pattern_detector import PatternDetector
        from src.analysis.ast_analyzer import ASTAnalyzer

        source = "def foo(): pass"
        analyzer = ASTAnalyzer()
        root, error = analyzer.parse_source(source)
        assert error is None

        detector = PatternDetector()
        antipatterns = detector.detect_antipatterns(root)
        assert isinstance(antipatterns, list)


class TestDependencyAnalyzer:
    """Test suite for dependency analyzer"""

    def test_placeholder_dependencies(self):
        """Test basic dependency analyzer"""
        from src.analysis.dependency_analyzer import DependencyAnalyzer

        analyzer = DependencyAnalyzer()
        assert analyzer is not None

    def test_analyze_dependencies(self):
        """Test dependency analysis"""
        from src.analysis.dependency_analyzer import DependencyAnalyzer
        from src.analysis.ast_analyzer import ASTAnalyzer

        source = "import os\nimport sys"
        ast_analyzer = ASTAnalyzer()
        root, error = ast_analyzer.parse_source(source)
        assert error is None

        analyzer = DependencyAnalyzer()
        graph = analyzer.analyze_dependencies(root)
        assert graph is not None
        assert isinstance(graph.nodes, set)
        assert isinstance(graph.edges, list)

    def test_dependency_graph_detect_cycles(self):
        """Test cycle detection in dependency graph"""
        from src.analysis.dependency_analyzer import DependencyGraph

        graph = DependencyGraph()
        cycles = graph.detect_cycles()
        assert isinstance(cycles, list)

    def test_dependency_graph_get_transitive_dependencies(self):
        """Test transitive dependency calculation"""
        from src.analysis.dependency_analyzer import DependencyGraph

        graph = DependencyGraph()
        deps = graph.get_transitive_dependencies("test_module")
        assert isinstance(deps, set)

    def test_analyze_project_dependencies(self):
        """Test project-wide dependency analysis"""
        from src.analysis.dependency_analyzer import DependencyAnalyzer
        import tempfile
        import os

        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = DependencyAnalyzer()
            graph = analyzer.analyze_project_dependencies(tmpdir)
            assert graph is not None
            assert isinstance(graph.nodes, set)
            assert isinstance(graph.edges, list)


class TestFlowAnalyzer:
    """Test suite for flow analyzer"""

    def test_analyze_control_flow(self):
        """Test control flow analysis"""
        from src.analysis.flow_analyzer import FlowAnalyzer
        from src.analysis.ast_analyzer import ASTAnalyzer

        source = "if x > 0:\n    y = 1\nelse:\n    y = 2"
        ast_analyzer = ASTAnalyzer()
        root, error = ast_analyzer.parse_source(source)
        assert error is None

        analyzer = FlowAnalyzer()
        cfg = analyzer.analyze_control_flow(root)
        assert cfg is not None
        assert cfg.entry_node == "start"
        assert isinstance(cfg.exit_nodes, set)
        assert isinstance(cfg.edges, dict)

    def test_analyze_data_flow(self):
        """Test data flow analysis"""
        from src.analysis.flow_analyzer import FlowAnalyzer
        from src.analysis.ast_analyzer import ASTAnalyzer

        source = "x = 1\ny = x + 2"
        ast_analyzer = ASTAnalyzer()
        root, error = ast_analyzer.parse_source(source)
        assert error is None

        analyzer = FlowAnalyzer()
        dfg = analyzer.analyze_data_flow(root)
        assert dfg is not None
        assert isinstance(dfg.definitions, dict)
        assert isinstance(dfg.uses, dict)


class TestCodeCivilizationIntegration:
    """Test integration of aggressive analysis with code civilization pipeline"""

    def test_aggressive_analysis_in_architectural_pass(self):
        """Test that aggressive analysis is triggered in architectural pass"""
        from src.core.code_civilization import (
            CodeAuthoringCivilization,
            CodeDirective,
            ProgrammingLanguage,
            RequestedOutcome,
            InputType,
        )

        civilization = CodeAuthoringCivilization()

        # Create directive with existing Python code
        directive = CodeDirective(
            directive_id="test-001",
            language=ProgrammingLanguage.PYTHON,
            source="""
def calculate(x, y):
    if x > 0:
        return x + y
    else:
        return x - y
""",
            requested_outcome=RequestedOutcome.AUDIT,
            input_type=InputType.EXISTING_CODE,
            constraints=[],
        )

        # Submit and process directive
        directive_id = civilization.submit_directive(directive)

        # Process through architectural pass (Step 2)
        arch_decision = civilization._architectural_pass(directive)

        # Verify aggressive analysis was performed
        assert arch_decision.approved
        assert arch_decision.ast_analysis is not None

        # Verify comprehensive analysis fields
        if arch_decision.ast_analysis and 'parse_error' not in arch_decision.ast_analysis:
            assert 'total_lines' in arch_decision.ast_analysis
            assert 'function_count' in arch_decision.ast_analysis
            assert 'function_details' in arch_decision.ast_analysis

            # Verify function details
            func_details = arch_decision.ast_analysis['function_details']
            assert len(func_details) > 0

            # Check first function has all required fields
            func = func_details[0]
            assert 'name' in func
            assert 'complexity' in func
            assert 'arguments' in func
            assert 'scope_id' in func
            assert 'bindings' in func
            assert 'references' in func

    def test_analysis_not_triggered_for_spec_input(self):
        """Test that aggressive analysis is NOT triggered for spec-based input"""
        from src.core.code_civilization import (
            CodeAuthoringCivilization,
            CodeDirective,
            ProgrammingLanguage,
            RequestedOutcome,
            InputType,
        )

        civilization = CodeAuthoringCivilization()

        # Create directive with specification (not existing code)
        directive = CodeDirective(
            directive_id="test-002",
            language=ProgrammingLanguage.PYTHON,
            source="Create a function that adds two numbers",
            requested_outcome=RequestedOutcome.EXTEND,
            input_type=InputType.SPEC,
            constraints=[],
        )

        civilization.submit_directive(directive)
        arch_decision = civilization._architectural_pass(directive)

        # Aggressive analysis should NOT be triggered for specs
        assert arch_decision.approved
        assert arch_decision.ast_analysis is None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
