"""Unit tests for the supply store system."""
import pytest
from src.tools.supply_store import (
    SupplyStore, Tool, ToolTag, ToolMetadata, 
    CheckoutRecord, get_supply_store
)
from src.agents.agent import Agent, AgentRole
from src.departments.department import Department


class TestToolMetadata:
    """Test ToolMetadata class."""
    
    def test_metadata_creation(self):
        """Test creating tool metadata."""
        metadata = ToolMetadata(
            tag=ToolTag.COMPILER,
            version="1.0.0",
            trust_score=0.8,
            security_rating=4
        )
        assert metadata.tag == ToolTag.COMPILER
        assert metadata.version == "1.0.0"
        assert metadata.trust_score == 0.8
        assert metadata.security_rating == 4
    
    def test_metadata_defaults(self):
        """Test default metadata values."""
        metadata = ToolMetadata(tag=ToolTag.LINTER, version="1.0")
        assert metadata.trust_score == 0.5
        assert metadata.security_rating == 3
        assert not metadata.requires_justification
    
    def test_metadata_with_capabilities(self):
        """Test metadata with capabilities."""
        metadata = ToolMetadata(
            tag=ToolTag.SDK,
            version="2.0",
            capabilities={"build", "test", "deploy"}
        )
        assert len(metadata.capabilities) == 3
        assert "build" in metadata.capabilities
    
    def test_metadata_to_dict(self):
        """Test serializing metadata."""
        metadata = ToolMetadata(
            tag=ToolTag.COMPILER,
            version="1.0",
            capabilities={"compile", "optimize"}
        )
        data = metadata.to_dict()
        assert data['tag'] == "compiler"
        assert data['version'] == "1.0"
        assert isinstance(data['capabilities'], list)


class TestTool:
    """Test Tool class."""
    
    def test_tool_creation(self):
        """Test creating a tool."""
        metadata = ToolMetadata(tag=ToolTag.COMPILER, version="1.0")
        tool = Tool("tool-001", "GCC", metadata)
        assert tool.entity_id == "tool-001"
        assert tool.name == "GCC"
        assert tool.is_available
        assert tool.checked_out_by is None
    
    def test_tool_to_dict(self):
        """Test serializing tool."""
        metadata = ToolMetadata(tag=ToolTag.LINTER, version="2.0")
        tool = Tool("tool-001", "ESLint", metadata)
        data = tool.to_dict()
        assert data['is_available']
        assert 'tool_metadata' in data


class TestCheckoutRecord:
    """Test CheckoutRecord class."""
    
    def test_record_creation(self):
        """Test creating a checkout record."""
        record = CheckoutRecord(
            tool_id="tool-001",
            agent_id="agent-001",
            justification="Need to compile"
        )
        assert record.tool_id == "tool-001"
        assert record.agent_id == "agent-001"
        assert record.checked_in_at is None
    
    def test_record_to_dict(self):
        """Test serializing checkout record."""
        record = CheckoutRecord(tool_id="tool-001", agent_id="agent-001")
        data = record.to_dict()
        assert 'checkout_id' in data
        assert data['tool_id'] == "tool-001"


class TestSupplyStore:
    """Test SupplyStore class."""
    
    def test_supply_store_creation(self):
        """Test creating a supply store."""
        store = SupplyStore()
        assert len(store.tools) == 0
        assert len(store.checkouts) == 0
    
    def test_add_tool(self):
        """Test adding tool to store."""
        store = SupplyStore()
        metadata = ToolMetadata(tag=ToolTag.COMPILER, version="1.0")
        tool = Tool("tool-001", "GCC", metadata)
        store.add_tool(tool)
        assert len(store.tools) == 1
        assert "tool-001" in store.tools
    
    def test_check_out_tool(self):
        """Test checking out a tool."""
        store = SupplyStore()
        dept = Department("dept-001", "Engineering", "Python")
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        
        metadata = ToolMetadata(tag=ToolTag.COMPILER, version="1.0")
        tool = Tool("tool-001", "GCC", metadata)
        store.add_tool(tool)
        
        record = store.check_out_tool("tool-001", "agent-001", "Need compiler")
        assert record is not None
        assert record.tool_id == "tool-001"
        assert tool.checked_out_by == "agent-001"
        assert not tool.is_available
    
    def test_check_out_unavailable_tool(self):
        """Test checking out unavailable tool."""
        store = SupplyStore()
        dept = Department("dept-001", "Engineering", "Python")
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        
        metadata = ToolMetadata(tag=ToolTag.COMPILER, version="1.0")
        tool = Tool("tool-001", "GCC", metadata)
        store.add_tool(tool)
        
        store.check_out_tool("tool-001", "agent-001")
        record = store.check_out_tool("tool-001", "agent-002")
        assert record is None
    
    def test_check_out_nonexistent_tool(self):
        """Test checking out non-existent tool."""
        store = SupplyStore()
        record = store.check_out_tool("nonexistent", "agent-001")
        assert record is None
    
    def test_check_out_with_justification_required(self):
        """Test checking out tool requiring justification."""
        store = SupplyStore()
        dept = Department("dept-001", "Engineering", "Python")
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        
        metadata = ToolMetadata(
            tag=ToolTag.MCP_SERVER,
            version="1.0",
            requires_justification=True
        )
        tool = Tool("tool-001", "MCP Server", metadata)
        store.add_tool(tool)
        
        # Without justification
        record = store.check_out_tool("tool-001", "agent-001")
        assert record is None
        
        # With justification
        record = store.check_out_tool("tool-001", "agent-001", "Need MCP access")
        assert record is not None
    
    def test_check_in_tool(self):
        """Test checking in a tool."""
        store = SupplyStore()
        dept = Department("dept-001", "Engineering", "Python")
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        
        metadata = ToolMetadata(tag=ToolTag.COMPILER, version="1.0")
        tool = Tool("tool-001", "GCC", metadata)
        store.add_tool(tool)
        
        checkout = store.check_out_tool("tool-001", "agent-001")
        assert checkout is not None
        
        success = store.check_in_tool(checkout.checkout_id)
        assert success
        assert tool.is_available
        assert tool.checked_out_by is None
    
    def test_check_in_nonexistent_checkout(self):
        """Test checking in non-existent checkout."""
        store = SupplyStore()
        result = store.check_in_tool("nonexistent")
        assert not result
    
    def test_check_in_already_checked_in(self):
        """Test checking in already checked in tool."""
        store = SupplyStore()
        dept = Department("dept-001", "Engineering", "Python")
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        
        metadata = ToolMetadata(tag=ToolTag.COMPILER, version="1.0")
        tool = Tool("tool-001", "GCC", metadata)
        store.add_tool(tool)
        
        checkout = store.check_out_tool("tool-001", "agent-001")
        store.check_in_tool(checkout.checkout_id)
        result = store.check_in_tool(checkout.checkout_id)
        assert not result
    
    def test_get_available_tools(self):
        """Test getting available tools."""
        store = SupplyStore()
        dept = Department("dept-001", "Engineering", "Python")
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        
        tool1 = Tool("tool-001", "GCC", ToolMetadata(ToolTag.COMPILER, "1.0"))
        tool2 = Tool("tool-002", "Clang", ToolMetadata(ToolTag.COMPILER, "2.0"))
        store.add_tool(tool1)
        store.add_tool(tool2)
        
        available = store.get_available_tools()
        assert len(available) == 2
        
        store.check_out_tool("tool-001", "agent-001")
        available = store.get_available_tools()
        assert len(available) == 1
    
    def test_get_tools_by_tag(self):
        """Test getting tools by tag."""
        store = SupplyStore()
        tool1 = Tool("tool-001", "GCC", ToolMetadata(ToolTag.COMPILER, "1.0"))
        tool2 = Tool("tool-002", "ESLint", ToolMetadata(ToolTag.LINTER, "2.0"))
        tool3 = Tool("tool-003", "Clang", ToolMetadata(ToolTag.COMPILER, "3.0"))
        store.add_tool(tool1)
        store.add_tool(tool2)
        store.add_tool(tool3)
        
        compilers = store.get_tools_by_tag(ToolTag.COMPILER)
        assert len(compilers) == 2
        
        linters = store.get_tools_by_tag(ToolTag.LINTER)
        assert len(linters) == 1
    
    def test_get_tools_by_capability(self):
        """Test getting tools by capability."""
        store = SupplyStore()
        tool1 = Tool(
            "tool-001", 
            "Tool1", 
            ToolMetadata(ToolTag.SDK, "1.0", capabilities={"build", "test"})
        )
        tool2 = Tool(
            "tool-002", 
            "Tool2", 
            ToolMetadata(ToolTag.SDK, "2.0", capabilities={"deploy"})
        )
        store.add_tool(tool1)
        store.add_tool(tool2)
        
        build_tools = store.get_tools_by_capability("build")
        assert len(build_tools) == 1
        
        deploy_tools = store.get_tools_by_capability("deploy")
        assert len(deploy_tools) == 1
    
    def test_get_agent_checkouts(self):
        """Test getting agent checkouts."""
        store = SupplyStore()
        dept = Department("dept-001", "Engineering", "Python")
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        
        tool1 = Tool("tool-001", "Tool1", ToolMetadata(ToolTag.COMPILER, "1.0"))
        tool2 = Tool("tool-002", "Tool2", ToolMetadata(ToolTag.LINTER, "2.0"))
        store.add_tool(tool1)
        store.add_tool(tool2)
        
        store.check_out_tool("tool-001", "agent-001")
        store.check_out_tool("tool-002", "agent-001")
        
        checkouts = store.get_agent_checkouts("agent-001")
        assert len(checkouts) == 2
    
    def test_get_supply_store_global(self):
        """Test getting global supply store."""
        store = get_supply_store()
        assert store is not None
        assert isinstance(store, SupplyStore)
    
    def test_check_out_tool_nonexistent_agent(self):
        """Test checking out tool with non-existent agent."""
        store = SupplyStore()
        metadata = ToolMetadata(tag=ToolTag.COMPILER, version="1.0")
        tool = Tool("tool-001", "GCC", metadata)
        store.add_tool(tool)
        
        # Try to check out with non-existent agent ID
        record = store.check_out_tool("tool-001", "nonexistent-agent-999")
        assert record is None
    
    def test_check_in_tool_missing_from_store(self):
        """Test checking in tool that was removed from store."""
        store = SupplyStore()
        dept = Department("dept-001", "Engineering", "Python")
        agent = Agent("agent-001", "Test Agent", AgentRole.BUILDER, dept.entity_id)
        
        metadata = ToolMetadata(tag=ToolTag.COMPILER, version="1.0")
        tool = Tool("tool-001", "GCC", metadata)
        store.add_tool(tool)
        
        checkout = store.check_out_tool("tool-001", "agent-001")
        assert checkout is not None
        
        # Remove tool from store
        del store.tools["tool-001"]
        
        # Try to check in
        result = store.check_in_tool(checkout.checkout_id)
        assert not result
