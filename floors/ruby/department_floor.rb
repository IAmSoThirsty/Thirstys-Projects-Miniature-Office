#!/usr/bin/env ruby
# frozen_string_literal: true

##
# FLOOR 16 - RUBY JURISDICTION
# Department Floor Implementation
#
# Domain: Scripting, Web frameworks (Rails/Sinatra), Developer tools
# Architectural Law: Idiomatic Ruby, SOLID principles, Duck typing
# Security Doctrine: Input sanitization, SQL injection prevention, Mass assignment protection

require 'json'
require 'time'

# ============================================================================
# CORE DATA MODELS
# ============================================================================

class FloorAgent
  attr_reader :agent_id, :name, :role, :office, :capabilities

  def initialize(agent_id:, name:, role:, office:, capabilities:)
    @agent_id = agent_id
    @name = name
    @role = role
    @office = office
    @capabilities = capabilities
  end

  def to_h
    {
      agentId: @agent_id,
      name: @name,
      role: @role,
      office: @office,
      capabilities: @capabilities
    }
  end
end

class Task
  attr_reader :task_id, :title, :assigned_to, :created_at
  attr_accessor :status, :completed_at

  def initialize(task_id:, title:, status:, assigned_to:, created_at:)
    @task_id = task_id
    @title = title
    @status = status
    @assigned_to = assigned_to
    @created_at = created_at
    @completed_at = nil
  end

  def to_h
    {
      taskId: @task_id,
      title: @title,
      status: @status,
      assignedTo: @assigned_to,
      createdAt: @created_at,
      completedAt: @completed_at
    }
  end
end

class CodeAnalysis
  attr_reader :lines, :classes, :modules, :methods, :symbols, :language

  def initialize(lines:, classes:, modules:, methods:, symbols:, language:)
    @lines = lines
    @classes = classes
    @modules = modules
    @methods = methods
    @symbols = symbols
    @language = language
  end

  def to_h
    {
      lines: @lines,
      classes: @classes,
      modules: @modules,
      methods: @methods,
      symbols: @symbols,
      language: @language
    }
  end
end

# ============================================================================
# AGENT CLASSES - Production Grade Implementations
# ============================================================================

class ServiceAgent
  ##
  # Service Agent - Handles HTTP services, APIs, and external integrations
  # Office: Implementation Office
  
  attr_reader :agent_id, :name, :capabilities

  def initialize(agent_id, name)
    @agent_id = agent_id
    @name = name
    @capabilities = [
      'http_handling',
      'rest_api',
      'rack_middleware',
      'external_integration'
    ]
  end

  def execute_service(service_name, params)
    case service_name
    when 'validate_style'
      validate_style(params)
    when 'check_idioms'
      check_idioms(params)
    when 'analyze_gems'
      analyze_gems(params)
    else
      raise "Unknown service: #{service_name}"
    end
  end

  private

  def validate_style(params)
    code = params['code'] || raise('Code parameter required')
    
    issues = []
    issues << 'Use 2-space indentation' if code.include?('    ')
    issues << 'Use snake_case for methods' if code.match?(/def [a-z]+[A-Z]/)
    issues << 'Use single quotes for strings without interpolation' if code.match?(/"[^#"]*"/)
    
    {
      valid: issues.empty?,
      issues: issues,
      issueCount: issues.length
    }
  end

  def check_idioms(params)
    code = params['code'] || raise('Code parameter required')
    
    idiomatic = []
    non_idiomatic = []
    
    idiomatic << 'Uses blocks and iterators' if code.match?(/\.(each|map|select|reject)/)
    idiomatic << 'Uses symbols appropriately' if code.include?(':')
    idiomatic << 'Uses string interpolation' if code.match?(/#\{/)
    
    non_idiomatic << 'C-style for loops' if code.match?(/for\s+\w+\s+in/)
    non_idiomatic << 'Unnecessary explicit return' if code.match?(/return\s+\w+\s*\n\s*end/)
    
    {
      idiomatic: idiomatic,
      nonIdiomatic: non_idiomatic,
      score: idiomatic.length - non_idiomatic.length
    }
  end

  def analyze_gems(params)
    code = params['code'] || raise('Code parameter required')
    
    requires = code.scan(/require\s+['"]([^'"]+)['"]/).flatten
    gem_specs = code.scan(/gem\s+['"]([^'"]+)['"]/).flatten
    
    {
      requireCount: requires.length,
      gemCount: gem_specs.length,
      requires: requires,
      gems: gem_specs
    }
  end

  public

  def info
    FloorAgent.new(
      agent_id: @agent_id,
      name: @name,
      role: 'Service Agent',
      office: 'Implementation Office',
      capabilities: @capabilities
    )
  end
end

class DataModelAgent
  ##
  # Data Model Agent - Manages ActiveRecord models, data structures, serialization
  # Office: Architecture Office
  
  attr_reader :agent_id, :name, :capabilities

  def initialize(agent_id, name)
    @agent_id = agent_id
    @name = name
    @capabilities = [
      'activerecord_modeling',
      'data_validation',
      'serialization',
      'schema_design'
    ]
  end

  def process_data(operation, data)
    case operation
    when 'validate'
      validate_data(data)
    when 'transform'
      transform_data(data)
    when 'infer_structure'
      infer_structure(data)
    else
      raise "Unknown operation: #{operation}"
    end
  end

  private

  def validate_data(data)
    if data.nil?
      return { valid: false, error: 'Data is nil' }
    end
    
    type = data.class.name
    is_hash = data.is_a?(Hash)
    is_array = data.is_a?(Array)
    
    {
      valid: true,
      type: type,
      isHash: is_hash,
      isArray: is_array,
      keys: is_hash ? data.keys : []
    }
  end

  def transform_data(data)
    unless data.is_a?(Hash)
      return { transformed: data, changesMade: false }
    end
    
    transformed = {}
    changes_made = false
    
    data.each do |key, value|
      camel_key = key.to_s.split('_').map.with_index { |w, i| i.zero? ? w : w.capitalize }.join
      transformed[camel_key] = value
      changes_made = true if camel_key != key.to_s
    end
    
    { transformed: transformed, changesMade: changes_made }
  end

  def infer_structure(data)
    return { type: 'nil' } if data.nil?
    
    type = data.class.name
    
    if data.is_a?(Array)
      element_types = data.map { |item| item.class.name }.uniq
      return {
        type: 'Array',
        elementTypes: element_types,
        length: data.length
      }
    end
    
    if data.is_a?(Hash)
      schema = {}
      data.each do |key, value|
        schema[key] = value.class.name
      end
      return {
        type: 'Hash',
        schema: schema
      }
    end
    
    { type: type }
  end

  public

  def info
    FloorAgent.new(
      agent_id: @agent_id,
      name: @name,
      role: 'Data Model Agent',
      office: 'Architecture Office',
      capabilities: @capabilities
    )
  end
end

class OperationsAgent
  ##
  # Operations Agent - Handles code analysis, refactoring, and quality checks
  # Office: Review Office
  
  attr_reader :agent_id, :name, :capabilities

  def initialize(agent_id, name)
    @agent_id = agent_id
    @name = name
    @capabilities = [
      'code_analysis',
      'rubocop_integration',
      'quality_checks',
      'refactoring'
    ]
  end

  def execute_operation(operation, params)
    code = params['code'] || raise('Code parameter required')
    
    case operation
    when 'analyze'
      analyze_code(code)
    when 'check_quality'
      check_quality(code)
    when 'find_issues'
      find_issues(code)
    else
      raise "Unknown operation: #{operation}"
    end
  end

  private

  def analyze_code(code)
    lines = code.lines.count
    classes = code.scan(/class\s+\w+/).length
    modules = code.scan(/module\s+\w+/).length
    methods = code.scan(/def\s+\w+/).length
    symbols = code.scan(/:\w+/).length
    
    analysis = CodeAnalysis.new(
      lines: lines,
      classes: classes,
      modules: modules,
      methods: methods,
      symbols: symbols,
      language: 'ruby'
    )
    
    { status: 'success', analysis: analysis.to_h }
  end

  def check_quality(code)
    issues = []
    score = 100
    
    unless code.include?('# frozen_string_literal: true')
      issues << 'Missing frozen_string_literal pragma'
      score -= 10
    end
    
    if code.match?(/puts|p\s/)
      issues << 'Debug output statements found'
      score -= 10
    end
    
    if code.match?(/class\s+\w+\s*\n\s*def/)
      issues << 'Class lacks documentation'
      score -= 5
    end
    
    if code.match?(/rescue\s*\n\s*end/)
      issues << 'Empty rescue blocks - swallowing exceptions'
      score -= 15
    end
    
    if code.match?(/eval/)
      issues << 'eval() usage detected'
      score -= 25
    end
    
    {
      score: [score, 0].max,
      issues: issues,
      passed: score >= 70
    }
  end

  def find_issues(code)
    issues = []
    
    code.lines.each_with_index do |line, index|
      line_num = index + 1
      
      if line.match?(/eval/)
        issues << {
          severity: 'critical',
          message: 'eval() usage is dangerous',
          line: line_num
        }
      end
      
      if line.match?(/params\[:/)
        issues << {
          severity: 'high',
          message: 'Potential mass assignment vulnerability',
          line: line_num
        }
      end
      
      if line.match?(/\.html_safe/)
        issues << {
          severity: 'high',
          message: 'html_safe bypasses XSS protection',
          line: line_num
        }
      end
      
      if line.match?(/==/)
        issues << {
          severity: 'warning',
          message: 'Consider using .eql? or .equal? for clarity',
          line: line_num
        }
      end
    end
    
    critical = issues.select { |i| i[:severity] == 'critical' }
    
    {
      totalIssues: issues.length,
      issues: issues,
      critical: critical.length
    }
  end

  public

  def info
    FloorAgent.new(
      agent_id: @agent_id,
      name: @name,
      role: 'Operations Agent',
      office: 'Review Office',
      capabilities: @capabilities
    )
  end
end

class SecurityAgent
  ##
  # Security Agent - Performs security audits and vulnerability detection
  # Office: Security Office
  
  attr_reader :agent_id, :name, :capabilities

  def initialize(agent_id, name)
    @agent_id = agent_id
    @name = name
    @capabilities = [
      'vulnerability_scanning',
      'rails_security',
      'injection_detection',
      'authentication_review'
    ]
  end

  def audit_security(code)
    vulnerabilities = []
    
    if code.match?(/eval/)
      vulnerabilities << {
        type: 'code_injection',
        severity: 'critical',
        description: 'eval() usage - remote code execution risk'
      }
    end
    
    if code.match?(/system|exec|`/)
      vulnerabilities << {
        type: 'command_injection',
        severity: 'critical',
        description: 'Shell command execution detected'
      }
    end
    
    if code.match?(/\.html_safe/)
      vulnerabilities << {
        type: 'xss',
        severity: 'high',
        description: 'html_safe bypasses XSS protection'
      }
    end
    
    if code.match?(/params\[:\w+\]\.permit!/)
      vulnerabilities << {
        type: 'mass_assignment',
        severity: 'high',
        description: 'Mass assignment with permit! is dangerous'
      }
    end
    
    if code.match?(/password.*=.*['"][^'"]+['"]/)
      vulnerabilities << {
        type: 'hardcoded_credentials',
        severity: 'critical',
        description: 'Hardcoded password detected'
      }
    end
    
    if code.match?(/send\(params/)
      vulnerabilities << {
        type: 'arbitrary_method',
        severity: 'critical',
        description: 'Arbitrary method invocation with user input'
      }
    end
    
    if code.match?(/File\.read|File\.open.*params/)
      vulnerabilities << {
        type: 'path_traversal',
        severity: 'high',
        description: 'File access with user input'
      }
    end
    
    critical = vulnerabilities.select { |v| v[:severity] == 'critical' }
    high = vulnerabilities.select { |v| v[:severity] == 'high' }
    
    {
      secure: vulnerabilities.empty?,
      vulnerabilities: vulnerabilities,
      criticalCount: critical.length,
      highCount: high.length
    }
  end

  def info
    FloorAgent.new(
      agent_id: @agent_id,
      name: @name,
      role: 'Security Agent',
      office: 'Security Office',
      capabilities: @capabilities
    )
  end
end

class TestAgent
  ##
  # Test Agent - Handles RSpec, Minitest, and test analysis
  # Office: Test Office
  
  attr_reader :agent_id, :name, :capabilities

  def initialize(agent_id, name)
    @agent_id = agent_id
    @name = name
    @capabilities = [
      'rspec_testing',
      'minitest_support',
      'test_coverage',
      'factory_management'
    ]
  end

  def analyze_tests(code)
    frameworks = {
      rspec: code.match?(/describe|context|it\s+['"]/) || code.include?('RSpec'),
      minitest: code.match?(/class\s+\w+Test|assert_/) || code.include?('Minitest'),
      test_unit: code.match?(/def\s+test_/)
    }
    
    assertions = [
      code.scan(/expect\(/).length,
      code.scan(/assert_/).length,
      code.scan(/should\s+/).length
    ].sum
    
    {
      hasTests: frameworks.values.any?,
      frameworks: frameworks.select { |_, v| v }.keys,
      assertionCount: assertions,
      coverage: 'unknown'
    }
  end

  def info
    FloorAgent.new(
      agent_id: @agent_id,
      name: @name,
      role: 'Test Agent',
      office: 'Test Office',
      capabilities: @capabilities
    )
  end
end

class ManagerAgent
  ##
  # Manager Agent - Coordinates tasks and manages workflow
  # Office: Manager Office
  
  attr_reader :agent_id, :name, :capabilities

  def initialize(agent_id, name)
    @agent_id = agent_id
    @name = name
    @capabilities = [
      'task_management',
      'workflow_coordination',
      'resource_allocation',
      'progress_tracking'
    ]
    @tasks = {}
  end

  def create_task(task_id, title, assigned_to)
    task = Task.new(
      task_id: task_id,
      title: title,
      status: 'pending',
      assigned_to: assigned_to,
      created_at: Time.now.utc.iso8601
    )
    @tasks[task_id] = task
    task
  end

  def update_task_status(task_id, status)
    task = @tasks[task_id]
    raise "Task not found: #{task_id}" unless task
    
    task.status = status
    task.completed_at = Time.now.utc.iso8601 if status == 'completed'
    task
  end

  def tasks
    @tasks.values
  end

  def info
    FloorAgent.new(
      agent_id: @agent_id,
      name: @name,
      role: 'Manager Agent',
      office: 'Manager Office',
      capabilities: @capabilities
    )
  end
end

# ============================================================================
# MAIN DEPARTMENT FLOOR CLASS
# ============================================================================

class RubyDepartmentFloor
  ##
  # Ruby Department Floor - Floor 16
  #
  # Implements department logic following:
  # - Language Sovereignty: All code in Ruby
  # - Identical Internal Topology: 6 offices
  # - Contract-Bound Operation: JSON-RPC protocol
  # - Non-Creative Mandate: Strict adherence to requests
  # - Failure Escalation Guarantee: Explicit error handling

  FLOOR_NUMBER = 16
  LANGUAGE = 'ruby'
  DOMAIN = 'Scripting, Web frameworks (Rails/Sinatra), Developer tools'
  OFFICES = [
    'Architecture Office',
    'Implementation Office',
    'Review Office',
    'Test Office',
    'Security Office',
    'Manager Office'
  ].freeze

  def initialize
    @service_agent = ServiceAgent.new('service-001', 'Ruby Service Agent')
    @data_model_agent = DataModelAgent.new('data-001', 'Ruby Data Agent')
    @operations_agent = OperationsAgent.new('ops-001', 'Ruby Operations Agent')
    @security_agent = SecurityAgent.new('security-001', 'Ruby Security Agent')
    @test_agent = TestAgent.new('test-001', 'Ruby Test Agent')
    @manager_agent = ManagerAgent.new('manager-001', 'Ruby Manager Agent')
  end

  def floor_info
    agents = [
      @service_agent.info.to_h,
      @data_model_agent.info.to_h,
      @operations_agent.info.to_h,
      @security_agent.info.to_h,
      @test_agent.info.to_h,
      @manager_agent.info.to_h
    ]

    {
      floorNumber: FLOOR_NUMBER,
      language: LANGUAGE,
      domain: DOMAIN,
      offices: OFFICES,
      agentCount: agents.length,
      agents: agents,
      tasks: @manager_agent.tasks.map(&:to_h),
      architecturalLaw: 'Idiomatic Ruby, SOLID principles, Duck typing',
      securityDoctrine: 'Input sanitization, SQL injection prevention, Mass assignment protection'
    }
  end

  def process_code(code, operation)
    case operation
    when 'analyze'
      @operations_agent.execute_operation('analyze', 'code' => code)
    when 'security_audit'
      @security_agent.audit_security(code)
    when 'check_quality'
      @operations_agent.execute_operation('check_quality', 'code' => code)
    when 'find_issues'
      @operations_agent.execute_operation('find_issues', 'code' => code)
    when 'analyze_tests'
      @test_agent.analyze_tests(code)
    else
      raise "Unknown code operation: #{operation}"
    end
  end

  def handle_request(request)
    method = request['method'] || raise('Method required')
    params = request['params'] || {}
    request_id = request['id']

    result = case method
             when 'get_info'
               floor_info
             when 'process_code'
               process_code(params['code'], params['operation'])
             when 'create_task'
               @manager_agent.create_task(
                 params['taskId'],
                 params['title'],
                 params['assignedTo']
               ).to_h
             when 'update_task'
               @manager_agent.update_task_status(
                 params['taskId'],
                 params['status']
               ).to_h
             when 'execute_service'
               @service_agent.execute_service(
                 params['serviceName'],
                 params['params']
               )
             when 'process_data'
               @data_model_agent.process_data(
                 params['operation'],
                 params['data']
               )
             else
               raise "Unknown method: #{method}"
             end

    { result: result, id: request_id }
  rescue StandardError => e
    {
      error: {
        code: -32603,
        message: e.message
      },
      id: request_id
    }
  end
end

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================

def main
  floor = RubyDepartmentFloor.new

  $stderr.puts 'Ruby Department Floor (Floor 16) - Ready'
  $stderr.puts "Domain: #{floor.floor_info[:domain]}"
  $stderr.puts "Offices: #{floor.floor_info[:offices].join(', ')}"

  $stdin.each_line do |line|
    line = line.strip
    next if line.empty?

    begin
      request = JSON.parse(line)
      response = floor.handle_request(request)
      puts JSON.generate(response)
    rescue JSON::ParserError => e
      error_response = {
        error: {
          code: -32700,
          message: "Parse error: #{e.message}"
        }
      }
      puts JSON.generate(error_response)
    end

    $stdout.flush
  end
end

main if __FILE__ == $PROGRAM_NAME
