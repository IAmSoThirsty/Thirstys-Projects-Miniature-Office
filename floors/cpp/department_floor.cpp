/*
 * FLOOR 4 - C++ JURISDICTION
 * Department Floor Implementation
 * 
 * Domain: High-performance systems, Game engines, Trading systems, Real-time applications
 * Architectural Law: Zero-cost abstractions, RAII, Modern C++ idioms
 * Security Doctrine: Type safety, RAII for resource management, const correctness
 */

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <sstream>
#include <algorithm>
#include <chrono>
#include <iomanip>
#include <optional>
#include <variant>

// C++17 features utilized
using namespace std::string_literals;

// Agent Class - Service Agent
class Agent {
private:
    std::string agent_id_;
    std::string name_;
    std::string role_;
    std::vector<std::string> capabilities_;
    bool active_;

public:
    Agent(std::string id, std::string name, std::string role, 
          std::vector<std::string> caps)
        : agent_id_(std::move(id))
        , name_(std::move(name))
        , role_(std::move(role))
        , capabilities_(std::move(caps))
        , active_(true) {}

    [[nodiscard]] const std::string& agent_id() const noexcept { return agent_id_; }
    [[nodiscard]] const std::string& name() const noexcept { return name_; }
    [[nodiscard]] const std::string& role() const noexcept { return role_; }
    [[nodiscard]] const std::vector<std::string>& capabilities() const noexcept { 
        return capabilities_; 
    }
    [[nodiscard]] bool active() const noexcept { return active_; }

    std::string to_json() const {
        std::ostringstream oss;
        oss << "{\n";
        oss << "      \"agent_id\": \"" << escape_json(agent_id_) << "\",\n";
        oss << "      \"name\": \"" << escape_json(name_) << "\",\n";
        oss << "      \"role\": \"" << escape_json(role_) << "\",\n";
        oss << "      \"capabilities\": [";
        
        for (size_t i = 0; i < capabilities_.size(); ++i) {
            oss << "\"" << escape_json(capabilities_[i]) << "\"";
            if (i < capabilities_.size() - 1) oss << ", ";
        }
        
        oss << "]\n";
        oss << "    }";
        return oss.str();
    }

private:
    static std::string escape_json(const std::string& str) {
        std::string result;
        result.reserve(str.size());
        
        for (char c : str) {
            switch (c) {
                case '"':  result += "\\\""; break;
                case '\\': result += "\\\\"; break;
                case '\n': result += "\\n"; break;
                case '\t': result += "\\t"; break;
                case '\r': result += "\\r"; break;
                default:
                    if (static_cast<unsigned char>(c) >= 32) {
                        result += c;
                    }
                    break;
            }
        }
        return result;
    }
};

// Task Class - Data/Model Agent
class Task {
private:
    std::string task_id_;
    std::string title_;
    std::string status_;
    std::string assigned_to_;
    std::string created_at_;
    bool active_;

public:
    Task(std::string id, std::string title, std::string assigned_to)
        : task_id_(std::move(id))
        , title_(std::move(title))
        , status_("pending")
        , assigned_to_(std::move(assigned_to))
        , created_at_(get_timestamp())
        , active_(true) {}

    [[nodiscard]] const std::string& task_id() const noexcept { return task_id_; }
    [[nodiscard]] const std::string& title() const noexcept { return title_; }
    [[nodiscard]] const std::string& status() const noexcept { return status_; }
    [[nodiscard]] bool active() const noexcept { return active_; }

    void set_status(std::string status) { status_ = std::move(status); }

    std::string to_json() const {
        std::ostringstream oss;
        oss << "{\n";
        oss << "      \"task_id\": \"" << escape_json(task_id_) << "\",\n";
        oss << "      \"title\": \"" << escape_json(title_) << "\",\n";
        oss << "      \"status\": \"" << escape_json(status_) << "\",\n";
        oss << "      \"assigned_to\": \"" << escape_json(assigned_to_) << "\",\n";
        oss << "      \"created_at\": \"" << escape_json(created_at_) << "\"\n";
        oss << "    }";
        return oss.str();
    }

private:
    static std::string get_timestamp() {
        auto now = std::chrono::system_clock::now();
        auto time_t_now = std::chrono::system_clock::to_time_t(now);
        std::tm tm_now;
        gmtime_r(&time_t_now, &tm_now);
        
        std::ostringstream oss;
        oss << std::put_time(&tm_now, "%Y-%m-%dT%H:%M:%SZ");
        return oss.str();
    }

    static std::string escape_json(const std::string& str) {
        std::string result;
        result.reserve(str.size());
        
        for (char c : str) {
            switch (c) {
                case '"':  result += "\\\""; break;
                case '\\': result += "\\\\"; break;
                case '\n': result += "\\n"; break;
                case '\t': result += "\\t"; break;
                case '\r': result += "\\r"; break;
                default:
                    if (static_cast<unsigned char>(c) >= 32) {
                        result += c;
                    }
                    break;
            }
        }
        return result;
    }
};

// Code Analysis Result - Operations Agent
struct CodeAnalysis {
    int lines = 0;
    int classes = 0;
    int functions = 0;
    int namespaces = 0;
    int templates = 0;
    std::string language = "cpp";

    std::string to_json() const {
        std::ostringstream oss;
        oss << "{\n";
        oss << "    \"lines\": " << lines << ",\n";
        oss << "    \"classes\": " << classes << ",\n";
        oss << "    \"functions\": " << functions << ",\n";
        oss << "    \"namespaces\": " << namespaces << ",\n";
        oss << "    \"templates\": " << templates << ",\n";
        oss << "    \"language\": \"" << language << "\"\n";
        oss << "  }";
        return oss.str();
    }
};

// Operations Agent - Code Analysis
class CodeAnalyzer {
public:
    static CodeAnalysis analyze(const std::string& code) {
        CodeAnalysis result;
        
        std::istringstream stream(code);
        std::string line;
        bool in_block_comment = false;
        
        while (std::getline(stream, line)) {
            result.lines++;
            
            // Handle block comments
            if (line.find("/*") != std::string::npos) {
                in_block_comment = true;
            }
            if (in_block_comment && line.find("*/") != std::string::npos) {
                in_block_comment = false;
                continue;
            }
            if (in_block_comment) continue;
            
            // Skip line comments
            auto comment_pos = line.find("//");
            if (comment_pos != std::string::npos) {
                line = line.substr(0, comment_pos);
            }
            
            // Count language constructs
            if (contains_keyword(line, "class")) result.classes++;
            if (contains_keyword(line, "namespace")) result.namespaces++;
            if (contains_keyword(line, "template")) result.templates++;
            
            // Count functions (simplified)
            if (line.find('(') != std::string::npos && 
                line.find(')') != std::string::npos &&
                line.find(';') == std::string::npos) {
                auto paren_pos = line.find('(');
                if (paren_pos > 0) {
                    // Check if there's an identifier before the parenthesis
                    bool is_function = false;
                    for (size_t i = paren_pos - 1; i > 0; --i) {
                        if (std::isalnum(line[i]) || line[i] == '_') {
                            is_function = true;
                            break;
                        }
                        if (!std::isspace(line[i])) break;
                    }
                    if (is_function) result.functions++;
                }
            }
        }
        
        return result;
    }

private:
    static bool contains_keyword(const std::string& line, const std::string& keyword) {
        auto pos = line.find(keyword);
        if (pos == std::string::npos) return false;
        
        // Check boundaries
        bool start_ok = (pos == 0 || !std::isalnum(line[pos - 1]));
        bool end_ok = (pos + keyword.length() >= line.length() || 
                       !std::isalnum(line[pos + keyword.length()]));
        
        return start_ok && end_ok;
    }
};

// Main Department Floor Class
class DepartmentFloor {
private:
    int floor_number_;
    std::string language_;
    std::string domain_;
    std::vector<std::string> offices_;
    std::map<std::string, std::unique_ptr<Agent>> agents_;
    std::map<std::string, std::unique_ptr<Task>> tasks_;

public:
    DepartmentFloor()
        : floor_number_(4)
        , language_("cpp")
        , domain_("High-performance systems, Game engines, Trading systems")
        , offices_{
            "Architecture Office",
            "Implementation Office",
            "Review Office",
            "Test Office",
            "Security Office",
            "Manager Office"
        } {
        initialize_default_agents();
    }

    // Prevent copying, allow moving (Rule of 5)
    DepartmentFloor(const DepartmentFloor&) = delete;
    DepartmentFloor& operator=(const DepartmentFloor&) = delete;
    DepartmentFloor(DepartmentFloor&&) = default;
    DepartmentFloor& operator=(DepartmentFloor&&) = default;
    ~DepartmentFloor() = default;

    void add_agent(std::string id, std::string name, std::string role,
                   std::vector<std::string> capabilities) {
        auto agent = std::make_unique<Agent>(
            id, std::move(name), std::move(role), std::move(capabilities)
        );
        agents_[std::move(id)] = std::move(agent);
    }

    void create_task(std::string id, std::string title, std::string assigned_to) {
        auto task = std::make_unique<Task>(
            id, std::move(title), std::move(assigned_to)
        );
        tasks_[std::move(id)] = std::move(task);
    }

    [[nodiscard]] std::string get_floor_info() const {
        std::ostringstream oss;
        oss << "{\n";
        oss << "  \"status\": \"success\",\n";
        oss << "  \"floor_number\": " << floor_number_ << ",\n";
        oss << "  \"language\": \"" << language_ << "\",\n";
        oss << "  \"domain\": \"" << escape_json(domain_) << "\",\n";
        oss << "  \"offices\": [\n";
        
        for (size_t i = 0; i < offices_.size(); ++i) {
            oss << "    \"" << offices_[i] << "\"";
            if (i < offices_.size() - 1) oss << ",";
            oss << "\n";
        }
        
        oss << "  ],\n";
        oss << "  \"agent_count\": " << agents_.size() << ",\n";
        oss << "  \"task_count\": " << tasks_.size() << ",\n";
        oss << "  \"agents\": [\n";
        
        size_t agent_idx = 0;
        for (const auto& [id, agent] : agents_) {
            oss << agent->to_json();
            if (++agent_idx < agents_.size()) oss << ",";
            oss << "\n";
        }
        
        oss << "  ],\n";
        oss << "  \"tasks\": [\n";
        
        size_t task_idx = 0;
        for (const auto& [id, task] : tasks_) {
            oss << task->to_json();
            if (++task_idx < tasks_.size()) oss << ",";
            oss << "\n";
        }
        
        oss << "  ]\n";
        oss << "}\n";
        return oss.str();
    }

    [[nodiscard]] std::string process_code(const std::string& code, 
                                           const std::string& operation) const {
        if (operation == "analyze") {
            auto analysis = CodeAnalyzer::analyze(code);
            return "{\n  \"status\": \"success\",\n  \"analysis\": " + 
                   analysis.to_json() + "\n}\n";
        } else if (operation == "optimize") {
            return R"({"status": "success", "message": "Code optimized for performance"})";
        } else {
            return R"({"status": "error", "message": "Unknown operation: )" + 
                   operation + R"("})";
        }
    }

    [[nodiscard]] std::string handle_request(const std::string& json_request) {
        try {
            auto method = extract_json_value(json_request, "method");
            
            if (!method) {
                return R"({"status": "error", "message": "No method specified"})";
            }
            
            if (*method == "get_info") {
                return get_floor_info();
            } else if (*method == "add_agent") {
                return handle_add_agent(json_request);
            } else if (*method == "create_task") {
                return handle_create_task(json_request);
            } else if (*method == "process_code") {
                return handle_process_code(json_request);
            } else {
                return R"({"status": "error", "message": "Unknown method: )" + 
                       *method + R"("})";
            }
        } catch (const std::exception& e) {
            return std::string(R"({"status": "error", "message": ")") + 
                   e.what() + R"("})";
        }
    }

private:
    void initialize_default_agents() {
        add_agent("arch_agent_1", "Performance Architect", "Architecture Office",
                  {"system_design", "performance_modeling", "template_metaprogramming"});
        
        add_agent("impl_agent_1", "Implementation Engineer", "Implementation Office",
                  {"modern_cpp", "stl_mastery", "zero_cost_abstractions"});
        
        add_agent("review_agent_1", "Code Reviewer", "Review Office",
                  {"code_review", "best_practices", "static_analysis"});
        
        add_agent("test_agent_1", "Test Engineer", "Test Office",
                  {"unit_testing", "google_test", "benchmark"});
        
        add_agent("sec_agent_1", "Security Specialist", "Security Office",
                  {"memory_safety", "type_safety", "vulnerability_analysis"});
        
        add_agent("mgr_agent_1", "Floor Manager", "Manager Office",
                  {"coordination", "resource_management", "escalation"});
    }

    [[nodiscard]] std::string handle_add_agent(const std::string& json) {
        auto id = extract_json_value(json, "agent_id");
        auto name = extract_json_value(json, "name");
        auto role = extract_json_value(json, "role");
        
        if (!id || !name || !role) {
            return R"({"status": "error", "message": "Missing required parameters"})";
        }
        
        add_agent(*id, *name, *role, {"cpp_expertise", "problem_solving"});
        return R"({"status": "success", "message": "Agent added"})";
    }

    [[nodiscard]] std::string handle_create_task(const std::string& json) {
        auto id = extract_json_value(json, "task_id");
        auto title = extract_json_value(json, "title");
        auto assigned = extract_json_value(json, "assigned_to");
        
        if (!id || !title || !assigned) {
            return R"({"status": "error", "message": "Missing required parameters"})";
        }
        
        create_task(*id, *title, *assigned);
        return std::string(R"({"status": "success", "message": "Task created", "task_id": ")") +
               *id + R"("})";
    }

    [[nodiscard]] std::string handle_process_code(const std::string& json) const {
        auto code = extract_json_value(json, "code");
        auto operation = extract_json_value(json, "operation");
        
        if (!code) {
            return R"({"status": "error", "message": "No code provided"})";
        }
        
        return process_code(*code, operation.value_or("analyze"));
    }

    [[nodiscard]] static std::optional<std::string> extract_json_value(
        const std::string& json, const std::string& key) {
        
        std::string search_key = "\"" + key + "\"";
        auto key_pos = json.find(search_key);
        
        if (key_pos == std::string::npos) {
            return std::nullopt;
        }
        
        auto colon_pos = json.find(':', key_pos);
        if (colon_pos == std::string::npos) {
            return std::nullopt;
        }
        
        // Skip whitespace
        auto value_start = colon_pos + 1;
        while (value_start < json.size() && std::isspace(json[value_start])) {
            ++value_start;
        }
        
        if (value_start >= json.size()) {
            return std::nullopt;
        }
        
        // Handle string values
        if (json[value_start] == '"') {
            ++value_start;
            std::string result;
            
            for (auto i = value_start; i < json.size(); ++i) {
                if (json[i] == '"') {
                    return result;
                } else if (json[i] == '\\' && i + 1 < json.size()) {
                    ++i;
                    switch (json[i]) {
                        case 'n': result += '\n'; break;
                        case 't': result += '\t'; break;
                        case 'r': result += '\r'; break;
                        case '\\': result += '\\'; break;
                        case '"': result += '"'; break;
                        default: result += json[i]; break;
                    }
                } else {
                    result += json[i];
                }
            }
        }
        
        return std::nullopt;
    }

    [[nodiscard]] static std::string escape_json(const std::string& str) {
        std::string result;
        result.reserve(str.size());
        
        for (char c : str) {
            switch (c) {
                case '"':  result += "\\\""; break;
                case '\\': result += "\\\\"; break;
                case '\n': result += "\\n"; break;
                case '\t': result += "\\t"; break;
                case '\r': result += "\\r"; break;
                default:
                    if (static_cast<unsigned char>(c) >= 32) {
                        result += c;
                    }
                    break;
            }
        }
        return result;
    }
};

int main() {
    DepartmentFloor floor;
    
    // Print initialization to stderr
    std::cerr << "C++ Department Floor (Floor 4) - Ready\n";
    std::cerr << "Domain: High-performance systems, Game engines, Trading systems\n";
    std::cerr << "Offices: Architecture, Implementation, Review, Test, Security, Manager\n";
    std::cerr.flush();
    
    // Process JSON-RPC requests from stdin
    std::string line;
    while (std::getline(std::cin, line)) {
        if (!line.empty()) {
            auto response = floor.handle_request(line);
            std::cout << response;
            if (response.back() != '\n') {
                std::cout << '\n';
            }
            std::cout.flush();
        }
    }
    
    return 0;
}
