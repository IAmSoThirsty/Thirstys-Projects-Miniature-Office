(* 
 * Floor 21 - OCaml Jurisdiction
 * Department Floor Implementation
 * 
 * Domain: Functional systems, Compilers, Type-safe systems
 * Architectural Law: Functional purity, Immutability, Type safety
 * Security Doctrine: Type safety, No null references, Pattern matching exhaustiveness
 *)

open Yojson.Basic.Util

(* ============================================================================
   CORE DATA MODELS
   ============================================================================ *)

type floor_agent = {
  agent_id: string;
  name: string;
  role: string;
  office: string;
  capabilities: string list;
}

type task_status = Pending | InProgress | Completed | Failed

type task = {
  task_id: string;
  title: string;
  status: task_status;
  assigned_to: string;
  created_at: float;
  completed_at: float option;
}

type code_analysis = {
  lines: int;
  modules: int;
  functions: int;
  types: int;
  variants: int;
  language: string;
}

(* Conversion functions *)
let status_to_string = function
  | Pending -> "pending"
  | InProgress -> "in_progress"
  | Completed -> "completed"
  | Failed -> "failed"

let agent_to_json agent =
  `Assoc [
    ("agentId", `String agent.agent_id);
    ("name", `String agent.name);
    ("role", `String agent.role);
    ("office", `String agent.office);
    ("capabilities", `List (List.map (fun c -> `String c) agent.capabilities));
  ]

let task_to_json task =
  let completed_json = match task.completed_at with
    | Some t -> `Float t
    | None -> `Null
  in
  `Assoc [
    ("taskId", `String task.task_id);
    ("title", `String task.title);
    ("status", `String (status_to_string task.status));
    ("assignedTo", `String task.assigned_to);
    ("createdAt", `Float task.created_at);
    ("completedAt", completed_json);
  ]

let analysis_to_json analysis =
  `Assoc [
    ("lines", `Int analysis.lines);
    ("modules", `Int analysis.modules);
    ("functions", `Int analysis.functions);
    ("types", `Int analysis.types);
    ("variants", `Int analysis.variants);
    ("language", `String analysis.language);
  ]

(* ============================================================================
   AGENT MODULES - Production Grade Implementations
   ============================================================================ *)

module ServiceAgent = struct
  type t = {
    agent_id: string;
    name: string;
    capabilities: string list;
  }

  let create agent_id name = {
    agent_id;
    name;
    capabilities = [
      "functional_services";
      "type_inference";
      "pattern_matching";
      "module_systems";
    ];
  }

  let validate_style code =
    let issues = ref [] in
    
    (* Check for proper module signature *)
    if String.contains code ':' && 
       (try ignore (Str.search_forward (Str.regexp "module.*:.*sig") code 0); false with Not_found -> true) then
      issues := "Use module signatures for type abstraction" :: !issues;
    
    (* Check for mutable references *)
    if (try ignore (Str.search_forward (Str.regexp "ref\\s+") code 0); true with Not_found -> false) then
      issues := "Prefer immutable data structures over refs" :: !issues;
    
    (* Check for proper naming *)
    if (try ignore (Str.search_forward (Str.regexp "let\\s+[A-Z]") code 0); true with Not_found -> false) then
      issues := "Use snake_case for function names, not CamelCase" :: !issues;
    
    let valid = List.length !issues = 0 in
    `Assoc [
      ("valid", `Bool valid);
      ("issues", `List (List.map (fun i -> `String i) !issues));
      ("issueCount", `Int (List.length !issues));
    ]

  let check_functional_patterns code =
    let patterns = ref [] in
    
    if String.contains code '|' then
      patterns := "Pattern matching" :: !patterns;
    
    if (try ignore (Str.search_forward (Str.regexp "List\\.\\(map\\|fold\\|filter\\)") code 0); true with Not_found -> false) then
      patterns := "Higher-order functions" :: !patterns;
    
    if (try ignore (Str.search_forward (Str.regexp "match.*with") code 0); true with Not_found -> false) then
      patterns := "Algebraic data types" :: !patterns;
    
    if (try ignore (Str.search_forward (Str.regexp "let\\s+rec\\s+") code 0); true with Not_found -> false) then
      patterns := "Recursive functions" :: !patterns;
    
    if (try ignore (Str.search_forward (Str.regexp "fun\\s+\\|->") code 0); true with Not_found -> false) then
      patterns := "Anonymous functions" :: !patterns;
    
    `Assoc [
      ("patterns", `List (List.map (fun p -> `String p) !patterns));
      ("patternCount", `Int (List.length !patterns));
    ]

  let analyze_types code =
    let type_features = ref [] in
    
    if (try ignore (Str.search_forward (Str.regexp "type.*=") code 0); true with Not_found -> false) then
      type_features := "Type definitions" :: !type_features;
    
    if (try ignore (Str.search_forward (Str.regexp "type.*'[a-z]") code 0); true with Not_found -> false) then
      type_features := "Polymorphic types" :: !type_features;
    
    if (try ignore (Str.search_forward (Str.regexp "module\\s+type") code 0); true with Not_found -> false) then
      type_features := "Module types" :: !type_features;
    
    if (try ignore (Str.search_forward (Str.regexp "\\[[^]]*\\]\\|<[^>]*>") code 0); true with Not_found -> false) then
      type_features := "Type annotations" :: !type_features;
    
    `Assoc [
      ("typeFeatures", `List (List.map (fun f -> `String f) !type_features));
      ("featureCount", `Int (List.length !type_features));
    ]

  let execute_service service_name params =
    match service_name with
    | "validate_style" ->
        let code = params |> member "code" |> to_string in
        Ok (validate_style code)
    | "check_functional_patterns" ->
        let code = params |> member "code" |> to_string in
        Ok (check_functional_patterns code)
    | "analyze_types" ->
        let code = params |> member "code" |> to_string in
        Ok (analyze_types code)
    | _ -> Error (Printf.sprintf "Unknown service: %s" service_name)

  let get_info t =
    {
      agent_id = t.agent_id;
      name = t.name;
      role = "Service Agent";
      office = "Implementation Office";
      capabilities = t.capabilities;
    }
end

module DataModelAgent = struct
  type t = {
    agent_id: string;
    name: string;
    capabilities: string list;
  }

  let create agent_id name = {
    agent_id;
    name;
    capabilities = [
      "algebraic_types";
      "variant_types";
      "record_types";
      "type_inference";
    ];
  }

  let validate_data data =
    let json_type = match data with
      | `Null -> "null"
      | `Bool _ -> "bool"
      | `Int _ -> "int"
      | `Float _ -> "float"
      | `String _ -> "string"
      | `List _ -> "list"
      | `Assoc _ -> "assoc"
      | _ -> "unknown"
    in
    `Assoc [
      ("valid", `Bool true);
      ("type", `String json_type);
      ("isNull", `Bool (data = `Null));
    ]

  let infer_variant_type data =
    match data with
    | `String s when String.length s > 0 && Char.uppercase_ascii s.[0] = s.[0] ->
        `Assoc [
          ("inferredType", `String "variant_constructor");
          ("constructor", `String s);
        ]
    | `List items ->
        let types = List.map (fun item ->
          match item with
          | `String _ -> "string"
          | `Int _ -> "int"
          | `Float _ -> "float"
          | _ -> "unknown"
        ) items in
        `Assoc [
          ("inferredType", `String "list");
          ("elementTypes", `List (List.map (fun t -> `String t) types));
        ]
    | _ ->
        `Assoc [
          ("inferredType", `String "primitive");
        ]

  let transform_to_variant data =
    match data with
    | `Assoc fields ->
        let tag = List.assoc "tag" fields |> to_string_option in
        let value = List.assoc "value" fields in
        begin match tag with
        | Some t ->
            `Assoc [
              ("transformed", `Bool true);
              ("variant", `String t);
              ("data", value);
            ]
        | None ->
            `Assoc [
              ("transformed", `Bool false);
              ("error", `String "No tag field for variant");
            ]
        end
    | _ ->
        `Assoc [
          ("transformed", `Bool false);
          ("error", `String "Expected object with tag field");
        ]

  let process_data operation data =
    match operation with
    | "validate" -> Ok (validate_data data)
    | "infer_variant" -> Ok (infer_variant_type data)
    | "transform_to_variant" -> Ok (transform_to_variant data)
    | _ -> Error (Printf.sprintf "Unknown operation: %s" operation)

  let get_info t =
    {
      agent_id = t.agent_id;
      name = t.name;
      role = "Data Model Agent";
      office = "Architecture Office";
      capabilities = t.capabilities;
    }
end

module OperationsAgent = struct
  type t = {
    agent_id: string;
    name: string;
    capabilities: string list;
  }

  let create agent_id name = {
    agent_id;
    name;
    capabilities = [
      "code_analysis";
      "type_checking";
      "pattern_exhaustiveness";
      "purity_analysis";
    ];
  }

  let count_occurrences pattern str =
    let regex = Str.regexp pattern in
    let rec count pos acc =
      try
        let _ = Str.search_forward regex str pos in
        count (Str.match_end ()) (acc + 1)
      with Not_found -> acc
    in
    count 0 0

  let analyze_code code =
    let lines = List.length (String.split_on_char '\n' code) in
    let modules = count_occurrences "module\\s+" code in
    let functions = count_occurrences "let\\s+\\(rec\\s+\\)?[a-z_][a-zA-Z0-9_]*" code in
    let types = count_occurrences "type\\s+[a-z_]" code in
    let variants = count_occurrences "\\s*|\\s*[A-Z]" code in
    
    {
      lines;
      modules;
      functions;
      types;
      variants;
      language = "ocaml";
    }

  let check_quality code =
    let issues = ref [] in
    let score = ref 100 in
    
    (* Check for proper module structure *)
    if not ((try ignore (Str.search_forward (Str.regexp "open\\|module") code 0); true with Not_found -> false)) then begin
      issues := "Missing module organization" :: !issues;
      score := !score - 10
    end;
    
    (* Check for type annotations *)
    if (try ignore (Str.search_forward (Str.regexp "let\\s+") code 0); true with Not_found -> false) && 
       not (try ignore (Str.search_forward (Str.regexp ":\\s*[a-z]") code 0); true with Not_found -> false) then begin
      issues := "Consider adding type annotations for clarity" :: !issues;
      score := !score - 5
    end;
    
    (* Check for pattern matching exhaustiveness markers *)
    if (try ignore (Str.search_forward (Str.regexp "match.*with") code 0); true with Not_found -> false) && 
       not (try ignore (Str.search_forward (Str.regexp "\\s*_\\s*->") code 0); true with Not_found -> false) then begin
      issues := "Ensure pattern matching is exhaustive" :: !issues;
      score := !score - 10
    end;
    
    (* Check for mutation *)
    if (try ignore (Str.search_forward (Str.regexp ":=\\|<-") code 0); true with Not_found -> false) then begin
      issues := "Mutation detected - prefer immutable data structures" :: !issues;
      score := !score - 15
    end;
    
    let final_score = max 0 !score in
    `Assoc [
      ("qualityScore", `Int final_score);
      ("issues", `List (List.map (fun i -> `String i) !issues));
      ("issueCount", `Int (List.length !issues));
    ]

  let get_info t =
    {
      agent_id = t.agent_id;
      name = t.name;
      role = "Operations Agent";
      office = "Review Office";
      capabilities = t.capabilities;
    }
end

module TestAgent = struct
  type t = {
    agent_id: string;
    name: string;
    capabilities: string list;
  }

  let create agent_id name = {
    agent_id;
    name;
    capabilities = [
      "alcotest_framework";
      "ounit_framework";
      "property_testing";
      "test_coverage";
    ];
  }

  let count_occurrences pattern str =
    let regex = Str.regexp pattern in
    let rec count pos acc =
      try
        let _ = Str.search_forward regex str pos in
        count (Str.match_end ()) (acc + 1)
      with Not_found -> acc
    in
    count 0 0

  let analyze_tests code =
    (* Count test cases - Alcotest *)
    let test_cases = count_occurrences "test_case\\s+" code in
    
    (* Count test suites *)
    let test_suites = count_occurrences "\\[\\s*\"" code in
    
    (* Count assertions *)
    let alcotest_checks = count_occurrences "Alcotest\\.check\\|Alcotest\\.(check_)" code in
    let ounit_asserts = count_occurrences "assert_\\(equal\\|raises\\|bool\\)" code in
    let assertions = alcotest_checks + ounit_asserts in
    
    (* Check for test framework *)
    let is_alcotest = String.contains code 'A' && Str.string_match (Str.regexp ".*Alcotest") code 0 in
    let is_ounit = Str.string_match (Str.regexp ".*OUnit") code 0 in
    
    `Assoc [
      ("testCases", `Int test_cases);
      ("testSuites", `Int test_suites);
      ("assertions", `Int assertions);
      ("isAlcotest", `Bool is_alcotest);
      ("isOUnit", `Bool is_ounit);
      ("hasTests", `Bool (test_cases > 0 || assertions > 0));
    ]

  let get_info t =
    {
      agent_id = t.agent_id;
      name = t.name;
      role = "Test Agent";
      office = "Test Office";
      capabilities = t.capabilities;
    }
end

module SecurityAgent = struct
  type t = {
    agent_id: string;
    name: string;
    capabilities: string list;
  }

  let create agent_id name = {
    agent_id;
    name;
    capabilities = [
      "type_safety_verification";
      "mutation_detection";
      "unsafe_operation_detection";
      "exception_handling";
    ];
  }

  let scan_security code =
    let vulnerabilities = ref [] in
    
    (* Unsafe operations *)
    if (try ignore (Str.search_forward (Str.regexp "Obj\\.magic") code 0); true with Not_found -> false) then
      vulnerabilities := `Assoc [
        ("severity", `String "CRITICAL");
        ("type", `String "type_safety_bypass");
        ("description", `String "Obj.magic bypasses type safety - extremely dangerous");
      ] :: !vulnerabilities;
    
    (* Unsafe string operations *)
    if (try ignore (Str.search_forward (Str.regexp "String\\.\\(get\\|set\\)\\s+.*unsafe") code 0); true with Not_found -> false) then
      vulnerabilities := `Assoc [
        ("severity", `String "HIGH");
        ("type", `String "unsafe_string_access");
        ("description", `String "Unsafe string operations can cause buffer overruns");
      ] :: !vulnerabilities;
    
    (* Mutable references in shared state *)
    if (try ignore (Str.search_forward (Str.regexp "let\\s+.*ref\\s+") code 0); true with Not_found -> false) && 
       (try ignore (Str.search_forward (Str.regexp "Thread") code 0); true with Not_found -> false) then
      vulnerabilities := `Assoc [
        ("severity", `String "HIGH");
        ("type", `String "race_condition");
        ("description", `String "Mutable references with threading can cause race conditions");
      ] :: !vulnerabilities;
    
    (* Unchecked exceptions *)
    if (try ignore (Str.search_forward (Str.regexp "raise\\s+") code 0); true with Not_found -> false) && 
       not (try ignore (Str.search_forward (Str.regexp "try.*with") code 0); true with Not_found -> false) then
      vulnerabilities := `Assoc [
        ("severity", `String "MEDIUM");
        ("type", `String "unchecked_exception");
        ("description", `String "Unhandled exceptions - use Result type or try/with");
      ] :: !vulnerabilities;
    
    (* External C bindings without validation *)
    if (try ignore (Str.search_forward (Str.regexp "external\\s+") code 0); true with Not_found -> false) then
      vulnerabilities := `Assoc [
        ("severity", `String "MEDIUM");
        ("type", `String "unsafe_ffi");
        ("description", `String "External C functions require careful validation");
      ] :: !vulnerabilities;
    
    (* Format string issues *)
    if (try ignore (Str.search_forward (Str.regexp "Printf\\.\\(sprintf\\|printf\\)\\s+[a-z]") code 0); true with Not_found -> false) then
      vulnerabilities := `Assoc [
        ("severity", `String "LOW");
        ("type", `String "format_string");
        ("description", `String "Ensure format strings match argument types");
      ] :: !vulnerabilities;
    
    let secure = List.length !vulnerabilities = 0 in
    `Assoc [
      ("secure", `Bool secure);
      ("vulnerabilities", `List !vulnerabilities);
      ("vulnerabilityCount", `Int (List.length !vulnerabilities));
    ]

  let get_info t =
    {
      agent_id = t.agent_id;
      name = t.name;
      role = "Security Agent";
      office = "Security Office";
      capabilities = t.capabilities;
    }
end

module ManagerAgent = struct
  type t = {
    agent_id: string;
    name: string;
    capabilities: string list;
  }

  let create agent_id name = {
    agent_id;
    name;
    capabilities = [
      "workflow_orchestration";
      "task_delegation";
      "resource_management";
      "priority_scheduling";
    ];
  }

  let manage_workflow params =
    let task_count = 
      try params |> member "taskCount" |> to_int 
      with _ -> 0
    in
    `Assoc [
      ("workflowStatus", `String "active");
      ("tasksManaged", `Int task_count);
      ("priorityQueue", `List []);
      ("resourceUtilization", `String "optimal");
    ]

  let get_info t =
    {
      agent_id = t.agent_id;
      name = t.name;
      role = "Manager Agent";
      office = "Manager Office";
      capabilities = t.capabilities;
    }
end

(* ============================================================================
   MAIN DEPARTMENT FLOOR CLASS
   ============================================================================ *)

module OCamlDepartmentFloor = struct
  type t = {
    floor_number: int;
    language: string;
    domain: string;
    mutable agents: (string, floor_agent) Hashtbl.t;
    mutable tasks: (string, task) Hashtbl.t;
    offices: string list;
    service_agent: ServiceAgent.t;
    data_model_agent: DataModelAgent.t;
    operations_agent: OperationsAgent.t;
    test_agent: TestAgent.t;
    security_agent: SecurityAgent.t;
    manager_agent: ManagerAgent.t;
  }

  let create () =
    let agents = Hashtbl.create 10 in
    let tasks = Hashtbl.create 10 in
    
    let service_agent = ServiceAgent.create "service-001" "FunctionalServiceAgent" in
    let data_model_agent = DataModelAgent.create "data-001" "AlgebraicTypeAgent" in
    let operations_agent = OperationsAgent.create "ops-001" "TypeCheckerAgent" in
    let test_agent = TestAgent.create "test-001" "AlcotestAgent" in
    let security_agent = SecurityAgent.create "sec-001" "TypeSafetyAgent" in
    let manager_agent = ManagerAgent.create "mgr-001" "WorkflowManager" in
    
    let floor = {
      floor_number = 21;
      language = "ocaml";
      domain = "Functional systems, Compilers, Type-safe systems";
      agents;
      tasks;
      offices = [
        "Architecture Office";
        "Implementation Office";
        "Review Office";
        "Test Office";
        "Security Office";
        "Manager Office";
      ];
      service_agent;
      data_model_agent;
      operations_agent;
      test_agent;
      security_agent;
      manager_agent;
    } in
    
    (* Register agents *)
    Hashtbl.add agents "service-001" (ServiceAgent.get_info service_agent);
    Hashtbl.add agents "data-001" (DataModelAgent.get_info data_model_agent);
    Hashtbl.add agents "ops-001" (OperationsAgent.get_info operations_agent);
    Hashtbl.add agents "test-001" (TestAgent.get_info test_agent);
    Hashtbl.add agents "sec-001" (SecurityAgent.get_info security_agent);
    Hashtbl.add agents "mgr-001" (ManagerAgent.get_info manager_agent);
    
    floor

  let add_agent floor agent_id name role office capabilities =
    try
      let agent = { agent_id; name; role; office; capabilities } in
      Hashtbl.add floor.agents agent_id agent;
      Ok (`Assoc [
        ("status", `String "success");
        ("agent", agent_to_json agent);
      ])
    with e ->
      Error (Printexc.to_string e)

  let create_task floor task_id title assigned_to =
    try
      let task = {
        task_id;
        title;
        status = Pending;
        assigned_to;
        created_at = Unix.time ();
        completed_at = None;
      } in
      Hashtbl.add floor.tasks task_id task;
      Ok (`Assoc [
        ("status", `String "success");
        ("task", task_to_json task);
      ])
    with e ->
      Error (Printexc.to_string e)

  let get_floor_info floor =
    let agent_list = Hashtbl.fold (fun _ agent acc -> (agent_to_json agent) :: acc) floor.agents [] in
    let task_list = Hashtbl.fold (fun _ task acc -> (task_to_json task) :: acc) floor.tasks [] in
    
    `Assoc [
      ("floorNumber", `Int floor.floor_number);
      ("language", `String floor.language);
      ("domain", `String floor.domain);
      ("offices", `List (List.map (fun o -> `String o) floor.offices));
      ("agentCount", `Int (Hashtbl.length floor.agents));
      ("taskCount", `Int (Hashtbl.length floor.tasks));
      ("agents", `List agent_list);
      ("tasks", `List task_list);
    ]

  let process_code floor code operation =
    try
      match operation with
      | "analyze" ->
          let analysis = OperationsAgent.analyze_code floor.operations_agent code in
          Ok (`Assoc [
            ("status", `String "success");
            ("analysis", analysis_to_json analysis);
          ])
      | "quality" ->
          let quality = OperationsAgent.check_quality floor.operations_agent code in
          Ok (`Assoc [
            ("status", `String "success");
            ("quality", quality);
          ])
      | "security" ->
          let security = SecurityAgent.scan_security floor.security_agent code in
          Ok (`Assoc [
            ("status", `String "success");
            ("security", security);
          ])
      | "test_analysis" ->
          let test_analysis = TestAgent.analyze_tests floor.test_agent code in
          Ok (`Assoc [
            ("status", `String "success");
            ("testAnalysis", test_analysis);
          ])
      | _ ->
          Error (Printf.sprintf "Unknown operation: %s" operation)
    with e ->
      Error (Printexc.to_string e)

  let handle_request floor request =
    try
      let method_name = request |> member "method" |> to_string in
      let params = request |> member "params" in
      
      match method_name with
      | "get_info" ->
          Ok (get_floor_info floor)
      | "add_agent" ->
          let agent_id = params |> member "agentId" |> to_string in
          let name = params |> member "name" |> to_string in
          let role = params |> member "role" |> to_string in
          let office = params |> member "office" |> to_string in
          let capabilities = params |> member "capabilities" |> to_list |> List.map to_string in
          add_agent floor agent_id name role office capabilities
      | "create_task" ->
          let task_id = params |> member "taskId" |> to_string in
          let title = params |> member "title" |> to_string in
          let assigned_to = params |> member "assignedTo" |> to_string in
          create_task floor task_id title assigned_to
      | "process_code" ->
          let code = params |> member "code" |> to_string in
          let operation = params |> member "operation" |> to_string in
          process_code floor code operation
      | "execute_service" ->
          let service_name = params |> member "serviceName" |> to_string in
          ServiceAgent.execute_service floor.service_agent service_name params
      | "process_data" ->
          let operation = params |> member "operation" |> to_string in
          let data = params |> member "data" in
          DataModelAgent.process_data floor.data_model_agent operation data
      | _ ->
          Error (Printf.sprintf "Unknown method: %s" method_name)
    with
    | Yojson.Basic.Util.Type_error (msg, _) ->
        Error (Printf.sprintf "Type error: %s" msg)
    | e ->
        Error (Printf.sprintf "Error: %s" (Printexc.to_string e))
end

(* ============================================================================
   MAIN ENTRY POINT - JSON-RPC SERVER
   ============================================================================ *)

let () =
  let floor = OCamlDepartmentFloor.create () in
  
  Printf.eprintf "OCaml Department Floor (Floor 21) - Ready\n%!";
  Printf.eprintf "Domain: %s\n%!" floor.domain;
  Printf.eprintf "Offices: %s\n%!" (String.concat ", " floor.offices);
  
  (* Process requests line by line from stdin *)
  try
    while true do
      let line = read_line () in
      try
        let request = Yojson.Basic.from_string line in
        let response = match OCamlDepartmentFloor.handle_request floor request with
          | Ok json -> json
          | Error msg -> `Assoc [("status", `String "error"); ("message", `String msg)]
        in
        let response_str = Yojson.Basic.to_string response in
        Printf.printf "%s\n%!" response_str
      with
      | Yojson.Json_error msg ->
          let error_response = `Assoc [
            ("status", `String "error");
            ("message", `String (Printf.sprintf "Invalid JSON: %s" msg));
          ] in
          Printf.printf "%s\n%!" (Yojson.Basic.to_string error_response)
      | e ->
          let error_response = `Assoc [
            ("status", `String "error");
            ("message", `String (Printf.sprintf "Error: %s" (Printexc.to_string e)));
          ] in
          Printf.printf "%s\n%!" (Yojson.Basic.to_string error_response)
    done
  with End_of_file -> ()
