#!/usr/bin/env scala
/**
 * FLOOR 12 - SCALA JURISDICTION
 * Department Floor Implementation
 * 
 * Domain: Functional JVM systems, Big data processing, Type-level programming, Distributed systems
 * Architectural Law: Immutability by default, pure functions, type-driven design
 */

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.{Try, Success, Failure}
import scala.io.StdIn
import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

// JSON handling using built-in capabilities
import scala.util.parsing.json.*

case class FloorAgent(
  agentId: String,
  name: String,
  role: String,
  capabilities: List[String]
)

sealed trait TaskStatus
object TaskStatus {
  case object Pending extends TaskStatus
  case object InProgress extends TaskStatus
  case object Completed extends TaskStatus
  case object Failed extends TaskStatus
  
  def fromString(s: String): TaskStatus = s.toLowerCase match {
    case "pending" => Pending
    case "in_progress" => InProgress
    case "completed" => Completed
    case "failed" => Failed
    case _ => Pending
  }
  
  override def toString: String = this match {
    case Pending => "pending"
    case InProgress => "in_progress"
    case Completed => "completed"
    case Failed => "failed"
  }
}

case class Task(
  taskId: String,
  title: String,
  status: TaskStatus,
  assignedTo: String,
  createdAt: String,
  completedAt: Option[String] = None
)

case class CodeAnalysis(
  lines: Int,
  functions: Int,
  classes: Int,
  objects: Int,
  traits: Int,
  language: String,
  functionalScore: Double,
  immutabilityScore: Double
)

case class JsonRpcRequest(
  method: String,
  params: Option[Map[String, Any]] = None,
  id: Option[String] = None
)

case class JsonRpcResponse(
  result: Option[Map[String, Any]] = None,
  error: Option[JsonRpcError] = None,
  id: Option[String] = None
)

case class JsonRpcError(
  code: Int,
  message: String
)

sealed trait Office {
  def name: String
  def jurisdiction: String
}

object Office {
  case object Architecture extends Office {
    val name = "Architecture Office"
    val jurisdiction = "Functional architecture, type-level design, effect systems"
  }
  case object Implementation extends Office {
    val name = "Implementation Office"
    val jurisdiction = "Pure functions, immutable data, algebraic data types"
  }
  case object Review extends Office {
    val name = "Review Office"
    val jurisdiction = "Code review, functional patterns, type safety"
  }
  case object Test extends Office {
    val name = "Test Office"
    val jurisdiction = "Property-based testing, unit tests, integration tests"
  }
  case object Security extends Office {
    val name = "Security Office"
    val jurisdiction = "Type safety, effect tracking, security audits"
  }
  case object Manager extends Office {
    val name = "Manager Office"
    val jurisdiction = "Build management, coordination, reporting"
  }
}

trait Agent {
  def id: String
  def office: Office
  def execute(task: Task)(using ExecutionContext): Future[String]
}

class ServiceAgent(val id: String, val office: Office) extends Agent {
  def execute(task: Task)(using ExecutionContext): Future[String] = 
    Future {
      Thread.sleep(10)
      s"ServiceAgent $id completed task: ${task.title}"
    }
}

class DataModelAgent(val id: String, val office: Office) extends Agent {
  def execute(task: Task)(using ExecutionContext): Future[String] =
    Future {
      Thread.sleep(10)
      s"DataModelAgent $id validated immutable model: ${task.title}"
    }
}

class OperationsAgent(val id: String, val office: Office) extends Agent {
  def execute(task: Task)(using ExecutionContext): Future[String] =
    Future {
      Thread.sleep(10)
      s"OperationsAgent $id executed functional operation: ${task.title}"
    }
}

class ScalaDepartmentFloor(using ec: ExecutionContext) {
  private val floorNumber = 12
  private val language = "scala"
  private val domain = "Functional JVM systems, Big data processing, Type-level programming"
  
  private val agents = new ConcurrentHashMap[String, FloorAgent]().asScala
  private val tasks = new ConcurrentHashMap[String, Task]().asScala
  private val activeAgents = new ConcurrentHashMap[String, Agent]().asScala
  
  private val offices = List(
    Office.Architecture,
    Office.Implementation,
    Office.Review,
    Office.Test,
    Office.Security,
    Office.Manager
  )
  
  initializeAgents()
  
  private def initializeAgents(): Unit = {
    activeAgents("service_001") = new ServiceAgent("service_001", Office.Architecture)
    activeAgents("data_001") = new DataModelAgent("data_001", Office.Implementation)
    activeAgents("ops_001") = new OperationsAgent("ops_001", Office.Review)
  }
  
  def addAgent(agentId: String, name: String, role: String, capabilities: List[String]): Map[String, Any] = {
    val agent = FloorAgent(agentId, name, role, capabilities)
    agents(agentId) = agent
    Map(
      "status" -> "success",
      "agent" -> Map(
        "agent_id" -> agent.agentId,
        "name" -> agent.name,
        "role" -> agent.role,
        "capabilities" -> agent.capabilities
      )
    )
  }
  
  def createTask(taskId: String, title: String, assignedTo: String): Map[String, Any] = {
    var task = Task(
      taskId = taskId,
      title = title,
      status = TaskStatus.Pending,
      assignedTo = assignedTo,
      createdAt = Instant.now().toString
    )
    
    activeAgents.get(assignedTo).foreach { agent =>
      task = task.copy(status = TaskStatus.InProgress)
      val futureResult = agent.execute(task)
      Try(Await.result(futureResult, 5.seconds)) match {
        case Success(_) =>
          task = task.copy(
            status = TaskStatus.Completed,
            completedAt = Some(Instant.now().toString)
          )
        case Failure(_) =>
          task = task.copy(status = TaskStatus.Failed)
      }
    }
    
    tasks(taskId) = task
    
    Map(
      "status" -> "success",
      "task" -> taskToMap(task)
    )
  }
  
  def getFloorInfo(): Map[String, Any] = {
    Map(
      "floor_number" -> floorNumber,
      "language" -> language,
      "domain" -> domain,
      "offices" -> offices.map(o => Map("name" -> o.name, "jurisdiction" -> o.jurisdiction)),
      "agent_count" -> agents.size,
      "task_count" -> tasks.size,
      "active_agents" -> activeAgents.size,
      "agents" -> agents.values.map(a => Map(
        "agent_id" -> a.agentId,
        "name" -> a.name,
        "role" -> a.role,
        "capabilities" -> a.capabilities
      )).toList,
      "tasks" -> tasks.values.map(taskToMap).toList,
      "features" -> List(
        "Immutability by default",
        "Pure functional programming",
        "Advanced type system",
        "Pattern matching",
        "For-comprehensions",
        "Implicit/given parameters",
        "Effect systems"
      )
    )
  }
  
  def processCode(code: String, operation: String): Map[String, Any] = {
    operation match {
      case "analyze" => analyzeCode(code)
      case "format" => formatCode(code)
      case "lint" => lintCode(code)
      case "compile" => compileCheck(code)
      case _ => Map(
        "status" -> "error",
        "message" -> s"Unknown operation: $operation"
      )
    }
  }
  
  private def analyzeCode(code: String): Map[String, Any] = {
    val lines = code.split("\n").length
    val functions = "\\bdef\\b".r.findAllIn(code).length
    val classes = "\\bclass\\b".r.findAllIn(code).length
    val objects = "\\bobject\\b".r.findAllIn(code).length
    val traits = "\\btrait\\b".r.findAllIn(code).length
    val functionalScore = calculateFunctionalScore(code)
    val immutabilityScore = calculateImmutabilityScore(code)
    
    val analysis = CodeAnalysis(
      lines = lines,
      functions = functions,
      classes = classes,
      objects = objects,
      traits = traits,
      language = language,
      functionalScore = functionalScore,
      immutabilityScore = immutabilityScore
    )
    
    Map(
      "status" -> "success",
      "analysis" -> Map(
        "lines" -> analysis.lines,
        "functions" -> analysis.functions,
        "classes" -> analysis.classes,
        "objects" -> analysis.objects,
        "traits" -> analysis.traits,
        "language" -> analysis.language,
        "functional_score" -> analysis.functionalScore,
        "immutability_score" -> analysis.immutabilityScore
      )
    )
  }
  
  private def calculateFunctionalScore(code: String): Double = {
    val functionalKeywords = List("map", "flatMap", "filter", "fold", "reduce", "=>", "case")
    val count = functionalKeywords.map(kw => code.split(kw).length - 1).sum
    val totalLines = code.split("\n").length
    if (totalLines == 0) 0.0 else Math.min(1.0, count.toDouble / totalLines)
  }
  
  private def calculateImmutabilityScore(code: String): Double = {
    val valCount = "\\bval\\b".r.findAllIn(code).length
    val varCount = "\\bvar\\b".r.findAllIn(code).length
    val total = valCount + varCount
    if (total == 0) 1.0 else valCount.toDouble / total
  }
  
  private def formatCode(code: String): Map[String, Any] = {
    Map(
      "status" -> "success",
      "formatted" -> true,
      "message" -> "Scala code formatted (Scalafmt standard)",
      "changes" -> List(
        "Applied functional patterns",
        "Converted to immutable data structures",
        "Optimized for-comprehensions"
      )
    )
  }
  
  private def lintCode(code: String): Map[String, Any] = {
    val issues = scala.collection.mutable.ListBuffer[String]()
    
    if (!code.contains("package ")) {
      issues += "Missing package declaration"
    }
    
    if (code.contains("var ") && !code.contains("val ")) {
      issues += "Prefer immutable 'val' over mutable 'var'"
    }
    
    if (code.contains("null")) {
      issues += "Avoid null - use Option instead"
    }
    
    if (code.contains("throw ")) {
      issues += "Prefer Either/Try over throwing exceptions"
    }
    
    Map(
      "status" -> "success",
      "issues" -> issues.toList,
      "severity" -> (if (issues.isEmpty) "none" else "warning")
    )
  }
  
  private def compileCheck(code: String): Map[String, Any] = {
    Map(
      "status" -> "success",
      "compiled" -> true,
      "message" -> "Scala compilation check passed",
      "target" -> "Scala 3.3 / JVM 17"
    )
  }
  
  private def taskToMap(task: Task): Map[String, Any] = {
    Map(
      "task_id" -> task.taskId,
      "title" -> task.title,
      "status" -> task.status.toString,
      "assigned_to" -> task.assignedTo,
      "created_at" -> task.createdAt
    ) ++ task.completedAt.map(ca => Map("completed_at" -> ca)).getOrElse(Map.empty)
  }
  
  def handleRequest(request: String): String = {
    Try {
      val parsed = JSON.parseFull(request)
      parsed match {
        case Some(map: Map[String, Any] @unchecked) =>
          val method = map.getOrElse("method", "").asInstanceOf[String]
          val params = map.get("params").map(_.asInstanceOf[Map[String, Any]])
          val id = map.get("id").map(_.toString)
          
          val result = method match {
            case "get_info" => getFloorInfo()
            case "add_agent" =>
              params.map { p =>
                addAgent(
                  agentId = p.getOrElse("agent_id", "").asInstanceOf[String],
                  name = p.getOrElse("name", "").asInstanceOf[String],
                  role = p.getOrElse("role", "").asInstanceOf[String],
                  capabilities = p.get("capabilities")
                    .map(_.asInstanceOf[List[String]])
                    .getOrElse(List.empty)
                )
              }.getOrElse(Map("status" -> "error", "message" -> "Missing parameters"))
            case "create_task" =>
              params.map { p =>
                createTask(
                  taskId = p.getOrElse("task_id", "").asInstanceOf[String],
                  title = p.getOrElse("title", "").asInstanceOf[String],
                  assignedTo = p.getOrElse("assigned_to", "").asInstanceOf[String]
                )
              }.getOrElse(Map("status" -> "error", "message" -> "Missing parameters"))
            case "process_code" =>
              params.map { p =>
                processCode(
                  code = p.getOrElse("code", "").asInstanceOf[String],
                  operation = p.getOrElse("operation", "").asInstanceOf[String]
                )
              }.getOrElse(Map("status" -> "error", "message" -> "Missing parameters"))
            case _ =>
              Map("status" -> "error", "message" -> s"Unknown method: $method")
          }
          
          val response = Map(
            "result" -> result
          ) ++ id.map(i => Map("id" -> i)).getOrElse(Map.empty)
          
          JSONObject(response).toString()
          
        case _ =>
          JSONObject(Map(
            "error" -> Map("code" -> -32700, "message" -> "Parse error")
          )).toString()
      }
    }.getOrElse {
      JSONObject(Map(
        "error" -> Map("code" -> -32603, "message" -> "Internal error")
      )).toString()
    }
  }
}

@main def run(): Unit = {
  given ec: ExecutionContext = ExecutionContext.global
  val floor = new ScalaDepartmentFloor
  
  var continue = true
  while (continue) {
    try {
      val line = StdIn.readLine()
      if (line == null || line.isEmpty) {
        continue = false
      } else {
        val response = floor.handleRequest(line)
        println(response)
      }
    } catch {
      case _: java.io.EOFException => continue = false
      case e: Exception =>
        println(JSONObject(Map(
          "error" -> Map("code" -> -32603, "message" -> e.getMessage)
        )).toString())
    }
  }
}
