// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "SwiftDepartmentFloor",
    platforms: [
        .macOS(.v13)
    ],
    products: [
        .executable(
            name: "department_floor",
            targets: ["SwiftDepartmentFloor"]
        )
    ],
    dependencies: [],
    targets: [
        .executableTarget(
            name: "SwiftDepartmentFloor",
            dependencies: [],
            path: ".",
            sources: ["DepartmentFloor.swift"]
        )
    ]
)
