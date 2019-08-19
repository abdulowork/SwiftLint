import Foundation
import SourceKittenFramework

public struct UnusedImportRule: CorrectableRule, ConfigurationProviderRule, AnalyzerRule, AutomaticTestableRule {
    public var configuration = SeverityConfiguration(.warning)

    public init() {}

    public static let description = RuleDescription(
        identifier: "unused_import",
        name: "Unused Import",
        description: "All imported modules should be required to make the file compile.",
        kind: .lint,
        nonTriggeringExamples: [
            """
            import Dispatch
            dispatchMain()
            """,
            """
            @testable import Dispatch
            dispatchMain()
            """,
            """
            import Foundation
            @objc
            class A {}
            """,
            """
            import UnknownModule
            func foo(error: Swift.Error) {}
            """,
            """
            import Foundation
            import ObjectiveC
            let 👨‍👩‍👧‍👦 = #selector(NSArray.contains(_:))
            👨‍👩‍👧‍👦 == 👨‍👩‍👧‍👦
            """
        ],
        triggeringExamples: [
            """
            ↓import Dispatch
            struct A {
              static func dispatchMain() {}
            }
            A.dispatchMain()
            """,
            """
            ↓import Foundation
            struct A {
              static func dispatchMain() {}
            }
            A.dispatchMain()
            ↓import Dispatch

            """,
            """
            ↓import Foundation
            dispatchMain()
            """,
            """
            ↓import Foundation
            // @objc
            class A {}
            """,
            """
            ↓import Foundation
            import UnknownModule
            func foo(error: Swift.Error) {}
            """
        ],
        corrections: [
            """
            ↓import Dispatch
            struct A {
              static func dispatchMain() {}
            }
            A.dispatchMain()
            """:
            """
            struct A {
              static func dispatchMain() {}
            }
            A.dispatchMain()
            """,
            """
            ↓import Foundation
            struct A {
              static func dispatchMain() {}
            }
            A.dispatchMain()
            ↓import Dispatch

            """:
            """
            struct A {
              static func dispatchMain() {}
            }
            A.dispatchMain()

            """,
            """
            ↓import Foundation
            dispatchMain()
            """:
            """
            dispatchMain()
            """,
            """
            ↓@testable import Foundation
            import Dispatch
            dispatchMain()
            """:
            """
            import Dispatch
            dispatchMain()
            """,
            """
            ↓import Foundation
            // @objc
            class A {}
            """:
            """
            // @objc
            class A {}
            """
        ],
        requiresFileOnDisk: true
    )

    public func validate(file: File, compilerArguments: [String]) -> [StyleViolation] {
        return violationRanges(in: file, compilerArguments: compilerArguments).map {
            StyleViolation(ruleDescription: type(of: self).description,
                           severity: configuration.severity,
                           location: Location(file: file, characterOffset: $0.location))
        }
    }

    public func correct(file: File, compilerArguments: [String]) -> [Correction] {
        let violations = violationRanges(in: file, compilerArguments: compilerArguments)
        let matches = file.ruleEnabled(violatingRanges: violations, for: self)
        if matches.isEmpty { return [] }

        var contents = file.contents.bridge()
        let description = type(of: self).description
        var corrections = [Correction]()
        for range in matches.reversed() {
            contents = contents.replacingCharacters(in: range, with: "").bridge()
            let location = Location(file: file, characterOffset: range.location)
            corrections.append(Correction(ruleDescription: description, location: location))
        }
        file.write(contents.bridge())
        return corrections
    }

    private func violationRanges(in file: File, compilerArguments: [String]) -> [NSRange] {
        guard !compilerArguments.isEmpty else {
            queuedPrintError("""
                Attempted to lint file at path '\(file.path ?? "...")' with the \
                \(type(of: self).description.identifier) rule without any compiler arguments.
                """)
            return []
        }

        return file.unusedImports(compilerArguments: compilerArguments).map { $0.1 }
    }
}

private extension File {
    func unusedImports(compilerArguments: [String]) -> [(String, NSRange)] {
        guard let index = try? Request.index(file: path!, arguments: compilerArguments).sendIfNotDisabled() else {
            queuedPrintError("Could not get index")
            return []
        }

        let flatEntities = self.flatEntities(entity: index)
        let importedModules: Set<String> = Set(
            flatEntities.compactMap {
                guard let kind = $0["key.kind"] as? String else { return nil }
                if kind == moduleKind {
                    return $0["key.name"] as? String
                } else {
                    return nil
                }
            }
        )

        let offsetPerLine = self.offsetPerLine()
        var usedModules: Set<String> = Set(
            flatEntities
                .filter { entity in
                    guard let kind = entity["key.kind"] as? String else { return false }
                    if let syntaxKind = SyntaxKind(rawValue: kind),
                        syntaxKindsToSkip.contains(syntaxKind) { return false }
                    return kind != moduleKind
                }
                .compactMap { entity -> [String]? in
                    guard let offset = entity.offset(in: offsetPerLine) else {
                        queuedPrintError("Could not get offset")
                        return nil
                    }
                    guard let cursorInfo = try? Request
                        .cursorInfo(file: path!, offset: offset, arguments: compilerArguments)
                        .sendIfNotDisabled() else {
                            queuedPrintError("Could not get cursor info")
                            return nil
                    }
                    return (cursorInfo["key.modulename"] as? String)?.split(separator: ".").map(String.init)
                }
                .flatMap { $0 }
        )

        if containsAttributesRequiringFoundation() { usedModules.insert("Foundation") }

        return rangedAndSortedUnusedImports(
            of: Array(importedModules.subtracting(usedModules)),
            contents: contents.bridge()
        )
    }

    func rangedAndSortedUnusedImports(of unusedImports: [String], contents: NSString) -> [(String, NSRange)] {
        return unusedImports
            .map { module in
                let testableImportRange = contents.range(of: "@testable import \(module)\n")
                if testableImportRange.location != NSNotFound {
                    return (module, testableImportRange)
                }

                return (module, contents.range(of: "import \(module)\n"))
            }
            .sorted(by: { $0.1.location < $1.1.location })
    }

    typealias Entity = [String: SourceKitRepresentable]
    func flatEntities(entity: Entity) -> [Entity] {
        let entities = entity.entities
        if entities.isEmpty {
            return [entity]
        } else {
            return [entity] + entities.flatMap { flatEntities(entity: $0) }
        }
    }

    func offsetPerLine() -> [Int: Int64] {
        return [Int: Int64](
            uniqueKeysWithValues: contents.bridge()
                .components(separatedBy: "\n")
                .map {
                    Int64($0.bridge().lengthOfBytes(using: .utf8))
                }
                .reduce([Int64(0)]) { result, length in
                    let newLineCharacterLength = Int64(1)
                    let lineLength = length + newLineCharacterLength
                    return result + [(result.last ?? 0) + lineLength]
                }
                .enumerated()
                .map { ($0.offset, $0.element) }
        )
    }

    func containsAttributesRequiringFoundation() -> Bool {
        guard contents.contains("@objc") else {
            return false
        }

        func containsAttributesRequiringFoundation(dict: [String: SourceKitRepresentable]) -> Bool {
            if !attributesRequiringFoundation.isDisjoint(with: dict.enclosedSwiftAttributes) {
                return true
            } else {
                return dict.substructure.contains(where: containsAttributesRequiringFoundation)
            }
        }

        return containsAttributesRequiringFoundation(dict: self.structure.dictionary)
    }
}

private extension Dictionary where Key == String, Value == SourceKitRepresentable {
    func offset(in offsetPerLine: [Int: Int64]) -> Int64? {
        guard
            let line = self["key.line"] as? Int64,
            let column = self["key.column"] as? Int64,
            let lineOffset = offsetPerLine[Int(line) - 1]
            else { return nil }
        return lineOffset + column - 1
    }
}

private let moduleKind = "source.lang.swift.ref.module"

private let syntaxKindsToSkip: Set<SyntaxKind> = [
    .attributeBuiltin,
    .keyword,
    .number,
    .docComment,
    .string,
    .stringInterpolationAnchor,
    .attributeID,
    .buildconfigKeyword,
    .buildconfigID,
    .commentURL,
    .comment,
    .docCommentField
]

private let attributesRequiringFoundation: Set<SwiftDeclarationAttributeKind> = [
    .objc,
    .objcName,
    .objcMembers,
    .objcNonLazyRealization
]
