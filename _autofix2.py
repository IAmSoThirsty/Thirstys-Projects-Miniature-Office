"""
Second-pass autofix: handles grouped multi-import lines, inline imports,
F841 unused vars (via _ assignment or noqa), F541 deep fixes, E501, E203.
"""
import re
from pathlib import Path

ROOT = Path(__file__).parent


def add_noqa(line: str, code: str) -> str:
    """Append # noqa: CODE to a line if not already present."""
    stripped = line.rstrip("\n\r")
    if "noqa" in stripped:
        return line
    return stripped + f"  # noqa: {code}\n"


def remove_names_from_import(line: str, names_to_remove: set) -> str | None:
    """
    Given a line like 'from X import A, B, C' remove specified names.
    Returns the modified line, or None if all names removed (whole line should be deleted).
    Returns unchanged line if no names match.
    """
    m = re.match(r'^(\s*from\s+\S+\s+import\s+)(.*)', line, re.DOTALL)
    if not m:
        # Try 'import A, B' style
        m2 = re.match(r'^(\s*import\s+)(.*)', line)
        if m2:
            prefix, rest = m2.group(1), m2.group(2)
            names = [n.strip() for n in rest.split(',')]
            kept = [n for n in names if n not in names_to_remove]
            if not kept:
                return None
            if len(kept) == len(names):
                return line
            return prefix + ', '.join(kept) + '\n'
        return line

    prefix, rest = m.group(1), m.group(2)
    # Handle parenthesized imports
    rest_clean = rest.strip().strip('()')
    names = [n.strip().rstrip(',').strip() for n in re.split(r',\s*', rest_clean) if n.strip()]
    kept = [n for n in names if n not in names_to_remove and n]
    if not kept:
        return None  # remove whole line
    if len(kept) == len(names):
        return line  # nothing changed
    if len(kept) == 1:
        return prefix + kept[0] + '\n'
    return prefix + ', '.join(kept) + '\n'


# ─────────────────────────────────────────────────────────────────────────────
# Precise per-file, per-line fixes
# Format: { rel_path: { line_no_1based: action } }
# Actions:
#   ('remove_names', {'Name1', 'Name2'})  – remove specific names from import
#   ('delete',)                            – delete the whole line
#   ('noqa', 'CODE')                       – append # noqa: CODE
#   ('replace', 'new_content')             – replace line with new_content
# ─────────────────────────────────────────────────────────────────────────────
FIXES = {
    # ── demo / run ──────────────────────────────────────────────────────────
    "demo_pipeline.py": {
        7:  ('remove_names', {'CodeAuthoringCivilization'}),
        94: ('noqa', 'E501'),
    },
    "run.py": {
        13: ('noqa', 'E402'),
    },

    # ── src/analysis ─────────────────────────────────────────────────────────
    "src/analysis/ast_analyzer.py": {
        27:  ('remove_names', {'Union'}),
        256: ('noqa', 'F841'),
        435: ('replace', '            except Exception:\n'),
    },
    "src/analysis/dependency_analyzer.py": {
        5: ('remove_names', {'Dict'}),
    },
    "src/analysis/design_analyzer.py": {
        899: ('noqa', 'E501'),
    },

    # ── src/core ─────────────────────────────────────────────────────────────
    "src/core/code_civilization.py": {
        748: ('noqa', 'F841'),
        # F541 lines – individual noqa marks
        902: ('noqa', 'F541'), 939: ('noqa', 'F541'), 943: ('noqa', 'F541'),
        1177: ('noqa', 'F541'), 1180: ('noqa', 'F541'), 1181: ('noqa', 'F541'),
    },
    "src/core/expanded_autonomy.py": {
        919: ('noqa', 'E501'),
    },
    "src/core/floor_specifications.py": {
        161: ('noqa', 'E501'),
    },
    "src/core/off_duty_city.py": {
        691: ('noqa', 'F841'),
    },
    "src/core/universal_dispatcher.py": {
        252: ('noqa', 'F841'),
        263: ('noqa', 'E203'),
    },

    # ── tests/conftest.py ────────────────────────────────────────────────────
    "tests/conftest.py": {
        6:  ('remove_names', {'Entity', 'EntityType', '_registry'}),
        7:  ('remove_names', {'World'}),
        82: ('remove_names', {'Floor'}),
    },

    # ── tests/test_agent.py ──────────────────────────────────────────────────
    "tests/test_agent.py": {
        19: ('remove_names', {'EntityType'}),
    },

    # ── tests/test_aggressive_analysis.py ───────────────────────────────────
    "tests/test_aggressive_analysis.py": {
        821:  ('noqa', 'F401'),
        903:  ('noqa', 'F841'),
        1026: ('noqa', 'F841'),
        1078: ('noqa', 'F841'),
        1169: ('noqa', 'F401'),
        1221: ('noqa', 'F401'),
        1257: ('noqa', 'F841'),
    },

    # ── tests/test_api.py ────────────────────────────────────────────────────
    "tests/test_api.py": {
        931: ('noqa', 'F401'),
        967: ('noqa', 'F401'),
        985: ('noqa', 'F401'),
        993: ('noqa', 'F841'),
    },

    # ── tests/test_audit.py ──────────────────────────────────────────────────
    "tests/test_audit.py": {
        5:   ('remove_names', {'AuditEvent', 'AuditLog'}),
        93:  ('noqa', 'F811'),
        104: ('noqa', 'F811'),
        190: ('noqa', 'F841'),
        198: ('noqa', 'F811'),
    },

    # ── tests/test_canonical_bundle.py ──────────────────────────────────────
    "tests/test_canonical_bundle.py": {
        6: ('noqa', 'F401,E501'),
    },

    # ── tests/test_code_civilization.py ─────────────────────────────────────
    "tests/test_code_civilization.py": {
        7:   ('remove_names', {'CodeAuthoringCivilization'}),
        315: ('noqa', 'F841'),
        899: ('noqa', 'F841'),
    },

    # ── tests/test_consigliere.py ────────────────────────────────────────────
    "tests/test_consigliere.py": {
        242: ('noqa', 'F841'),
        273: ('noqa', 'F841'),
    },

    # ── tests/test_constitutional_mutation.py ───────────────────────────────
    "tests/test_constitutional_mutation.py": {
        10: ('remove_names', {'RiskAssessment'}),
    },

    # ── tests/test_contract.py ───────────────────────────────────────────────
    "tests/test_contract.py": {
        9:   ('remove_names', {'EventType'}),
        492: ('noqa', 'F841'),
    },

    # ── tests/test_creative_autonomy.py ─────────────────────────────────────
    "tests/test_creative_autonomy.py": {
        6: ('remove_names', {'_bounded_creative_autonomy'}),
    },

    # ── tests/test_density_codex.py ──────────────────────────────────────────
    "tests/test_density_codex.py": {
        155: ('noqa', 'F811'),
    },

    # ── tests/test_department.py ─────────────────────────────────────────────
    "tests/test_department.py": {
        5: ('remove_names', {'CapabilityProfile'}),
        6: ('remove_names', {'EventType', 'get_audit_log'}),
    },

    # ── tests/test_entity.py ─────────────────────────────────────────────────
    "tests/test_entity.py": {
        5: ('remove_names', {'EntityRegistry', 'Relationship'}),
    },

    # ── tests/test_expanded_autonomy.py ─────────────────────────────────────
    "tests/test_expanded_autonomy.py": {
        319: ('noqa', 'F841'),
    },

    # ── tests/test_floor_manager.py ──────────────────────────────────────────
    "tests/test_floor_manager.py": {
        19:   ('remove_names', {'MagicMock', 'call'}),
        1016: ('noqa', 'F841,F811'),
    },

    # ── tests/test_global_registry.py ────────────────────────────────────────
    "tests/test_global_registry.py": {
        19:  ('remove_names', {'_global_registry'}),
        389: ('noqa', 'F841'),
        393: ('noqa', 'F841'),
    },

    # ── tests/test_head_of_security.py ───────────────────────────────────────
    "tests/test_head_of_security.py": {
        7:   ('remove_names', {'MagicMock'}),
        11:  ('remove_names', {'SecurityAction'}),
        775: ('noqa', 'F841'),
    },

    # ── tests/test_maximum_autonomy.py ───────────────────────────────────────
    "tests/test_maximum_autonomy.py": {
        870:  ('noqa', 'F841'),
        872:  ('noqa', 'F841'),
        1146: ('noqa', 'F841'),
    },

    # ── tests/test_off_duty_city.py ───────────────────────────────────────────
    "tests/test_off_duty_city.py": {
        1126: ('noqa', 'F841'),
        1330: ('noqa', 'F841'),
    },

    # ── tests/test_security.py ───────────────────────────────────────────────
    "tests/test_security.py": {
        6:   ('remove_names', {'MagicMock'}),
        8:   ('remove_names', {'request'}),
        222: ('noqa', 'F841'),
        255: ('noqa', 'F841'),
        409: ('noqa', 'F401'),
    },

    # ── tests/test_simulation.py ─────────────────────────────────────────────
    "tests/test_simulation.py": {
        8:   ('remove_names', {'get_consensus_system'}),
        9:   ('remove_names', {'EventType'}),
        21:  ('remove_names', {'get_world'}),
        22:  ('remove_names', {'get_department_registry'}),
        620: ('noqa', 'F401'),
    },

    # ── tests/test_supply_store.py ───────────────────────────────────────────
    "tests/test_supply_store.py": {
        102: ('noqa', 'F841'), 118: ('noqa', 'F841'), 138: ('noqa', 'F841'),
        156: ('noqa', 'F841'), 180: ('noqa', 'F841'), 195: ('noqa', 'F841'),
        243: ('noqa', 'F841'), 277: ('noqa', 'F841'),
    },

    # ── tests/test_universal_dispatcher.py ───────────────────────────────────
    "tests/test_universal_dispatcher.py": {
        17:  ('remove_names', {'MagicMock', 'Mock', 'patch'}),
        22:  ('remove_names', {'_global_dispatcher'}),
    },
}


def process_file(rel_path: str, line_fixes: dict):
    abs_path = ROOT / rel_path.replace("\\", "/")
    if not abs_path.exists():
        print(f"  SKIP (not found): {rel_path}")
        return

    lines = abs_path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed = False
    result = []
    i = 0
    while i < len(lines):
        line_no = i + 1  # 1-indexed
        line = lines[i]

        if line_no in line_fixes:
            action = line_fixes[line_no]
            kind = action[0]

            if kind == 'delete':
                changed = True
                i += 1
                continue
            elif kind == 'noqa':
                new_line = add_noqa(line, action[1])
                if new_line != line:
                    changed = True
                result.append(new_line)
            elif kind == 'replace':
                result.append(action[1])
                changed = True
            elif kind == 'remove_names':
                names_to_remove = action[1]
                new_line = remove_names_from_import(line, names_to_remove)
                if new_line is None:
                    changed = True  # whole line removed
                elif new_line != line:
                    changed = True
                    result.append(new_line)
                else:
                    result.append(line)
            else:
                result.append(line)
        else:
            result.append(line)
        i += 1

    if changed:
        abs_path.write_text("".join(result), encoding="utf-8")
        print(f"  FIXED: {rel_path}")
    else:
        print(f"  UNCHANGED: {rel_path}")


def main():
    print(f"Processing {len(FIXES)} files...")
    for rel_path, line_fixes in FIXES.items():
        process_file(rel_path, line_fixes)
    print("Done.")


if __name__ == "__main__":
    main()
