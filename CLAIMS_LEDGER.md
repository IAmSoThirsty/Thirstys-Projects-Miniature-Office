# Claims ledger (machine-readable companion)

Canonical prose remains [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). This file is the
score that must sum.

**Code pin (measured tree):** [`fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a) — last commit that changed `src/` or `tests/`.

Later docs-only commits (`1a103bf`, `c783357`, `32a70dc`, `a0910d4`, `32b08d8`, `268058c`, and successors) do not retarget the pin. `src/` and `tests/` stay identical. Do not name a docs SHA as HEAD.

**Independent remeasure (9 September 2026 22:33 UTC):** clone of observed main `f25adcf` after squash-merge of PR [#45](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/45) (docs-only). `src/` tree `fafbad68` and `tests/` tree `1ddf08f8` match code pin `fdd9762`. Independent pytest on `f25adcf`: **1,573 passed**, 1 skipped, **7.63s**. Coverage XML **7,494 / 7,749** (96.71%). Bandit `-ll` 0 medium/high (13 low). `pip-audit` clean. CI [34412193707](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34412193707) / CD [34412193733](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34412193733) green on `f25adcf`. Present-tense miss: GETTING_STARTED.md opened without the experimental-prototype status line. Lint/vulns Holds is the Python pip-audit / bandit `-ll` gate; Dependabot PR #13 (`flatted` in toy `floors/typescript`) is outside that gate. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false.

**Independent remeasure (9 September 2026 22:22 UTC):** clone of observed main `8fdaca0` after squash-merge of PR [#44](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/44) (docs-only). `src/` tree `fafbad68` and `tests/` tree `1ddf08f8` match code pin `fdd9762`. Independent pytest on `8fdaca0`: **1,573 passed**, 1 skipped, **12.34s**. Coverage XML **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%). Bandit `-ll` 0 medium/high (13 low). `pip-audit` clean. CI [34406238644](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34406238644) / CD [34406238661](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34406238661) green on `8fdaca0`. Present-tense miss: `install.sh` / `install.ps1` still said Python 3.9+; [PLATFORM_SUPPORT.md](PLATFORM_SUPPORT.md) said the installer creates a venv; `ARCHITECTURE.md` Layer 9 treated the 100ms dataclass default as the running tick (`init_simulation()` hardcodes 1000ms; `TICK_DURATION_MS` unread); `.env.example` / compose listed HOST/PORT/WORKERS/LOG_* that the process ignores. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false.

**Independent remeasure (9 September 2026 21:16 UTC):** clone of observed main `d61d253` after squash-merge of PR [#43](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/43) (docs-only). `src/` tree `fafbad68` and `tests/` tree `1ddf08f8` match code pin `fdd9762`. Independent pytest on `d61d253`: **1,573 passed**, 1 skipped, **13.16s**. Coverage XML **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%). Bandit `-ll` 0 medium/high (13 low). `pip-audit` clean. CI [34393910856](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34393910856) / CD [34393910832](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34393910832) green on `d61d253`. Present-tense miss: `package.json` keywords still listed `pixel-art`; shipped canvas is `fillRect` rectangles (PR #41). `ARCHITECTURE.md` Contributing pointed at README contribution guidelines that do not exist. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false.

**Independent remeasure (9 September 2026 19:11 UTC):** clone of observed main `a153b92` after squash-merge of PR [#42](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/42) (docs-only). `src/` tree `fafbad68` and `tests/` tree `1ddf08f8` match code pin `fdd9762`. Independent pytest on `a153b92`: **1,573 passed**, 1 skipped, **12.56s**. Coverage XML **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%). Bandit `-ll` 0 medium/high (13 low). `pip-audit` clean. CI [34358022421](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34358022421) / CD [34358022556](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34358022556) green on `a153b92`. Present-tense miss: `ARCHITECTURE.md` Layer 7 presented a contract DSL the tree does not parse; Layer 8 called entities JSON Schema compliant (`codex/office.json`) but there is no `codex/` directory; Layer 9's `persistState` is a log-only `agent_action`; Performance claimed a 10–50ms tick and an O(log n) causality index. Tick config is 100ms; events are a list. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false.

**Independent remeasure (9 September 2026 13:26 UTC):** clone of observed main `a1db8ae` after squash-merge of PR [#41](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/41) (docs-only). `src/` tree `fafbad68` and `tests/` tree `1ddf08f8` match code pin `fdd9762`. Independent pytest on `a1db8ae`: **1,573 passed**, 1 skipped, **13.03s**. Coverage XML **7,494 / 7,749** (96.71%). Bandit `-ll` 0 medium/high (13 low). `pip-audit` clean. CI [34351639115](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34351639115) / CD [34351639093](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34351639093) green on `a1db8ae`. `DEPLOYMENT.md` no longer claims `/health` returns 503 or a shipped `k8s/` stack. `ARCHITECTURE.md` Layer 2 no longer calls `audit.py` an immutable ledger. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false.

**Independent remeasure (9 September 2026 12:30 UTC):** clone of observed main `526721e` after squash-merge of PR [#40](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/pull/40) (docs-only). `src/` tree `fafbad68` and `tests/` tree `1ddf08f8` match code pin `fdd9762`. Independent pytest on `526721e`: **1,573 passed**, 1 skipped, **13.09s**. Coverage XML **7,493 / 7,749** (96.70%); pin remains **7,494 / 7,749** (96.71%). Bandit `-ll` 0 medium/high (13 low). `pip-audit` clean. CI [34347045367](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34347045367) / CD [34347045102](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/34347045102) green on `526721e`. Operator how-to after #40: bash compose is not all-platforms; PWA SW needs a secure context; rotating `SECRET_KEY` breaks persisted HMAC. `ARCHITECTURE.md` Layer 11 no longer describes the shipped canvas as pixel-art / CRT. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false.

**Independent remeasure (6 September 2026 23:10 UTC):** clone of observed main `0aaf783` (PR #35, docs-only). `src/` tree `fafbad68` and `tests/` tree `1ddf08f8` match code pin `fdd9762`. Pytest **1,573 passed**, 1 skipped, **13.09s**. Coverage XML **7,494 / 7,749** (96.71%). Bandit `-ll` 0 medium/high (13 low). `pip-audit` clean. Live thirstysystems.com EC-013 now matches (no longer the February PRODUCTION READY sentence). Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**.

**Independent remeasure (30 August 2026):** clone of docs successor `268058c` (pytest **1,573 passed**, 1 skipped, **13.75s**; coverage XML **7,494 / 7,749**; CI [33263264093](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33263264093) / CD [33263264131](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33263264131) green). Whole-repo unanchored `def test_` corrected **1626 → 1619**. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**.

**Independent remeasure (29 August 2026):** clone of then-HEAD `32a70dc` (pytest) and later docs successors `a0910d4` and `32b08d8` (counts only; `a0910d4` CI [33252125717](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125717) / CD [33252125743](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125743); `32b08d8` CI [33262809624](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809624) / CD [33262809630](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809630) green). Counted `src/` **24,441** / **19,058** non-comment (53 files), **74** `@app.route` in `src/` (67+7), 28 floor dirs, `code_civilization.py` **1,421** lines / **52,653** bytes, anchored `def test_` = 1,606. Pytest **1,573 passed**, 1 skipped, **13.18s**. Coverage XML **7,494 / 7,749** (96.71%). Bandit `-ll` clean. `pip-audit` clean. Matches the code-pin metrics. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. [thirstysystems.com EC-013](https://www.thirstysystems.com/claims) was still stale as of that date (pinned `537c469`, described a PRODUCTION READY README badge).

**Status:** experimental prototype — not production-ready

**Rule:** a claim is true only if the tree implements it.

**Independent pytest on `fdd9762`:** **1,573 passed / 1 skipped** (13.14s). `src/` **24,441** lines / **19,058** non-comment; **74** `@app.route`; 28 floor dirs; `code_civilization.py` **1,421** lines / **52,653** bytes; fresh `--cov=src` **7,494 / 7,749** (96.71% of imported statements). Anchored `def test_` = 1,606; unanchored in `tests/` = 1,608; whole-repo unanchored = **1,619** (not 1,626). Bandit `-ll` clean (B104 nosec). `pip-audit` clean.

**Actions on code pin `fdd9762`:** CI [33212776987](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776987) **succeeded**. CD [33212776992](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33212776992) **succeeded**.

**Actions on later docs-only commits:** `1a103bf` CI [33215760008](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760008) **succeeded**, CD [33215760012](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33215760012) **succeeded**. `32a70dc` CI [33250434458](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434458) **succeeded**, CD [33250434461](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434461) **succeeded**. `a0910d4` CI [33252125717](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125717) **succeeded**, CD [33252125743](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125743) **succeeded**. `32b08d8` CI [33262809624](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809624) **succeeded**, CD [33262809630](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809630) **succeeded**. `268058c` CI [33263264093](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33263264093) **succeeded**, CD [33263264131](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33263264131) **succeeded**.

Re-running coverage on an identical tree can swing by one statement (7,493 vs 7,494). That is not evidence the parent pin was wrong. Do not report `def test_` = 1,612.

## Score

| Holds | Partial | Inflated | False | Total |
| --- | --- | --- | --- | --- |
| 9 | 6 | 1 | 3 | 19 |

`9 + 6 + 1 + 3 = 19`.

## Verdicts (must match the audit table)

**Holds (9)**

- Tests passing (1,573 passed, 1 skipped; anchored `def test_` = 1,606)
- 45+ API endpoints (74 routes)
- Formal entity ontology (7 types, 8 relations)
- Canonical docs consistent (this pin; files cite code pin `fdd9762`, not a docs-only HEAD)
- Apache 2.0
- Real workspace / editor API / terminal (jailed FS, no-shell argv, 7 `/api/ide/*` routes, browser file-tree/editor/terminal UI)
- Lint / known-vuln gates (Black, isort, flake8 E9/F63/F7/F82; bandit `-ll` 0 medium/high with documented B104 nosec; `pip-audit` clean on `requirements.txt`). 13 bandit **low** findings remain. Open Dependabot PR #13 is toy-floor npm, outside this gate.
- Registry thread-safety via `threading.RLock` (the GIL is not the mutex)
- CI / CD healthy (green on `fdd9762` and on later docs-only commits including `268058c`)

**Partial (6)**

- 30+ native language floors (28 toy dirs; SQL now has `schema.sql`; department runtime is Python)
- Complete codegen → tested artifact (identity bodies; Python tests **are** executed; other languages still assumed)
- Cryptographic immutable audit log (SHA-256 chain; optional HMAC-SHA256 when a real key is set; not a ledger)
- Desktop / mobile / VR (browser UI + PWA manifest/service worker; no WebXR)
- Docker / compose (files exist; CD `test-docker` green on `fdd9762` and later docs-only commits; not hardened — in-memory world, `chmod 777`)
- 23+ patterns / SOLID / smells (AST walkers exist for 5 patterns and 4 anti-patterns; not a 23+/SOLID product)

**Inflated (1)**

- 99% coverage (96.71% of imported statements; `integrated_specs/` omitted)

**False (3)**

- Production ready
- 18,285 `src/` lines (measured 24,441 / 19,058 non-comment)
- `code_civilization.py` is 49,741 lines (it is 1,421; bytes were counted as lines)

VR is **Partial**, not Holds: PWA exists; WebXR does not.

CI/CD is **Holds**, not Partial: both jobs were observed green on `fdd9762`. Docker stays Partial.

See [claims.json](claims.json) for the same table as JSON. See [INDEPENDENT_REMEASURE.md](INDEPENDENT_REMEASURE.md) for clone counts, including the 30 August 2026 pytest re-run.

## Remaining product gaps

- Template codegen for non-Python; identity bodies for Python
- 28 toy floors
- HMAC is optional and is not a public ledger
- IDE HTTP API is open unless `MO_IDE_TOKEN` is set (required only in production)
- Bandit still reports 13 low findings
- Docker compose is a healthchecked container, not a hardened service
