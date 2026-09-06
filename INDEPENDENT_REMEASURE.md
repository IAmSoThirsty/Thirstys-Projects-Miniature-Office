# Independent remeasure log

**Rule:** a claim is true only if the tree implements it.

**Code pin:** [`fdd9762`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a) — last commit that changed `src/` or `tests/`. This file is a measurement log. It does **not** retarget the pin. Do not record “docs commit on `main` at this writing.”

Score remains **9 hold / 6 partial / 1 inflated / 3 false** of 19.

## 6 September 2026 23:10 UTC — observed main `0aaf783` (pytest re-run)

Independent clone of [`0aaf783`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/0aaf7839d6dd2943647a2d0b87da83b2608a1d56) (PR #35, docs-only). `git log fdd9762..HEAD -- src tests` empty. Git tree objects for `src/` (`fafbad684ed9d61bd5fd347098276eeea4b911d3`) and `tests/` (`1ddf08f8a24d9003054c0a395e06c95470009fe0`) match code pin `fdd9762`.

| Metric | Value |
| --- | --- |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28 |
| `floors/sql/schema.sql` | present |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | 1,619 |
| Pytest | **1,573 passed**, 1 skipped, **13.09s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high (13 low; 1 skipped_tests) |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| CI on `0aaf783` | [33817498446](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33817498446) succeeded |
| CD on `0aaf783` | [33817498306](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33817498306) succeeded |
| Live EC-013 | matches tree (experimental prototype; pin `fdd9762`) |

Headline metrics match the code pin. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

## 30 August 2026 — docs successor `268058c` (pytest re-run)

Independent clone of [`268058c`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/268058c1b17408691807c20b01e2ba91fa54f4ce). `git log fdd9762..HEAD -- src tests` empty. Git tree objects for `src/` (`fafbad684ed9d61bd5fd347098276eeea4b911d3`) and `tests/` (`1ddf08f8a24d9003054c0a395e06c95470009fe0`) match code pin `fdd9762`.

| Metric | Value |
| --- | --- |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28 |
| `floors/sql/schema.sql` | present |
| Entity types / relations | 7 / 8 |
| AST patterns / anti-patterns | 5 / 4 |
| Anchored `def test_` in `tests/` | 1,606 (33 module-level + 1,573 class methods) |
| Unanchored `def test_` in `tests/` | 1,608 |
| Whole-repo unanchored `def test_` | **1,619** (not 1,626) |
| Pytest | **1,573 passed**, 1 skipped, **13.75s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high |
| Bandit low findings | 13 |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| CI on `268058c` | [33263264093](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33263264093) succeeded |
| CD on `268058c` | [33263264131](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33263264131) succeeded |

Headline metrics match the code pin. The only live numeric error in canonical files was `claims.json` `def_test_grep_whole_repo_unanchored` **1626**, which independent `*.py` grep (excluding `.git`) measures as **1619**. That field is corrected in the same docs change. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. Production ready remains false. Do not name a docs SHA as HEAD.

GitHub's HTML README on 30 August 2026 matches the raw experimental-prototype text (the earlier `27d7fdf` HTML-staleness note was clone-time history).


## 29 August 2026 — then-HEAD `32a70dc` (pytest run)

Clone of [`32a70dc`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/32a70dc7a73d3143a0f06c36361ff2b64a1a9124). `git log fdd9762..HEAD -- src tests` empty.

| Metric | Value |
| --- | --- |
| `src/**/*.py` files | 53 |
| `src/` lines | 24,441 total / 19,058 non-comment |
| `code_civilization.py` | 1,421 lines / 52,653 bytes |
| `@app.route` in `src/` | 74 (67 in `app.py` + 7 IDE) |
| Floor directories | 28 |
| `floors/sql/schema.sql` | present |
| Anchored `def test_` in `tests/` | 1,606 |
| Unanchored `def test_` in `tests/` | 1,608 |
| Pytest | **1,573 passed**, 1 skipped, **13.18s** |
| Coverage XML `--cov=src` | **7,494 / 7,749** (96.71%) |
| `bandit -r src -ll` | 0 medium/high |
| `pip-audit -r requirements.txt` | clean |
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |
| CI on `32a70dc` | [33250434458](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434458) succeeded |
| CD on `32a70dc` | [33250434461](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33250434461) succeeded |

These numbers match [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) and [claims.json](claims.json) for the code pin.

## 29 August 2026 — docs successor `a0910d4` (counts only)

Clone of [`a0910d4`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/a0910d4768f0a9ce05c65b91761ef7dc62e54110). Counted `src/` **24,441** / **19,058** non-comment (53 files), **74** `@app.route` in `src/` (67+7), 28 floor dirs, `code_civilization.py` **1,421** lines / **52,653** bytes, anchored `def test_` = 1,606. Matches the code pin. Pytest was **not** re-run in this clone (last pytest: `32a70dc`). CI [33252125717](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125717) **succeeded**. CD [33252125743](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33252125743) **succeeded**. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**.



## 29 August 2026 — docs successor `32b08d8` (counts only)

Clone of [`32b08d8`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/32b08d8a7cf5808bd188077ff0b32db56c69ee7a) (PR #31 squash). Git tree objects for `src/` (`fafbad684ed9d61bd5fd347098276eeea4b911d3`) and `tests/` (`1ddf08f8a24d9003054c0a395e06c95470009fe0`) match code pin `fdd9762`. Counted `src/` **24,441** / **19,058** non-comment (53 files), **74** `@app.route` in `src/` (67+7), 28 floor dirs, `code_civilization.py` **1,421** lines / **52,653** bytes, anchored `^\s*def test_` = 1,606 (33 module-level + 1,573 class methods), unanchored = 1,608. Pytest was **not** re-run (last pytest: `32a70dc`). CI [33262809624](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809624) **succeeded**. CD [33262809630](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/actions/runs/33262809630) **succeeded**. Pin stays `fdd9762`. Score stays **9/6/1/3 of 19**. GitHub's HTML landing page was still serving a stale `27d7fdf` README at the time of this clone; the API and raw files at `32b08d8` are the measured tree.

## 29 August 2026 — then-HEAD `c783357` (counts only)

Clone of [`c783357`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/c7833576720d381322a2186a26b610715fd6f388). Counts matched the pin. Pytest was **not** run in that clone.

## Outside this repository (updated 6 September 2026 23:10 UTC)

Live [thirstysystems.com/claims](https://www.thirstysystems.com/claims) EC-013 now reports that the README no longer badges Production Ready and pins `CLAIMS_AUDIT.md` at `fdd9762`. [/systems/miniature-office](https://www.thirstysystems.com/systems/miniature-office) now cites 1,573 tests and 28 floors. The February `537c469` / PRODUCTION READY sentence is historical. Production-ready remains false.
