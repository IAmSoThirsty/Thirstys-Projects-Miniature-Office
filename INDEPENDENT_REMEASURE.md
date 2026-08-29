# Independent remeasure — 29 August 2026

**Rule:** a claim is true only if the tree implements it.

This file records a fresh clone of `main` at
[`c783357`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/c7833576720d381322a2186a26b610715fd6f388).
It does **not** retarget the code pin. The measured implementation tree remains
[`fdd9762`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a).

## Counts taken on HEAD `c783357`

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
| PWA `manifest.json` + `sw.js` | present |
| WebXR | absent |
| LICENSE | Apache 2.0 |

These numbers match [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) and [claims.json](claims.json) for the code pin. Score remains **9 hold / 6 partial / 1 inflated / 3 false** of 19.

Canonical README / LIMITATIONS already state the product is an experimental prototype. Historical `PRODUCTION_READY.md` remains superseded.

## Still false outside this repository

[thirstysystems.com EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469` and still describes a README PRODUCTION READY badge. Current `main` does not wear that badge. The portal is stale of this tree.

Pytest and coverage were **not** re-run in this clone (shallow docs HEAD, no venv). Those figures stay pinned to `fdd9762`.
