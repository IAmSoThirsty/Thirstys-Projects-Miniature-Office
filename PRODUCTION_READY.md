# SUPERSEDED — not production ready

**This certificate is inaccurate.** It was generated 11 February 2026 from 22 tests and 32% coverage, then left in the tree after later documents claimed 1,537 tests and 99% coverage.

Audited 28 August 2026 against commit `537c469`. See [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md) and [LIMITATIONS.md](LIMITATIONS.md).

Facts that contradict this file:

- GitHub Actions `CI - Test and Lint` has been failing, including on HEAD
- Application state is in-memory
- Code generation writes `TODO` bodies and does not run tests
- `coverage.json` omits `design_analyzer.py` and `src/core/integrated_specs/`
- Compose `SECRET_KEY` defaults to `change-this-secret-key`

The original text is kept below as a historical artifact. Do not cite it.

---

# Production Readiness Certificate (historical, 11 Feb 2026)

## Miniature Office - Cognitive IDE

**Status at the time of writing (do not trust):** claimed production ready  
**Date:** February 11, 2026  
**Version:** 0.1.0

This document claimed enterprise production standards from:

- 22 unit and integration tests
- 32% code coverage
- Docker, GitHub Actions, gunicorn, security headers

Those checks, even if they passed then, do not make the system a production IDE. They also do not match the later README numbers (1,537 tests / 99% coverage), which were also not a faithful description of the tree.

**Do not deploy this application as a production service on the basis of this file.**
