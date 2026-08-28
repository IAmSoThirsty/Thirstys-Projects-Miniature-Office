# SUPERSEDED — not production ready

**This certificate is inaccurate.** It was generated 11 February 2026 from 22 tests and 32% coverage, then left in the tree after later documents claimed 1,537 tests and 99% coverage.

Do not cite this file. Current measured status: [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md), [LIMITATIONS.md](LIMITATIONS.md). Independently re-measured 28 August 2026 on **code pin** [`fdd9762`](https://github.com/IAmSoThirsty/Thirstys-Projects-Miniature-Office/commit/fdd9762af2be9ebf0aeee3bc9148b3f87a5d684a) (last `src/` / `tests/` change; docs commit `1a103bf` is identical there).

Facts that still contradict this certificate on current `main`:

- The product is an experimental Flask prototype, not a production IDE
- Application / world state is in-memory
- Python codegen is an identity transform; non-Python tests are not executed
- Fresh coverage is 7,494 / 7,749 imported statements (96.71%), and `integrated_specs/` is still omitted
- Compose has no default `SECRET_KEY`; `.env.example` still has a placeholder
- `/api/ide/*` is open unless `MO_IDE_TOKEN` is set (required only in production)
- Docker is a compose healthcheck, not a hardened stack

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
