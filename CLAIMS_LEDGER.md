# Claims ledger (machine-readable companion)

Canonical prose remains [CLAIMS_AUDIT.md](CLAIMS_AUDIT.md). This file is the
score that must sum.

**Commit measured:** `fe9cdf1` (28 August 2026)  
**Status:** experimental prototype — not production-ready  
**Rule:** a claim is true only if the tree implements it.

## Score

| Holds | Partial | Inflated | False | Total |
| --- | --- | --- | --- | --- |
| 6 | 7 | 2 | 3 | 18 |

The previous score line in CLAIMS_AUDIT (`8 hold / 6 partial / 2 inflated / 3 false`) sums to 19. The 18-row table cannot support that.

VR is **Partial**, not Holds: there is no WebXR and no PWA.

See [claims.json](claims.json) for the same table as JSON.

Ecosystem claim [EC-013](https://www.thirstysystems.com/claims) still pins `LIMITATIONS.md` at `537c469`. That commit is the smoking-gun contradictions, not current `main`. The README no longer badges PRODUCTION READY.

## Remaining product gaps (unchanged)

- Template codegen with `TODO` bodies; generated tests are not executed
- 28 toy floors; SQL floor is Python
- In-memory unsigned audit chain
- Security CI uses `|| true`
- Compose `SECRET_KEY` placeholder
