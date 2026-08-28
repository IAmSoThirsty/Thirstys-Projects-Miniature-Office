# Language floors

**Measured 28 August 2026:** 28 directories under `floors/`. This is not “30+ working native runtimes.” A directory is not a compiler, language server, or CI-quality toolchain. Every `floors/*/README.md` now opens with a toy-floor banner.


Older docs marked every floor “Implemented.” Completeness is uneven. Some directories are a single small program plus JSON-RPC glue.

## Inventory

| Directory | Native source | Notes |
| --- | --- | --- |
| `c` | `.c` + Makefile | Toy |
| `cpp` | `.cpp` + Makefile | Toy |
| `cuda` | `.cu` + Makefile | Toy; needs NVIDIA toolchain |
| `elixir` | `.ex` + `mix.exs` | Mix modules added 28 Aug 2026; still a toy JSON-RPC floor |
| `erlang` | `.erl` | |
| `fortran` | `.f90` + Makefile | |
| `go` | `.go` + `go.mod` | |
| `haskell` | `.hs` + cabal | |
| `java` | `.java` + Maven XML; committed `.class`/`.jar` | Build artifacts in git |
| `javascript` | `.js` | |
| `kotlin` | `.kt` | |
| `matlab` | `.m` | |
| `nosql` | `.js` | Not a database engine |
| `objective-c` | `.m` / `.h` + Makefile | |
| `ocaml` | `.ml` + dune | |
| `perl` | `.pl` | |
| `php` | `.php` | |
| `powershell` | `.ps1` | |
| `python` | `.py` | |
| `ruby` | `.rb` + Gemfile | |
| `rust` | `.rs` + Cargo; committed `target/` | |
| `rust-async` | `.rs` + Cargo; committed `target/` | |
| `scala` | `.scala` + sbt | |
| `shell` | `.sh` | |
| `sql` | **Python** (`*.py`) | Not SQL |
| `swift` | `.swift` | |
| `typescript` | `.ts` | |
| `wasm` | Rust targeting wasm; committed `target/` | |

## Honest labels

- **Wrong language:** `sql` is Python.
- **Toys:** most floors are a small program plus JSON-RPC glue, not a language runtime for the office.
- **Heavy artifacts:** `rust`, `rust-async`, `wasm`, and `java` commit build outputs. That is not “production.”

`./build_floors.sh` compiles whatever toolchains are installed. Failure of an optional floor is expected on a stock Python-only machine.

Canonical status: [CLAIMS_AUDIT.md](../CLAIMS_AUDIT.md).
