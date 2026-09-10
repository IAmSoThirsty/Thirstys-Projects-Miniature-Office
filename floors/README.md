# Language floors

**Measured 28 August 2026:** 28 directories under `floors/`. This is not “30+ working native runtimes.” A directory is not a compiler, language server, or CI-quality toolchain. Every `floors/*/README.md` now opens with a toy-floor banner.

Older docs marked every floor “Implemented.” Completeness is uneven. Some directories are a small program plus JSON-RPC glue.

## Inventory

| Directory | Native source | Notes |
| --- | --- | --- |
| `c` | `.c` + Makefile | Toy |
| `cpp` | `.cpp` + Makefile | Toy |
| `cuda` | `.cu` + Makefile | Toy; needs NVIDIA toolchain. API key is `cuda_gpu` |
| `elixir` | `.ex` + `mix.exs` | Mix modules added 28 Aug 2026; still a toy JSON-RPC floor |
| `erlang` | `.erl` | |
| `fortran` | `.f90` + Makefile | |
| `go` | `.go` + `go.mod` | |
| `haskell` | `.hs` + cabal | |
| `java` | `.java` + Maven XML; committed `.class`/`.jar` | Build artifacts in git |
| `javascript` | `.js` | |
| `kotlin` | `.kt` | |
| `matlab` | `.m` | API key is `matlab_octave` |
| `nosql` | `.js` | Not a database engine |
| `objective-c` | `.m` / `.h` + Makefile | API key is `objective_c` |
| `ocaml` | `.ml` + dune | |
| `perl` | `.pl` | |
| `php` | `.php` | |
| `powershell` | `.ps1` | |
| `python` | `.py` | |
| `ruby` | `.rb` + Gemfile | |
| `rust` | `.rs` + Cargo; committed `target/` | |
| `rust-async` | `.rs` + Cargo; committed `target/` | API key is `rust_async` |
| `scala` | `.scala` + sbt | |
| `shell` | `.sh` | |
| `sql` | `.sql` schema + Python department | Toy. `schema.sql` is SQL; department logic is still Python |
| `swift` | `.swift` | |
| `typescript` | `.ts` | |
| `wasm` | Rust targeting wasm; committed `target/` | API key is `webassembly` |

## API keys are not directory names

`GET /api/floors` returns **28** `FloorSpecification` dataclasses (`get_all_floors()`). `GET /api/floors/<language>` parses `ProgrammingLanguage(language.lower())`. Independent Flask test client on code pin `fdd9762`:

| Directory under `floors/` | API enum key | `GET /api/floors/<directory>` |
| --- | --- | --- |
| `wasm` | `webassembly` | **HTTP 404** `Unknown language: wasm` |
| `cuda` | `cuda_gpu` | **HTTP 404** `Unknown language: cuda` |
| `matlab` | `matlab_octave` | **HTTP 404** `Unknown language: matlab` |
| `objective-c` | `objective_c` | **HTTP 404** `Unknown language: objective-c` |
| `rust-async` | `rust_async` | **HTTP 404** `Unknown language: rust-async` |

The other 23 directory names match the enum. `GET /api` documents this as “Get specific floor specification” and does not list the remap. That list is **not** `world.floors` (**2**: `floor-python` / `floor-javascript`).

## Honest labels

- **Wrong language:** `sql` is Python.
- **Toys:** most floors are a small program plus JSON-RPC glue, not a language runtime for the office.
- **Heavy artifacts:** `rust`, `rust-async`, `wasm`, and `java` commit build outputs. That is not “production.”
- **Name split:** five directory names 404 against the floor-spec API.

`./build_floors.sh` compiles whatever toolchains are installed. Failure of an optional floor is expected on a stock Python-only machine.

Canonical status: [CLAIMS_AUDIT.md](../CLAIMS_AUDIT.md).
