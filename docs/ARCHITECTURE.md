# Architecture

## Scope

`kiln` is a standalone static-site builder written in Haskell.

Single entrypoint:

- `Src/Main.hs`

Core responsibilities:

- read markdown posts from `src/`
- render `post/*.html`
- render `index.html`
- aggregate `searchdb.klb`
- generate `res/fonts/cn-subset.woff2`

## Runtime Layout

Expected workspace layout is fixed by code (no runtime config file):

- `src/`
- `template/` (`post.html`, `index.html`, `component/`)
- `res/fonts/cn.woff2`
- output/cache dirs: `post/`, `temp/`, `.cache/`

## Build Pipeline

Implemented in `Src/Main.hs`:

1. Recreate `temp/` via `withTempDir`.
2. Run orphan checks (`Modules.Utils.OrphanCheck`).
3. Expand templates once (`Modules.Template`).
4. Enumerate `src/*.md` and build post plans (`Modules.BuildPlan`).
5. Execute build plans (`Modules.Builder`).
6. Build index plan.
7. Concatenate `search-item` artifacts into `searchdb.klb` (`Modules.SearchDB`).
8. Run font subsetting (`Modules.FontSubset`).

## Main Modules

- `Modules.BuildPlan`: construct post/index build plans.
- `Modules.BuildJudger`: incremental decision logic.
- `Modules.Builder`: execute build plans and emit artifacts.
- `Modules.Post.*`: markdown/meta parsing and preprocessing.
- `Modules.Index.*`: index item model and rendering.
- `Modules.Template`: component expansion.
- `Modules.SearchDB`: search payload append/merge.
- `Modules.FontSubset`: charset merge + `pyftsubset` integration.
- `Modules.Config`: centralized path constants.
- `Modules.Utils.*`: file helpers, temp dir, orphan checks, klb/string utilities.

## Incremental Model

Current incremental checks are file/artifact driven:

- posts: target missing OR source newer OR source hash changed
- index: target missing OR metadata artifact hash changed

Persistent state and artifacts are written under `.cache/`.

## Environment Model

Project is Nix-first:

- `flake.nix` defines package, devShell, runtime wrapper.
- runtime tools are provided via Nix (`pandoc`, `fonttools/pyftsubset`, `coreutils`).

Direct non-Nix execution is possible only if equivalent tools are already in `PATH`.
