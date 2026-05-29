# Kiln Builder

A standalone Haskell static-site builder used by the Kiln blog pipeline.

## What It Does

The builder reads markdown posts from `src/` and produces site artifacts:

- Per-post HTML pages in `post/`
- Site index page `index.html`
- Search database `searchdb.klb`
- Subset CJK font `res/fonts/SourceHanSerifCN-Subset.woff2`

## Runtime Environment

Required tools:

- GHC / `runghc`
- `pandoc`
- `pyftsubset` (from Python `fonttools`)
- `brotli` (required by some fonttools setups)

## Repository Layout

```text
.
├─ Src/                         # Builder source code (entry: Src/Main.hs)
├─ Test/                        # Unit tests (UT) and performance tests (PT)
├─ kiln.conf                    # Runtime config file
├─ ut.sh                        # UT runner
└─ pt.sh                        # PT runner
```

Runtime workspace/files (under `rootPath`, default `.`):

```text
<rootPath>/
├─ src/                         # Build input posts
├─ template/                    # Build input templates
├─ res/                         # Static resources (fonts, etc.)
├─ post/                        # Generated post html
├─ temp/                        # Temporary files during one run
├─ .cache/                      # Incremental states/artifacts
├─ index.html                   # Generated index
└─ searchdb.klb                 # Generated search database
```

## Build Flow

Execution starts from `Src/Main.hs`.

1. Recreate the temp workspace (`temp/`).
2. Warn about orphan pages in `post/` with no matching source markdown.
3. Expand template placeholders from `template/component/`.
4. Parse each markdown file in `src/`:
   - front matter: `title`, `author`, `date`
   - abstract/body split at the first `## ` heading
5. Build post pages (incremental).
6. Build `index.html` from metadata artifacts (incremental).
7. Merge search-item artifacts into `searchdb.klb`.
8. Merge charset artifacts and run `pyftsubset` for font subsetting.

## Incremental Build Rules

### Post page rebuild

A post is rebuilt when any condition is true:

- target `post/*.html` is missing
- source markdown mtime is newer than target
- source hash changed vs saved state
- builder source hash changed

### Index rebuild

The index is rebuilt when any condition is true:

- `index.html` is missing
- metadata artifact hash changed
- builder source hash changed

## Config

`rootPath` resolution priority:

1. `KILN_ROOT_PATH`
2. `KILN_CONFIG_PATH` (or default `kiln.conf`) with `rootPath=...`
3. default `.`

## Usage

### Build

```bash
runghc -iSrc Src/Main.hs
```

### Unit Tests

```bash
./ut.sh
```

### Performance Tests

```bash
./pt.sh
```

Set PT memory limit (KiB):

```bash
PT_ULIMIT_VMEM_KB=3145728 ./pt.sh
```

## Notes

- PT script prepares performance fixtures before running tests.
- Builder state and artifact hashes are stored under `.cache/`.
- Generated outputs are intended to be deterministic for identical inputs and toolchain versions.
