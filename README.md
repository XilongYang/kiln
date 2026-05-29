# Kiln

A standalone static-site builder.

`kiln` scans your workspace, builds post pages and index, generates search data, and subsets font files.

## Requirements

Runtime tools (must be available in `PATH`):

- `pandoc`
- `pyftsubset` (from Python `fonttools`)
- `sha256sum` (from `coreutils`)

If installed via this repo's Nix package (`.#kiln`), these runtime tools are injected automatically.

## Workspace Layout

`kiln` always uses the current working directory (`.`) as project root.

Required input/output layout:

```text
<workspace>/
├─ src/                          # input markdown posts
├─ template/
│  ├─ post.html                  # post template
│  ├─ index.html                 # index template
│  └─ component/                 # reusable template components
├─ res/
│  └─ fonts/
│     └─ cn.woff2                # source font input for subsetting
├─ post/                         # generated post html outputs
├─ temp/                         # temporary runtime files (recreated each run)
├─ .cache/                       # incremental build cache/state
├─ index.html                    # generated index page
└─ searchdb.klb                  # generated search payload
```

## Build Behavior

`kiln` performs one full build flow:

1. Recreate `temp/`.
2. Warn orphan pages in `post/`.
3. Expand templates from `template/component/`.
4. Parse markdown posts from `src/`.
5. Build `post/*.html` incrementally.
6. Build `index.html` incrementally.
7. Merge search-item artifacts into `searchdb.klb`.
8. Merge charset artifacts and run `pyftsubset`.

Font subsetting paths are fixed:

- Input: `res/fonts/cn.woff2`
- Output: `res/fonts/cn-subset.woff2`

## Incremental Rules

### Post rebuild

A post rebuilds when any condition is true:

- target `post/*.html` missing
- source mtime newer than target mtime
- source hash changed vs cached state

### Index rebuild

Index rebuilds when any condition is true:

- `index.html` missing
- metadata artifact hash changed

## Local Development

Run directly with GHC:

```bash
runghc -iSrc Src/Main.hs
```

Unit tests:

```bash
./ut.sh
```

Performance tests:

```bash
./pt.sh
```

With PT memory limit (KiB):

```bash
PT_ULIMIT_VMEM_KB=3145728 ./pt.sh
```

## Nix / Flake Usage

### 1) Build and install from this repository

Build package:

```bash
nix build .#kiln
```

Run directly:

```bash
nix run .#kiln
```

Install to user profile:

```bash
nix profile install .#kiln
```

Binary name is `kiln`.

### 2) Use as dependency in another flake

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    kiln.url = "github:XilongYang/kiln";
  };

  outputs = { self, nixpkgs, kiln, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ kiln.overlays.default ];
    };
  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = [ pkgs.kiln ];
    };
  };
}
```

Without overlay, you can also reference:

```nix
kiln.packages.${system}.kiln
```

Both styles still require `inputs.kiln` to be defined.  
Overlay just lets you use `pkgs.kiln` instead of the longer `kiln.packages.${system}.kiln`.

### 3) Install in NixOS

```nix
{
  nixpkgs.overlays = [ kiln.overlays.default ];
  environment.systemPackages = [ pkgs.kiln ];
}
```

or directly:

```nix
{
  environment.systemPackages = [ kiln.packages.${pkgs.system}.kiln ];
}
```

## Notes

- This repository intentionally does not use runtime config files.
- Path conventions are fixed by code to keep distribution simple.
- To switch actual source font, keep `res/fonts/cn.woff2` as a symlink to your desired font file.

## Commit Message Format

Use:

```text
type(scope): summary
```

Examples:

- `feat(builder): make fingerprint path configurable`
- `refactor(config): consolidate config handling`
- `test(build-judger): expand coverage`
- `docs(repo): update README and ignore rules`
- `chore(nix): bundle fonttools and brotli`

Preferred `type` values:

- `feat`
- `fix`
- `refactor`
- `test`
- `docs`
- `chore`
