# Release

## Branch and CI Preconditions

Before tagging a release:

1. `main` is clean (`git status` has no changes).
2. CI is green (`UT` + `PT`).
3. Local verification passes:

```bash
nix develop -c sh ./ut.sh
nix develop -c sh ./pt.sh
```

## Versioning Style

Recommended tag styles already used in this repo:

- milestone/baseline tags, e.g. `baseline/pre-standalone`
- release tags, e.g. `release/v3.0.0`

Pick one style per purpose and stay consistent.

## Tagging

Create annotated tag:

```bash
git tag -a <tag-name> <commit> -m "<tag message>"
```

Examples:

```bash
git tag -a baseline/pre-standalone 4d3e285 -m "Last commit before standalone layout migration"
git tag -a release/v3.0.0 main -m "Kiln v3.0.0"
```

Push tags:

```bash
git push origin <tag-name>
# or push all tags
# git push origin --tags
```

## Build Artifacts

Nix package output:

```bash
nix build .#kiln
```

Run packaged binary:

```bash
nix run .#kiln
```

Install to profile:

```bash
nix profile install .#kiln
```

## If History Was Rewritten

If commits/tags were rewritten:

1. re-check target tag commit hashes
2. force-push branch safely

```bash
git push --force-with-lease origin main
```

3. force-update tags if needed

```bash
git push --force-with-lease --tags
```

Only do this when history rewrite is intentional and understood.
