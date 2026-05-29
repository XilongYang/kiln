# Testing

## Test Types

- UT: module-level behavior checks.
- PT: performance-oriented checks + profiling HTML report generation.

## Official Entrypoints

Use these scripts as the canonical interface:

- `./ut.sh`
- `./pt.sh`

Both run with:

- `set -eu`
- `runghc -iSrc -i.`

## Commands

Inside Nix dev shell:

```bash
./ut.sh
./pt.sh
```

Or one-shot without entering shell:

```bash
nix develop -c sh ./ut.sh
nix develop -c sh ./pt.sh
```

PT memory limit override (KiB):

```bash
PT_ULIMIT_VMEM_KB=3145728 ./pt.sh
```

Default PT memory limit is set in `pt.sh` to `5242880`.

## Test Structure

- UT runner: `Test/UT/RunTest.hs`
- PT runner: `Test/PT/RunPerf.hs`
- shared framework: `Test/Framework/*`
- PT helper scripts: `Test/PT/scripts/*`

Runners set `TEST_ACTION_STDOUT_LOG`:

- UT -> `ut.log`
- PT -> `pt.log`

PT also tries to generate profiling HTML via `Test/PT/ProfilingReport.hs`.

## CI Behavior

CI is defined in `.github/workflows/ci.yml`:

- triggers: `push` on `main`, `pull_request`
- steps: install Nix, enable cache, run `ut.sh`, run `pt.sh`

## Failure Policy

- UT: any failing suite exits non-zero.
- PT: failing perf suite exits non-zero.
- PT profiling report generation failure is logged; perf suite result still controls pass/fail.
