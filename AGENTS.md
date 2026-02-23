# Repository Guidelines

## Project Structure & Module Organization
- R client lives at repo root; exported APIs sit in `R/*.R`, config/caching helpers in `R/config.R` and `R/cache.R`, with package metadata in `DESCRIPTION`/`NAMESPACE`. Bundled cache skeletons live in `inst/extdata` to keep tests offline.
- R tests use testthat under `tests/testthat/test_*.R`.
- Python client resides in `python-client/templateflow`; the CLI entrypoint is `templateflow/cli.py`, docs live in `python-client/docs`, and tests sit in `python-client/templateflow/tests`.
- `python-client/pyproject.toml` stores lint/format/test settings; update `CHANGES.rst` when altering Python-facing behavior.

## Build, Test, and Development Commands
- R package: `R CMD build .` to create a tarball, `R CMD check templateflow_*.tar.gz` for full checks, and `R -q -e "testthat::test_dir('tests/testthat')"` for a quick pass.
- Python (run from `python-client/`):
```
python -m pip install -e .[tests]
python -m pytest templateflow/tests          # unit tests
python -m ruff check templateflow            # lint (line length 99, single quotes)
python -m black --check templateflow         # format gate
```

## Coding Style & Naming Conventions
- R: two-space indent; braces on the same line; snake_case objects; exported functions use the `tf_*` prefix. Add roxygen2 comments when changing APIs so `NAMESPACE` stays in sync.
- Python: Black formatting (line length 99, single quotes), isort-aligned imports, and Ruff lint rules from `pyproject.toml` (E/W/F/UP/BLE/etc.). Use snake_case for functions/modules, PascalCase for classes, and uppercase constants.

## Testing Guidelines
- Keep R tests self-contained with `tempdir()` caches as in existing specs; avoid hitting the public S3 bucket by reusing the bundled skeletons in `inst/extdata`.
- Python tests belong in `templateflow/tests` and follow `test_*.py` naming. Prefer fixtures and temp dirs for cache paths; run coverage with `python -m pytest --cov templateflow`. When touching the CLI, assert exit codes and stdout/stderr text.

## Commit & Pull Request Guidelines
- Recent history favors concise, imperative messages with prefixes such as `fix:`, `feat:`, or `rel(<version>):`; follow that style and keep each commit focused.
- PRs should include a short summary, linked issues or motivation, notes on cache/data implications, and evidence of tests run (R and Python). Add doc preview links or screenshots when changing user-facing docs or CLI output.

## Caching & Configuration Tips
- Default cache root is `~/.cache/templateflow`; override with `TEMPLATEFLOW_HOME=/path/to/cache`.
- Disable automatic updates for debugging with `TEMPLATEFLOW_AUTOUPDATE=0`; keep timeouts reasonable per `R/config.R`.
