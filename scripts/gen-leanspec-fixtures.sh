#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LEANSPEC_DIR="${SCRIPT_DIR}/../leanSpec"

if [ ! -d "${LEANSPEC_DIR}" ]; then
  echo "Error: leanSpec submodule not found at ${LEANSPEC_DIR}" >&2
  echo "Run: git submodule update --init" >&2
  exit 1
fi

if ! command -v uv &>/dev/null; then
  echo "Error: uv not found. Install with: pip install uv" >&2
  exit 1
fi

cd "${LEANSPEC_DIR}"
uv sync
uv run fill --fork=devnet --clean --scheme=test

echo "Fixtures generated at: ${LEANSPEC_DIR}/fixtures/"
