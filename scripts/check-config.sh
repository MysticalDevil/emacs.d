#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BYTE_COMPILE=0

if [[ "${1:-}" == "--byte-compile" ]]; then
  BYTE_COMPILE=1
fi

echo "[check] loading config in batch mode..."
emacs --batch -Q \
  -l "$ROOT_DIR/early-init.el" \
  -l "$ROOT_DIR/init.el"

if [[ "$BYTE_COMPILE" -eq 1 ]]; then
  echo "[check] byte-compiling lisp/*.el..."
  emacs --batch -Q \
    --eval "(setq byte-compile-error-on-warn nil)" \
    -f batch-byte-compile "$ROOT_DIR"/lisp/*.el "$ROOT_DIR"/init.el
fi

echo "[check] done"
