#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ELN_DIR="$ROOT_DIR/eln-cache"

if [[ -d "$ELN_DIR" ]]; then
  echo "[clean-eln] removing $ELN_DIR"
  rm -rf "$ELN_DIR"
else
  echo "[clean-eln] no cache directory at $ELN_DIR"
fi

mkdir -p "$ELN_DIR"
echo "[clean-eln] recreated $ELN_DIR"
