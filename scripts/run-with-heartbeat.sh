#!/usr/bin/env bash

set -euo pipefail

interval="${1:-60}"
shift || true

if [[ "$#" -eq 0 ]]; then
  echo "usage: $0 <seconds> <command> [args...]" >&2
  exit 64
fi

cmd=("$@")
"${cmd[@]}" &
cmd_pid=$!

cleanup() {
  if kill -0 "$cmd_pid" 2>/dev/null; then
    kill "$cmd_pid" 2>/dev/null || true
    wait "$cmd_pid" 2>/dev/null || true
  fi
}

trap cleanup INT TERM

while kill -0 "$cmd_pid" 2>/dev/null; do
  sleep "$interval"
  if kill -0 "$cmd_pid" 2>/dev/null; then
    printf '[heartbeat] %s still running: %s\n' "$(date -u +'%Y-%m-%dT%H:%M:%SZ')" "${cmd[*]}"
  fi
done

wait "$cmd_pid"
