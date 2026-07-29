#!/bin/sh
if ! command -v lua >/dev/null 2>&1; then
	printf "Lua is not installed or is not in PATH.\nInstall Lua 5.1+ and try again.\n" >&2
	exit 1
fi

ROOT=$(CDPATH= cd -- "$(dirname "$0")" && pwd) || exit 1
cd "$ROOT" || exit 1

exec lua scripts/fnl.lua "$@"
