#!/usr/bin/env pwsh
if (-not (Get-Command lua -ErrorAction SilentlyContinue))
{
	Write-Error "Lua is not installed or is not in PATH."
	Write-Error "Install Lua 5.1+ and try again."
	exit 1
}

Set-Location -LiteralPath $PSScriptRoot

lua scripts/fnl.lua @args
exit $LASTEXITCODE
