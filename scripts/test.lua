#!/usr/bin/env lua
-- unit suite then hard tower corpus

local util = dofile("scripts/util.lua")
util.ensure_fennel()

local path = "src/?.fnl;src/?/init.fnl;tests/?.fnl;tests/?/init.fnl"
local cmd = string.format(
	'%s --add-fennel-path "%s" tests/run.fnl',
	util.fennel_cmd(),
	path
)
if not util.exec(cmd) then
	os.exit(1)
end
