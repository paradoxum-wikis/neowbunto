#!/usr/bin/env lua
-- src/init.fnl -> dist/neowtext.lua

local util = dofile("scripts/util.lua")
util.ensure_fennel()

local path = "src/?.fnl;src/?/init.fnl"
local raw_path = "dist/neowtext.raw.lua"
local out_path = "dist/neowtext.lua"

util.mkdir_p("dist")

local compile = string.format(
	'%s --require-as-include --add-fennel-path "%s" --compile src/init.fnl > %s',
	util.fennel_cmd(),
	path,
	raw_path
)
if not util.exec(compile) then
	util.die("build FAIL: fennel compile")
end

local raw = util.read(raw_path)
if not raw or raw == "" then
	util.die("build FAIL: empty fennel output: " .. raw_path)
end

local minified, n_vararg = util.minify_source(raw, { strip_varargs = true })
util.write(out_path, minified)
os.remove(raw_path)

local lines = 1
for _ in minified:gmatch("\n") do
	lines = lines + 1
end
print(string.format(
	"built %s (%d bytes, %d lines, minified; stripped %d preload vararg wrapper(s))",
	out_path,
	#minified,
	lines,
	n_vararg
))
