#!/usr/bin/env lua
-- ship file: luac -p, no coroutines, etc.

local util = dofile("scripts/util.lua")
util.require_repo_root()

local function which(name)
	local cmd = util.is_win and ("where " .. name .. " 2>nul")
		or ("command -v " .. name .. " 2>/dev/null")
	local p = io.popen(cmd)
	if not p then
		return nil
	end
	local out = p:read("*l")
	p:close()
	if out and out ~= "" then
		return out
	end
	return nil
end

if not util.exec("lua scripts/build.lua") then
	io.stderr:write("check: build failed\n")
	os.exit(1)
end

local f = io.open("dist/neowbunto.lua", "r")
if not f then
	io.stderr:write("check: dist/neowbunto.lua missing\n")
	os.exit(1)
end
local src = f:read("*a")
f:close()

local failed = false

local luac = which("luac5.1") or which("luac")
if not luac then
	io.stderr:write("check FAIL: neither luac5.1 nor luac found in PATH\n")
	failed = true
else
	local label = (luac:find("5%.1") or luac:find("luac5.1")) and "luac5.1" or "luac"
	local exe = luac:gsub('"', "")
	if util.exec(string.format('"%s" -p dist/neowbunto.lua', exe)) then
		print("check OK: " .. label .. " -p dist/neowbunto.lua")
	else
		io.stderr:write("check FAIL: " .. label .. " -p dist/neowbunto.lua\n")
		failed = true
	end
end

if src:lower():find("coroutine", 1, true) then
	io.stderr:write("check FAIL: compiled output contains 'coroutine'\n")
	failed = true
else
	print("check OK: no coroutine references")
end

local preloads = {}
for name in src:gmatch("package%.preload%[%s*[\"']([^\"']+)[\"']%s*%]") do
	preloads[name] = true
end
for name in src:gmatch("package%.preload%.([%a_][%w_]*)%s*=") do
	preloads[name] = true
end

local requires = {}
for name in src:gmatch("require%(%s*[\"']([^\"']+)[\"']%s*%)") do
	requires[#requires + 1] = name
end

local bad = {}
local external = {}
for _, name in ipairs(requires) do
	if name:find(":", 1, true) or name:match("^Module") then
		external[#external + 1] = name
	elseif not preloads[name] then
		bad[#bad + 1] = name
	end
end

local n_preload = 0
for _ in pairs(preloads) do
	n_preload = n_preload + 1
end

if #external > 0 then
	io.stderr:write("check FAIL: live wiki require(s) left in dist (must be self-contained):\n")
	for _, name in ipairs(external) do
		io.stderr:write("  " .. name .. "\n")
	end
	failed = true
elseif #bad > 0 then
	io.stderr:write("check FAIL: require() without package.preload (not inlined):\n")
	for _, name in ipairs(bad) do
		io.stderr:write("  " .. name .. "\n")
	end
	failed = true
else
	print(
		string.format(
			"check OK: self-contained, %d local module(s) inlined via package.preload",
			n_preload
		)
	)
end

if failed then
	os.exit(1)
end
print("check: all passed")
