#!/usr/bin/env lua
-- repo CLI (wrappers cd here then exec this)

local util = dofile("scripts/util.lua")
util.require_repo_root()

local function help()
	io.write([[
Usage: ./fnl <command> [args...]

Commands:
  help, -h             Show this help
  setup                Download vendor deps (Fennel, lua-minify, ...)
  build                Compile + minify src/init.fnl -> dist/neowbunto.lua
  test                 Run check + corpus
  check                Unit tests
  corpus               Corpus tests
  compile <in.fnl> [out.lua]
                       Compile one Fennel file to dist/
  run <file.fnl> ...   Run a Fennel file directly

Anything else is passed through to the actual Fennel CLI:
  ./fnl --repl
  ./fnl -e "(+ 1 2)"
]])
end

local function run_script(path)
	local cmd = 'lua "' .. path .. '"'
	if not util.exec(cmd) then
		os.exit(1)
	end
end

local function cmd_compile(args)
	if #args < 1 or #args > 2 then
		util.die("Usage: ./fnl compile input.fnl [output.lua]")
	end
	util.ensure_fennel()
	local input = args[1]
	if not util.file_exists(input) then
		local alt = "src/" .. input
		if util.file_exists(alt) then
			input = alt
		else
			util.die("compile: file not found: " .. args[1])
		end
	end
	local filename
	if args[2] then
		filename = args[2]:match("([^/\\]+)$") or args[2]
	else
		filename = (input:match("([^/\\]+)$") or input):gsub("%.fnl$", "") .. ".lua"
	end
	local output = "dist/" .. filename
	util.mkdir_p("dist")

	local compile = string.format(
		"%s --compile %s > %s",
		util.fennel_cmd(),
		util.shell_quote(input),
		util.shell_quote(output)
	)
	if not util.exec(compile) then
		util.die("compile FAIL: fennel")
	end
	local out = util.read(output)
	if not out or out == "" then
		util.die("compile FAIL: empty output")
	end
	print("compiled " .. output .. " (" .. #out .. " bytes)")
end

local function cmd_run(args)
	if #args < 1 then
		util.die("Usage: ./fnl run input.fnl [args...]")
	end
	util.ensure_fennel()
	local parts = { util.fennel_cmd() }
	for i = 1, #args do
		parts[#parts + 1] = util.shell_quote(args[i])
	end
	if not util.exec(table.concat(parts, " ")) then
		os.exit(1)
	end
end

local function cmd_fennel(args)
	util.ensure_fennel()
	local parts = { util.fennel_cmd() }
	for i = 1, #args do
		parts[#parts + 1] = util.shell_quote(args[i])
	end
	if not util.exec(table.concat(parts, " ")) then
		os.exit(1)
	end
end

local function shift(argv)
	local out = {}
	for i = 2, #argv do
		out[#out + 1] = argv[i]
	end
	return out
end

local cmd = arg[1]
if cmd == nil or cmd == "help" or cmd == "-h" then
	help()
	os.exit(0)
elseif cmd == "setup" then
	run_script("scripts/setup.lua")
elseif cmd == "build" then
	run_script("scripts/build.lua")
elseif cmd == "test" then
	run_script("scripts/test.lua")
elseif cmd == "corpus" then
	run_script("scripts/corpus.lua")
elseif cmd == "check" then
	run_script("scripts/check.lua")
elseif cmd == "compile" then
	cmd_compile(shift(arg))
elseif cmd == "run" then
	cmd_run(shift(arg))
else
	cmd_fennel(arg)
end
