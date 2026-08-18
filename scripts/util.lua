#!/usr/bin/env lua
-- build / CLI helpers

local util = {}

util.is_win = package.config:sub(1, 1) == "\\"
util.fennel_bin = "vendor/fennel/fennel-1.6.1"
util.dumbparser_bin = "vendor/dumbluaparser/dumbParser.lua"

function util.exec(cmd)
	local ok = os.execute(cmd)
	return ok == true or ok == 0
end

function util.die(msg)
	io.stderr:write(msg .. "\n")
	os.exit(1)
end

function util.read(path)
	local f = io.open(path, "r")
	if not f then
		return nil
	end
	local s = f:read("*a")
	f:close()
	return s
end

function util.write(path, s)
	local f = assert(io.open(path, "w"), "write " .. path)
	f:write(s)
	f:close()
end

function util.file_exists(path)
	local f = io.open(path, "r")
	if f then
		f:close()
		return true
	end
	return false
end

function util.at_repo_root()
	return util.file_exists("scripts/fnl.lua") and util.file_exists("src/init.fnl")
end

function util.require_repo_root()
	if not util.at_repo_root() then
		util.die(
			"run from the neowbunto repo root (use ./fnl.ps1 or ./fnl.sh so cwd is set)"
		)
	end
end

function util.ensure_fennel()
	util.require_repo_root()
	if not util.file_exists(util.fennel_bin) then
		util.die("Fennel isn't installed yet, run: ./fnl setup")
	end
end

function util.fennel_cmd()
	if util.is_win then
		return 'lua "./' .. util.fennel_bin .. '"'
	end
	return "./" .. util.fennel_bin
end

function util.mkdir_p(dir)
	if util.is_win then
		if not util.exec('if not exist "' .. dir .. '" mkdir "' .. dir .. '"') then
			util.die("mkdir failed: " .. dir)
		end
	else
		if not util.exec("mkdir -p " .. dir) then
			util.die("mkdir failed: " .. dir)
		end
	end
end

function util.ensure_dumbparser()
	util.require_repo_root()
	if not util.file_exists(util.dumbparser_bin) then
		util.die("DumbLuaParser missing: " .. util.dumbparser_bin .. " (run: ./fnl setup)")
	end
end

function util.minify_source(src, opts)
	opts = opts or {}
	util.ensure_dumbparser()
	local parser = dofile(util.dumbparser_bin)
	local ast, err = parser.parse(src)
	if not ast then
		util.die("dumbParser parse failed: " .. tostring(err))
	end
	local stats = parser.minify(ast, opts.optimize or false)
	local minified = parser.toLua(ast)
	if not minified or minified == "" then
		util.die("dumbParser toLua failed")
	end
	return minified, stats
end

function util.shell_quote(s)
	if util.is_win then
		return '"' .. tostring(s):gsub('"', '\\"') .. '"'
	end
	return "'" .. tostring(s):gsub("'", "'\\''") .. "'"
end

return util
