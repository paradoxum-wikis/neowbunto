#!/usr/bin/env lua
-- build / CLI helpers

local util = {}

util.is_win = package.config:sub(1, 1) == "\\"
util.fennel_bin = "vendor/fennel/fennel-1.6.1"
util.minify_bin = "vendor/lua-minify/minify.lua"

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

-- stravant minify chokes on function(...); ship preload never needs varargs
function util.strip_preload_varargs(src)
	local n = 0
	local out = src:gsub("function%s*%(%s*%.%.%.%s*%)", function()
		n = n + 1
		return "function()"
	end)
	return out, n
end

function util.minify_source(src, opts)
	opts = opts or {}
	util.mkdir_p("dist")
	local prep = "dist/.minify.prep.lua"
	local tmp = "dist/.minify.out.lua"
	local body = src
	local n_vararg = 0
	if opts.strip_varargs then
		body, n_vararg = util.strip_preload_varargs(body)
	end
	body = body:gsub("^%s*\n", "")
	while body:match("^\n") do
		body = body:sub(2)
	end
	util.write(prep, body)
	local cmd = string.format(
		'lua "./%s" minify %s > %s',
		util.minify_bin,
		prep,
		tmp
	)
	if not util.exec(cmd) then
		util.die("lua-minify failed (prep kept at " .. prep .. ")")
	end
	local minified = util.read(tmp)
	os.remove(prep)
	os.remove(tmp)
	if not minified or minified == "" then
		util.die("empty minify output")
	end
	if minified:find("Tokens%[", 1, true) then
		util.die("minify wrote token dump instead of code")
	end
	return minified, n_vararg
end

function util.shell_quote(s)
	if util.is_win then
		return '"' .. tostring(s):gsub('"', '\\"') .. '"'
	end
	return "'" .. tostring(s):gsub("'", "'\\''") .. "'"
end

return util
