#!/usr/bin/env lua
-- vendor Fennel, lua-minify, scribunto-luacats

local is_win = package.config:sub(1, 1) == "\\"

local function exec(cmd)
	local ok = os.execute(cmd)
	if ok ~= true and ok ~= 0 then os.exit(1) end
end

if is_win then
	exec("if exist vendor rmdir /s /q vendor")
	exec("git clone --depth 1 https://github.com/t7ru/scribunto-luacats.git vendor\\scribunto-luacats")
	exec("git clone --depth 1 https://github.com/stravant/lua-minify.git vendor\\lua-minify")
	exec("if not exist vendor\\fennel mkdir vendor\\fennel")
	exec("curl -fLo vendor\\fennel\\fennel-1.6.1 https://fennel-lang.org/downloads/fennel-1.6.1")
else
	exec("rm -rf vendor")
	exec("git clone --depth 1 https://github.com/t7ru/scribunto-luacats.git vendor/scribunto-luacats")
	exec("git clone --depth 1 https://github.com/stravant/lua-minify.git vendor/lua-minify")
	exec("mkdir -p vendor/fennel")
	exec("curl -fLo vendor/fennel/fennel-1.6.1 https://fennel-lang.org/downloads/fennel-1.6.1")
	exec("chmod +x vendor/fennel/fennel-1.6.1")
end
