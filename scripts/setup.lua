#!/usr/bin/env lua
-- vendor scribunto-luacats, DumbLuaParser, Fennel

local is_win = package.config:sub(1, 1) == "\\"

local pins = {
	scribunto  = "0.1.1",
	dumbparser = "2.3.0",
	fennel     = "1.6.1",
}

local function exec(cmd)
	local ok = os.execute(cmd)
	if ok ~= true and ok ~= 0 then os.exit(1) end
end

local function clone(url, tag, dir)
	local path = is_win and (dir:gsub("/", "\\")) or dir
	exec("git -c advice.detachedHead=false clone --depth 1 --branch "
		.. tag .. " " .. url .. " " .. path)
end

if is_win then
	exec("if exist vendor rmdir /s /q vendor")
	clone("https://github.com/t7ru/scribunto-luacats.git", pins.scribunto, "vendor/scribunto-luacats")
	clone("https://github.com/ReFreezed/DumbLuaParser.git", pins.dumbparser, "vendor/dumbluaparser")
	exec("if not exist vendor\\fennel mkdir vendor\\fennel")
	exec("curl -fLo vendor\\fennel\\fennel-" .. pins.fennel
		.. " https://fennel-lang.org/downloads/fennel-" .. pins.fennel)
else
	exec("rm -rf vendor")
	clone("https://github.com/t7ru/scribunto-luacats.git", pins.scribunto, "vendor/scribunto-luacats")
	clone("https://github.com/ReFreezed/DumbLuaParser.git", pins.dumbparser, "vendor/dumbluaparser")
	exec("mkdir -p vendor/fennel")
	exec("curl -fLo vendor/fennel/fennel-" .. pins.fennel
		.. " https://fennel-lang.org/downloads/fennel-" .. pins.fennel)
	exec("chmod +x vendor/fennel/fennel-" .. pins.fennel)
end
