#!/usr/bin/env lua
-- tower fixtures that fail on leftover $VAR$ / <var> / throw / empty

local util = dofile("scripts/util.lua")
util.ensure_fennel()
util.mkdir_p("dist")

local path = "src/?.fnl;src/?/init.fnl;tests/?.fnl;tests/?/init.fnl"

local driver = [[
(require :mock_mw)
(local corpus (require :corpus_test))
(print "neowtext tower corpus")
(corpus.run)
(print "corpus: all clear")
]]

local tmp = "dist/.corpus-run.fnl"
util.write(tmp, driver)

local cmd = string.format(
	'%s --add-fennel-path "%s" %s',
	util.fennel_cmd(),
	path,
	tmp
)
local ok = util.exec(cmd)
os.remove(tmp)
if not ok then
	os.exit(1)
end
