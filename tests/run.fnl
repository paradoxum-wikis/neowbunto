;;/usr/bin/env fennel
;; mock mw first, then every suite

(print "neowtext tests")
(require :mock_mw)

(local util (require :test_util))
(local lexer-test (require :lexer_test))
(local parser-test (require :parser_test))
(local splice-test (require :parser_splice_test))
(local eval-test (require :eval_test))
(local config-test (require :config_test))
(local wikitable-test (require :wikitable_test))
(local tablecache-test (require :tablecache_test))
(local render-test (require :render_test))
(local entry-test (require :entry_test))
(local corpus-test (require :corpus_test))

(print "\n[lexer]")
(lexer-test.run)

(print "\n[parser]")
(parser-test.run)

(print "\n[parser splice]")
(splice-test.run)

(print "\n[eval]")
(eval-test.run)

(print "\n[config]")
(config-test.run)

(print "\n[wikitable]")
(wikitable-test.run)

(print "\n[tablecache]")
(tablecache-test.run)

(print "\n[render]")
(render-test.run)

(print "\n[entry]")
(entry-test.run)

(print "\n[exp / Level / #expr]")
((. (require :exp_level_test) :run))

(util.summary)

(print "\n[corpus / towers]")
(corpus-test.run)
