;; Structural {|...|} parse

(local {: byte : find : lower : sub} string)

(local ws {32 true 9 true 10 true 11 true 12 true 13 true})

(fn trim-span [s from to]
  (var i from)
  (var j to)
  (while (and (<= i j) (. ws (byte s i)))
    (set i (+ i 1)))
  (while (and (<= i j) (. ws (byte s j)))
    (set j (- j 1)))
  (sub s i j))

(fn trim [s]
  (let [str (tostring (or s ""))]
    (if (or (. ws (byte str 1)) (. ws (byte str -1)))
        (trim-span str 1 (length str))
        str)))

(fn parse-level-keys [raw]
  (let [s (: (tostring (or raw "")) :gsub "%s+" "")]
    (if (find s "^%d+$")
        [(tonumber s)]
        (let [keys []
              seen {}]
          (fn add [k]
            (when (not (. seen k))
              (set (. seen k) true)
              (table.insert keys k)))

          (fn add-part [part]
            (let [(a la b lb) (part:match "^(%d+)(%a+)[^%d%a]+(%d+)(%a+)$")]
              (if (and a (= (la:upper) (lb:upper)))
                  (let [L (la:upper)]
                    (for [n (tonumber a) (tonumber b)]
                      (add (.. (tostring n) L))))
                  (let [(n L) (part:match "^(%d+)(%a+)$")]
                    (if n
                        (add (.. n (L:upper)))
                        (let [(lo hi) (part:match "^(%d+)[^%d]+(%d+)$")]
                          (if lo
                              (for [i (tonumber lo) (tonumber hi)]
                                (add i))
                              (let [num (tonumber part)]
                                (when num (add num))))))))))

          (each [part (s:gmatch "[^/]+")]
            (add-part part))
          keys))))

(fn unwrap-wikilink [inner]
  (let [name (trim (or (inner:match "^([^|]+)") inner))
        low (name:lower)]
    ;; stupid thing where it matches the column name
    (if (or (low:match "^file:") (low:match "^image:"))
        ""
        name)))

(fn clean-header-text [cell]
  (var h (trim cell))
  (when (find h "<" 1 true)
    (set h (pick-values 1 (h:gsub "<ref[^>]*>.-</ref>" "")))
    (set h (pick-values 1 (h:gsub "<ref[^>]*/>" "")))
    (set h (pick-values 1 (h:gsub "<[^>]+>" ""))))
  (when (find h "[[" 1 true)
    (set h (pick-values 1 (h:gsub "%[%[([^%]]+)%]%]" unwrap-wikilink)))
    (set h (pick-values 1 (h:gsub "%[%[" ""))))
  (when (find h "]]" 1 true)
    (set h (pick-values 1 (h:gsub "%]%]" ""))))
  (when (find h "$" 1 true)
    (set h (pick-values 1 (h:gsub "%$[^%$]+%$" ""))))
  (trim h))

(fn strip-cell-refs [cell]
  (var c (trim cell))
  (when (find c "<" 1 true)
    (set c (pick-values 1 (c:gsub "<ref[^>]*>.-</ref>" "")))
    (set c (pick-values 1 (c:gsub "<ref[^>]*/>" "")))
    (set c (trim c)))
  c)

(fn title-line? [t]
  (and (= (byte t 1) 33) (: (lower t) :find "colspan" 1 true)))

(fn header-line? [t]
  (and (= (byte t 1) 33) (not (title-line? t))))

(fn split-header-cells [line]
  ;; || -> !! in headers are undocumented on MediaWiki's docs
  ;; but are actually converted anyway
  (let [body (line:sub 2)
        (norm) (body:gsub "%|%|" "!!")
        cells []]
    (each [cell (: (.. norm "!!") :gmatch "([^!]*)!!")]
      (let [h (clean-header-text cell)]
        (when (not= h "")
          (table.insert cells h))))
    cells))

(fn split-row-cells [line]
  (let [cells []
        n (length line)]
    (var pos 2)
    (var done false)
    (while (not done)
      (let [nx (find line "||" pos true)
            stop (if nx (- nx 1) n)]
        (table.insert cells (trim-span line pos stop))
        (if nx
            (set pos (+ nx 2))
            (set done true))))
    cells))

(fn extract-title [line]
  ;; same cleanup as headers so "Sentry Stats$SREF$" -> "Sentry Stats"
  (let [name (line:match "%|(.+)$")]
    (when name (clean-header-text name))))

(fn detect-branch [title branch-map]
  (if (or (not branch-map) (not title) (= title ""))
      ""
      (or (. branch-map title) (. branch-map (title:gsub "%s+" "")) "")))

(fn parse-table [tbl-text branch-map ?cache-only]
  ;; structure without $VAR$ eval
  ;; rows share arrays with line-cells (process-table reuses them)
  ;; so never write into either
  (let [branch-map (or branch-map {})
        headers []
        rows []
        line-cells []]
    (var title nil)
    (var branch "")
    (var pending-cells nil)

    (fn flush-row []
      (when pending-cells
        (let [raw-cells pending-cells]
          (if ?cache-only
              ;; page-cache path need not cleaned cells or by-header
              (table.insert rows {:raw-cells raw-cells})
              (let [cells (icollect [_ c (ipairs raw-cells)]
                            (strip-cell-refs c))
                    by-header {}
                    n (math.min (length headers) (length raw-cells))]
                (for [i 1 n]
                  (let [h (. headers i)
                        (h-stripped) (h:gsub "%s+" "")]
                    (set (. by-header h) (. raw-cells i))
                    (set (. by-header h-stripped) (. raw-cells i))))
                (table.insert rows
                              {:raw-cells raw-cells
                               :cells cells
                               :by-header by-header
                               :level-raw (and (> (length cells) 0) (. cells 1))})))
          (set pending-cells nil))))

    ;; \n? yields a trailing "" line, a no-op
    (each [line (tbl-text:gmatch "([^\n]*)\n?")]
      ;; cells trim themselves
      (let [t (if (. ws (byte line 1)) (trim line) line)
            b (byte t 1)]
        (if (= b 33)
            (if (title-line? t)
                (do
                  (set title (extract-title t))
                  (set branch (detect-branch title branch-map)))
                (let [hs (split-header-cells t)]
                  (table.insert line-cells hs)
                  (each [_ h (ipairs hs)]
                    (table.insert headers h))))
            (= b 124)
            (let [b2 (byte t 2)]
              (if (= b2 45)
                  (flush-row)
                  (= b2 125)
                  (flush-row)
                  (let [cells (split-row-cells t)]
                    (table.insert line-cells cells)
                    (if pending-cells
                        (let [merged []]
                          (each [_ c (ipairs pending-cells)]
                            (table.insert merged c))
                          (each [_ c (ipairs cells)]
                            (table.insert merged c))
                          (set pending-cells merged))
                        (set pending-cells cells))))))))
    (flush-row)
    {:title title
     :branch branch
     :headers headers
     :rows rows
     :source tbl-text
     :line-cells line-cells}))

(fn find-table-spans [content]
  (let [out []
        text (tostring (or content ""))]
    (var i 1)
    (var done false)
    (while (not done)
      (let [start (text:find "{|" i true)]
        (if (not start)
            (set done true)
            (let [finish (text:find "|}" start true)]
              (if (not finish)
                  (set done true)
                  (let [stop (+ finish 1)]
                    (table.insert out
                                  {:start start
                                   :stop stop
                                   :text (text:sub start stop)})
                    (set i (+ stop 1))))))))
    out))

(fn find-table-blocks [content]
  (icollect [_ span (ipairs (find-table-spans content))]
    span.text))

(fn parse-all-tables [content branch-map]
  (icollect [_ span (ipairs (find-table-spans content))]
    (parse-table span.text branch-map)))

(fn index-col [table-ast index-overrides]
  (let [headers table-ast.headers
        title table-ast.title]
    (var col (and (> (length headers) 0) (. headers 1)))
    (when index-overrides
      (each [_ ov (ipairs index-overrides)]
        (when (and ov.table title ov.col
                   (or (= ov.table title)
                       (= (ov.table:gsub "%s+" "") (title:gsub "%s+" ""))))
          (set col ov.col))))
    col))

(fn row-level-keys [row index-header]
  (let [raw (or (and index-header (. row.by-header index-header)) row.level-raw
                "")]
    (parse-level-keys raw)))

{:trim trim
 :parse-level-keys parse-level-keys
 :clean-header-text clean-header-text
 :strip-cell-refs strip-cell-refs
 :split-header-cells split-header-cells
 :split-row-cells split-row-cells
 :header-line? header-line?
 :title-line? title-line?
 :extract-title extract-title
 :detect-branch detect-branch
 :parse-table parse-table
 :find-table-spans find-table-spans
 :find-table-blocks find-table-blocks
 :parse-all-tables parse-all-tables
 :index-col index-col
 :row-level-keys row-level-keys}
