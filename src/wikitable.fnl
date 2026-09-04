;; Structural {|...|} parse

(fn trim [s]
  (let [str (tostring (or s ""))]
    (pick-values 1 (str:match "^%s*(.-)%s*$"))))

(fn split-lines [s]
  (let [out []
        text (tostring (or s ""))]
    (var start 1)
    (for [i 1 (length text)]
      (when (= (text:sub i i) "\n")
        (table.insert out (text:sub start (- i 1)))
        (set start (+ i 1))))
    (table.insert out (text:sub start))
    out))

(fn parse-level-keys [raw]
  (let [raw-s (tostring (or raw ""))
        (s) (raw-s:gsub "%s+" "")
        keys []
        seen {}]
    (fn add [k]
      (when (and (not= k nil) (not (. seen k)))
        (set (. seen k) true)
        (table.insert keys k)))

    (fn add-part [part]
      (when (not= part "")
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
                            (when num (add num)))))))))))

    (when (not= s "")
      (each [part (s:gmatch "[^/]+")]
        (add-part part)))
    keys))

(fn unwrap-wikilink [inner]
  (let [name (trim (or (inner:match "^([^|]+)") inner))
        low (name:lower)]
    ;; File:/Image: is icon chrome, not the column name
    (if (or (low:match "^file:") (low:match "^image:"))
        ""
        name)))

(fn clean-header-text [cell]
  (var h (trim cell))
  (set h (pick-values 1 (h:gsub "<ref[^>]*>.-</ref>" "")))
  (set h (pick-values 1 (h:gsub "<ref[^>]*/>" "")))
  (set h (pick-values 1 (h:gsub "<[^>]+>" "")))
  (set h (pick-values 1 (h:gsub "%[%[([^%]]+)%]%]" unwrap-wikilink)))
  (set h (pick-values 1 (h:gsub "%[%[" "")))
  (set h (pick-values 1 (h:gsub "%]%]" "")))
  (set h (pick-values 1 (h:gsub "%$[^%$]+%$" "")))
  (trim h))

(fn strip-cell-refs [cell]
  (var c (trim cell))
  (set c (pick-values 1 (c:gsub "<ref[^>]*>.-</ref>" "")))
  (set c (pick-values 1 (c:gsub "<ref[^>]*/>" "")))
  (trim c))

(fn header-line? [t]
  (and (t:match "^!") (not (: (t:lower) :match "colspan"))))

(fn title-line? [t]
  (and (t:match "^!") (: (t:lower) :match "colspan")))

(fn row-sep? [t]
  (t:match "^|%-"))

(fn table-end? [t]
  (t:match "^|}"))

(fn data-line? [t]
  (and (t:match "^|") (not (t:match "^|}")) (not (t:match "^|%-"))))

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
  (let [s (line:sub 2)
        cells []]
    (var pos 1)
    (var done false)
    (while (not done)
      (let [nx (s:find "||" pos true)]
        (if nx
            (do
              (table.insert cells (trim (s:sub pos (- nx 1))))
              (set pos (+ nx 2)))
            (do
              (table.insert cells (trim (s:sub pos)))
              (set done true)))))
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
  (let [branch-map (or branch-map {})
        headers []
        rows []]
    (var title nil)
    (var branch "")
    (var pending-cells nil)

    (fn flush-row []
      (when (and pending-cells (> (length pending-cells) 0))
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

    (each [_ line (ipairs (split-lines tbl-text))]
      (let [t (trim line)]
        (if (title-line? t)
            (do
              (set title (extract-title t))
              (set branch (detect-branch title branch-map)))
            (header-line? t)
            (each [_ h (ipairs (split-header-cells t))]
              (table.insert headers h))
            (row-sep? t)
            (do
              (flush-row)
              (set pending-cells []))
            (table-end? t)
            (flush-row)
            (data-line? t)
            (do
              (when (not pending-cells)
                (set pending-cells []))
              (each [_ c (ipairs (split-row-cells t))]
                (table.insert pending-cells c))))))
    (flush-row)
    {:title title :branch branch :headers headers :rows rows :source tbl-text}))

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
