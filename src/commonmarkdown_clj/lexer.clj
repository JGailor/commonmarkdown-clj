(ns commonmarkdown-clj.lexer
  (:require [commonmarkdown-clj.util :refer :all]))

(defn tokenize-chunk
  "Given a template substring returns a tuple of [match token]
   where match is the exact string that matched and token is the
   token that corresponds to the regex match.
   Example: (tokenize-chunk '{{bar}} foo') will return
   ['{{bar}}' [:symbol 'bar']]"
  [template]
  (when-match template
    ;; match symbols e.g. {{foo}}
    [thematic-break #"\A(\s{0,3}((-\s*?){3,}|(_\s*?){3,}|(\*\s*?){3,}))\s*$"]
      [(first thematic-break) [:thematic-break (first thematic-break)]]

    [atx-heading #"\A {0,3}(#{1,6})(?: (.*?)(?: #*)? *)?$"]
      [(first atx-heading) [:atx-heading [(count (second atx-heading)) (clojure.string/trim (atx-heading 2))]]]

    [literal #"\A([\s\S][\s\S]*?)$"]
      [(last literal) [:literal (last literal)]]))

(defn tokenize
  [template]
  (loop [remaining template
         tokens []]
    (if (empty? remaining)
      tokens
      (let [[str-match token] (tokenize-chunk remaining)]
        (recur (.substring remaining (count str-match))
               (conj tokens token))))))