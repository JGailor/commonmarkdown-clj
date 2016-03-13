(ns commonmarkdown-clj.lexer
  (:require [commonmarkdown-clj.util :refer :all]
            [clojure.string :as s]))

(defn tokenize-chunk
  "Given a template substring returns a tuple of [match token]
   where match is the exact string that matched and token is the
   token that corresponds to the regex match.
   Example: (tokenize-chunk '{{bar}} foo') will return
   ['{{bar}}' [:symbol 'bar']]"
  [template]
  (when-match template
    ;; match symbols e.g. {{foo}}
    [l1-setext-heading #"\A(( {0,3}[^ ](.+?)\n)+) {0,3}(=+) *(\n|$)"]
      [(first l1-setext-heading) [:l1-setext-heading (s/replace (second l1-setext-heading) #"\n" " ")]]

    [l2-setext-heading #"\A(( {0,3}[^ ](.+?)\n)+) {0,3}(-+) *(\n|$)"]
      [(first l2-setext-heading) [:l2-setext-heading (s/replace (second l2-setext-heading) #"\n" " ")]]

    [thematic-break #"\A(\s{0,3}((-\s*?){3,}|(_\s*?){3,}|(\*\s*?){3,}))\s*(\n|$)"]
      [(first thematic-break) [:thematic-break (second thematic-break)]]

    [atx-heading #"\A {0,3}(#{1,6})(?: (.*?)(?: #*)? *)?(\n|$)"]
      [(first atx-heading) [:atx-heading [(count (second atx-heading)) (s/trim (atx-heading 2))]]]

    [code-block #"\A(( {4,}.+\n?)(( {4,}.+\n?)| *\n)*)(?:\n|$)"]
      [(first code-block) [:code-block (s/replace (second code-block) #"(\A|\n|(\r\n)) {1,4}" "$1")]]

    [blank-line #"\A\n"]
      [(first blank-line) [:blank-line (first blank-line)]]

    [paragraph #"\A((.+\n?)+)(?:\n\n|$)"]
      [(first paragraph) [:paragraph (s/trim (second paragraph))]]))

(defn tokenize
  [template]
  (loop [remaining template
         tokens []]
    (if (empty? remaining)
      tokens
      (let [[str-match token] (tokenize-chunk remaining)]
        (recur (.substring remaining (count str-match))
               (conj tokens token))))))