(ns commonmarkdown-clj.core-test
  (:require [clojure.test :refer :all]
            [commonmarkdown-clj.core :refer :all]))

(deftest simple-thematic-headers
  (testing "Thematic Headers"
    (is (= "<hr />" (render "***")) "***")
    (is (= "<hr />" (render "---")) "---")
    (is (= "<hr />" (render "___")) "___")
    (is (not (= "<hr />" (render "+++"))) "+++")
    (is (not (= "<hr />" (render "==="))) "===")
    (is (= "<hr />" (render " ***")) " ***")
    (is (= "<hr />" (render "  ***")) "  ***")
    (is (= "<hr />" (render "   ***")) "   ***")
    (is (not (= "<hr />" (render "    ***"))) "    ***")
    (is (= "<hr />" (render "_____________________________________")) "_____________________________________")
    (is (= "<hr />" (render "- - -")) "- - -")
    (is (= "<hr />" (render " **  * ** * ** * **")) " **  * ** * ** * **")
    (is (= "<hr />" (render "-     -      -      -")) "-     -      -      -")
    (is (= "<hr />" (render "- - - -    ")) "- - - -    ")
    (is (not (= "<hr />" (render "_ _ _ _ a"))) "_ _ _ _ a")
    (is (not (= "<hr />" (render "a------"))) "a------")
    (is (not (= "<hr />" (render "---a---"))) "---a---")
    (is (not (= "<hr />" (render " *-*"))) " *-*")))


(deftest simple-code-blocks
  (testing "Code Blocks"
    (is (= "<pre><code>a simple</code></pre>" (render "    a simple")))
    (is (= "<pre><code>  a simple</code></pre>" (render "      a simple")))
    (is (= "<pre><code>a simple\n  code block</code></pre>" (render "    a simple\n      code block")))
    (is (= "<pre><code>chunk1\n\nchunk2\n\n\n\nchunk3</code></pre>" (render "    chunk1\n\n    chunk2\n  \n \n \n    chunk3")))))

(deftest simple-paragraphs
  (testing "Paragraphs"
    (is (= "<p>aaa</p>" (render "aaa")))
    (is (= "<p>aaa</p><p>bbb</p>" (render "aaa\n\nbbb")))
    (is (= "<p>aaa\nbbb</p><p>ccc\nddd</p>" (render "aaa\nbbb\n\nccc\nddd")))
    (is (= "<p>aaa\nbbb</p>" (render "  aaa\nbbb")))))

