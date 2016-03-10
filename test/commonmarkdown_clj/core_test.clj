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

