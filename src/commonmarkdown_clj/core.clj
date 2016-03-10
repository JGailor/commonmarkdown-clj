(ns commonmarkdown-clj.core
  (:require [commonmarkdown-clj.util :refer :all]
            [commonmarkdown-clj.lexer :refer :all]))

(defn- unexpected-token-exception
  [token]
  (new IllegalArgumentException
    (str "Unexpected token " token)))

;; execute define-is-token-funcs to actually define the given functions:
;; is-literal, is-symbol, etc.
(define-is-token-funcs :literal :thematic-break :atx-heading :l1-setext-heading
                       :l2-setext-heading :blank-line :paragraph)

(defn- build-ast
  "Builds an abstract syntax tree given a list of tokens as produced by tokenize"
  [tokens]
  (loop [tokens tokens
         ast []]
    (if (empty? tokens)
      ast
      (let [token (first tokens)]
        (cond
          (is-thematic-break token)
            (recur (rest tokens)
                   (conj ast token))
          (is-atx-heading token)
            (recur (rest tokens)
                   (conj ast token))
          (is-l1-setext-heading token)
            (recur (rest tokens)
                   (conj ast token))
          (is-l2-setext-heading token)
            (recur (rest tokens)
                    (conj ast token))
          (is-blank-line token)
            (recur (rest tokens)
                   (conj ast tokens))
          (is-paragraph token)
            (recur (rest tokens)
                    (conj ast tokens))
          :else
            (throw (unexpected-token-exception token)))))))

(defn eval-tree
  "Evaluates a compiled template as a tree with the given context"
  [tree ctx]
  (cond (is-thematic-break tree)
          "<hr />"

        (is-atx-heading tree)
          (str "<h" (first (second tree)) ">" (second (second tree)) "</h" (first (second tree)) ">")

        (is-l1-setext-heading tree)
          (str "<h1>" (second tree) "</h1>")

        (is-l2-setext-heading tree)
          (str "<h2>" (second tree) "</h2>")

        (is-blank-line tree)
          (str "")

        (is-paragraph tree)
          (str "<p>" (second tree) "</p>")

        ;; Only executed once: the first time eval-tree is called, no subsequent
        ;; recursive call will go through this branch
        (coll? tree)
          (apply str (map #(eval-tree % ctx) tree))))

(defn parse
  "Parses a template string and returns a compiled tree representation of the template.
   You can later use (eval-tree tree context) to render a compiled template with a given
   context."
  [string]
  (-> (tokenize string)
      build-ast))

(defn render
  "Compiles and evaluates the template with the given context"
  ([template] (render template {}))
  ([template ctx] (eval-tree (parse template) ctx)))
