#|
    Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>

    This file is part of Graphene.

    Graphene is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    Graphene is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Graphene.  If not, see <http://www.gnu.org/licenses/>.
|#
#lang racket

(require "datum.rkt" "graph.rkt" "lookup.rkt")
(provide graph->dot)

(define (format-indented indent fmt . args)
  (let* ([indent (make-string indent #\ )]
         [newline (string-append "\n" indent)]
         [fmt (string-replace (string-append indent fmt) "\n" newline)])
  (apply format (cons fmt args))))

(define (id->string id)
  (string-join (map symbol->string id) "_"))

(define (format-exn exn)
  (let recurse ([str (exn-message exn)]
                [matches '(("<" "&lt;") (">" "&gt;") ("&" "&amp;")
                           ("\n" "<br/>")
                           ("UNKNOWN::0: " "")
                           ("`" "'"))])
    (if (empty? matches) str
      (recurse (string-replace str (caar matches) (cadar matches))
               (cdr matches)))))

(define (datum->dot id d [indent 0])
  ;; id is the full path to the datum d
  (let ([name (id->string id)]
        [res (datum-result d)])
    (format-indented indent "~a [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\">
    <tr><td>~a</td></tr>
    <tr><td balign=\"left\" port=\"~a\">~a</td></tr>
    <tr><td bgcolor=\"~a\" balign=\"left\">~a</td></tr></table>>];"
      name
      (last id)
      name
      (string-replace (datum-expr d) "\n" "<br/>")
      (if (exn:fail? res) "#ffdddd" "#ffffff")
      (if (exn:fail? res) (format-exn res) res)
      )))

(define (subgraph->dot g [name '()] [indent 2])
  (string-join
    (hash-map g
      (lambda (k v)
        (let ([id (append name (list k))])
        (cond [(datum? v) (datum->dot id v indent)]
              [(hash? v)
                (format-indented indent "subgraph cluster_~a {
  label=\"~a\"
~a
}"
                (id->string id) k (subgraph->dot v id (+ indent 2)))
              ]))))
    "\n"))

(define (lookup->dot g)
  (apply string-append (hash-map (lookup-forward (graph-lookup g))
      (lambda (k v)
          (apply string-append (set-map v
            (lambda (v)
              (if (graph-has-datum? g v)
                (string-append (id->string v) " -> " (id->string k) ";\n")
                ""))
            ))
        ))))

(define (graph->dot g)
  (format "digraph {
  rankdir=LR;
  node [shape=plaintext, fontname=\"Consolas\"]
  graph [fontname=\"Consolas\",
         dpi=150,
         bgcolor=transparent]
~a
~a}"
  (subgraph->dot (graph-sub-ref g '()))
  (lookup->dot g)))

(define g (make-graph))
(graph-insert-datum! g '(a) "(+ 1 2)")
(graph-insert-subgraph! g '(sub))
(graph-insert-subgraph! g '(sub super))
(graph-insert-datum! g '(sub a) "(input (+ (c) 2))")
(graph-insert-datum! g '(sub super q) "12")
(graph-insert-datum! g '(sub b) "(+ 1")
(graph-insert-datum! g '(sub c) "(+ 1 \"omg\")")
(graph-insert-datum! g '(b) "(range 10)")
(graph-insert-datum! g '(c)
"(let ([x 10]
      [y 20])
  (+ x y))")
(display (graph->dot g))
(newline)
(display (format-graph g))
(newline)
