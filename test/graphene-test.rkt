#!/usr/bin/env racket
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

(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../src/lookup.rkt")

(define-test-suite lookup-tests
  (test-case "make-lookup"
    (check-not-false (make-lookup)))

  (test-case "is-downstream?"
    (let ([t (make-lookup)])
      (lookup-record! t 'child 'parent)
      (lookup-record! t 'grandchild 'child)

      (check-true (is-downstream? t 'child 'child))
      (check-true (is-downstream? t 'child 'parent))
      (check-true (is-downstream? t 'grandchild 'parent))

      (check-false (is-downstream? t 'parent 'child))
      (check-false (is-downstream? t 'parent 'grandchild))
      (check-false (is-downstream? t 'child 'grandchild))))

  (test-case "lookup-inverse->list"
    (let ([t (make-lookup)])
      (lookup-record! t 'child 'parent)
      (check-equal? (lookup-inverse->list t 'parent) '(child))
      (check-equal? (lookup-inverse->list t 'child) '())))

  (test-case "lookup-clear!"
    (let ([t (make-lookup)])
      (lookup-record! t 'child 'parent)
      (lookup-clear! t 'child)
      (check-false (is-downstream? t 'child 'parent))
      (check-equal? (lookup-inverse->list t 'parent) '())))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../src/topolist.rkt")

(define-test-suite topolist-tests
  (test-case "make-topolist"
    (check-not-false (make-topolist <)))

  (test-case "topolist-empty?"
    (let ([t (make-topolist <)])
      (check-true (topolist-empty? t))
      (topolist-insert! t 1)
      (check-false (topolist-empty? t))))

  (test-case "topolist-insert!"
    (let ([t (make-topolist <)])
      (topolist-insert! t 1)
      (topolist-insert! t 10)
      (topolist-insert! t 5)
      (check-equal? (topolist-pop! t) 10)
      (check-equal? (topolist-pop! t) 5)
      (check-equal? (topolist-pop! t) 1)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../src/datum.rkt")

(define-test-suite datum-tests
  (test-case "make-datum"
    (check-not-false (make-datum)))

  (test-case "set-datum-expr!"
    (let ([d (make-datum)])
      (set-datum-expr! d "12")
      (check-equal? (datum-expr d) "12")))

  (test-case "datum-eval!"
    (let ([d (make-datum)])
      (set-datum-expr! d "12")
      ;; On the first run, the value goes from #f to 12
      (check-true (datum-eval! d))
      ;; On the second run, the result hasn't changed
      (check-false (datum-eval! d))))

  (test-case "datum-value"
    (let ([d (make-datum)])
      (set-datum-expr! d "12")
      (datum-eval! d)
      (check-equal? (datum-value d) 12)

      (set-datum-expr! d "(+ 1 2)")
      (datum-eval! d)
      (check-equal? (datum-value d) 3)))

  (test-case "datum-error"
    (let ([d (make-datum)])
      (set-datum-expr! d "12")
      (datum-eval! d)
      (check-false (datum-error d))

      (set-datum-expr! d "(+ 1 2")
      (datum-eval! d)
      (check-not-false (datum-error d))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../src/graph.rkt")

(define-test-suite graph-tests
  (test-case "make-graph"
    (check-not-false (make-graph)))

  (test-case "graph-env"
    (let* ([g (make-graph)]
           [env (graph-env g '(dummy))])
      (check-equal? (eval '(+ 1 2) env) 3)))

  (test-case "graph-env datum lookup"
    (let* ([g (make-graph)])
      (graph-insert-datum! g '(x) "12")
      (check-equal? (eval '(+ 1 (x)) (graph-env g '(dummy))) 13)))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit/text-ui)
(define-syntax-rule (run-named-tests tests)
  (begin
    (display 'tests)(newline)(display "  ")
    (run-tests tests)))

(exit (bitwise-ior
  (run-named-tests lookup-tests)
  (run-named-tests topolist-tests)
  (run-named-tests datum-tests)
  (run-named-tests graph-tests)
))
