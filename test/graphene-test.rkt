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

  (test-case "lookup-downstream?"
    (let ([t (make-lookup)])
      (lookup-record! t 'child 'parent)
      (lookup-record! t 'grandchild 'child)

      (check-true (lookup-downstream? t 'child 'child))
      (check-true (lookup-downstream? t 'child 'parent))
      (check-true (lookup-downstream? t 'grandchild 'parent))

      (check-false (lookup-downstream? t 'parent 'child))
      (check-false (lookup-downstream? t 'parent 'grandchild))
      (check-false (lookup-downstream? t 'child 'grandchild))))

  (test-case "lookup-inverse->list"
    (let ([t (make-lookup)])
      (lookup-record! t 'child 'parent)
      (check-equal? (lookup-inverse->list t 'parent) '(child))
      (check-equal? (lookup-inverse->list t 'child) '())))

  (test-case "lookup-record! multiple calls"
    (let ([t (make-lookup)])
      (lookup-record! t 'child 'parent)
      (lookup-record! t 'child 'parent)))

  (test-case "lookup-clear!"
    (let ([t (make-lookup)])
      (lookup-record! t 'child 'parent)
      (lookup-clear! t 'child)
      (check-false (lookup-downstream? t 'child 'parent))
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

  (test-case "datum-result"
    (let ([d (make-datum)])
      (set-datum-expr! d "12")
      (datum-eval! d)
      (check-equal? (datum-result d) 12)

      (set-datum-expr! d "(+ 1 2)")
      (datum-eval! d)
      (check-equal? (datum-result d) 3)))

  (test-case "datum-valid?"
    (let ([d (make-datum)])
      (set-datum-expr! d "12")
      (datum-eval! d)
      (check-true (datum-valid? d))

      (set-datum-expr! d "(+ 1 2")
      (datum-eval! d)
      (check-false (datum-valid? d))))
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
    (let ([g (make-graph)])
      (graph-insert-datum! g '(x) "12")
      (check-equal? (eval '(+ 1 (x)) (graph-env g '(dummy))) 13)))

  (test-case "graph-frozen and friends"
    (let ([g (make-graph)])
      (check-false (graph-frozen? g))
      (graph-freeze! g)
      (check-true (graph-frozen? g))
      (graph-unfreeze! g)
      (check-false (graph-frozen? g))))

  (test-case "Inserting a subgraph"
    (let ([g (make-graph)])
      (check-exn exn:fail? (lambda () (graph-sub-ref g '(a))))
      (check-exn exn:fail? (lambda () (graph-sub-ref g '(a b))))
      (graph-insert-subgraph! g '(a))

      (check-not-false (graph-sub-ref g '(a)))
      (check-exn exn:fail? (lambda () (graph-datum-ref g '(a))))

      (graph-insert-subgraph! g '(a b))
      (check-not-false (graph-sub-ref g '(a b)))
      (check-exn exn:fail? (lambda () (graph-datum-ref g '(a b))))))


  (test-case "Inserting datums"
    (let ([g (make-graph)])
      (check-exn exn:fail? (lambda () (graph-datum-ref g '(a))))
      (check-exn exn:fail? (lambda () (graph-datum-ref g '(a b))))

      (graph-insert-datum! g '(a) "(+ 1 2)")
      (check-not-false (graph-datum-ref g '(a)))
      (check-exn exn:fail? (lambda () (graph-sub-ref g '(a))))

      (graph-insert-subgraph! g '(x))
      (graph-insert-datum! g '(x a) "3")
      (check-not-false (graph-datum-ref g '(x a)))))
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
