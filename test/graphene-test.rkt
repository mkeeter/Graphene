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

    (let ([t (make-topolist <)])
      (topolist-insert! t 1)
      (topolist-insert! t 10)
      (topolist-insert! t 5)
      (topolist-insert! t 5)
      (check-equal? (topolist-pops! t) '(10))
      (check-equal? (topolist-pops! t) '(5 5))
      (check-equal? (topolist-pops! t) '(1)))
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

  (test-case "datum-eval! (different expression, same value)"
    (let ([d (make-datum)])
      (set-datum-expr! d "12")
      (check-true (datum-eval! d))
      (set-datum-expr! d "(+ 10 2)")
      (check-false (datum-eval! d))
      (set-datum-expr! d "(+ 10 3)")
      (check-true (datum-eval! d))))

  (test-case "datum-eval! going from error to error"
    (let ([d (make-datum)])
      ;; On the first run, the value goes from #f to an error
      (set-datum-expr! d "(+ 1 a)")
      (check-true (datum-eval! d))
      ;; On the first run, the value goes from an error to an error
      ;; On the second run, the result hasn't changed
      (set-datum-expr! d "(+ 1 b)")
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

  (test-case "Multiple forms in datum"
    (let ([d (make-datum)])
      (set-datum-expr! d "(+ 1 2)(+ 3 4)")
      (datum-eval! d)
      (check-false (datum-valid? d))))

  (test-case "datum-is-output?"
    (let ([d (make-datum)])
      (set-datum-expr! d "(output (+ 1 2))")
      (check-true (datum-is-output? d))
      (set-datum-expr! d "(+ 1 2)")
      (check-false (datum-is-output? d))
      (set-datum-expr! d "(+ 1 2")
      (check-false (datum-is-output? d))))

  (test-case "input and output commands"
    (let ([d (make-datum)])
      (set-datum-expr! d "(output (+ 1 2))")
      (datum-eval! d)
      (check-equal? (datum-result d) 3)

      (set-datum-expr! d "(input (- 1 2))")
      (datum-eval! d)
      (check-equal? (datum-result d) -1)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../src/graph.rkt")

(define-test-suite graph-tests
  (test-case "make-graph"
    (check-not-false (make-graph)))

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

  (test-case "Validity of recursive loops"
    (let ([g (make-graph)])
      (graph-insert-datum! g '(a) "(+ 1 (b))")
      (graph-insert-datum! g '(b) "(+ 1 (a))")

      (check-false (datum-valid? (graph-datum-ref g '(a))))
      (check-false (datum-valid? (graph-datum-ref g '(b))))))

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

  (test-case "Auto-eval (single level)"
    (let ([g (make-graph)])
      (graph-insert-datum! g '(a) "(+ 1 (b))")
      (graph-insert-datum! g '(b) "13")
      (graph-insert-datum! g '(c) "(+ 1 (a))")

      (check-equal? (graph-result g '(a)) 14)
      (check-equal? (graph-result g '(b)) 13)
      (check-equal? (graph-result g '(c)) 15)))

  (test-case "Change tracking"
    (let ([g (make-graph)])
      (graph-insert-datum! g '(a) "(+ 1 (b))")
      (graph-insert-datum! g '(b) "13")
      (graph-insert-datum! g '(c) "(+ 1 (a))")
      (graph-set-expr! g '(b) "20")

      (check-equal? (graph-result g '(a)) 21)
      (check-equal? (graph-result g '(b)) 20)
      (check-equal? (graph-result g '(c)) 22)))

  (test-case "Change tracking in a subgraph"
    (let ([g (make-graph)])
      (graph-insert-subgraph! g '(sub))
      (graph-insert-datum! g '(sub a) "(+ 1 (b))")
      (graph-insert-datum! g '(sub b) "13")
      (graph-insert-datum! g '(sub c) "(+ 1 (a))")
      (graph-set-expr! g '(sub b) "20")

      (check-equal? (graph-result g '(sub a)) 21)
      (check-equal? (graph-result g '(sub b)) 20)
      (check-equal? (graph-result g '(sub c)) 22)))

  (test-case "Breaking recursive loops"
    (define (setup)
      (let ([g (make-graph)])
        (graph-insert-datum! g '(a) "(+ 1 (b))")
        (graph-insert-datum! g '(b) "(+ 1 (a))")

        (check-false (datum-valid? (graph-datum-ref g '(a))))
        (check-false (datum-valid? (graph-datum-ref g '(b))))
        g))
    (let ([g (setup)])
      (graph-set-expr! g '(b) "0")
      (check-equal? (graph-result g '(a)) 1)
      (check-equal? (graph-result g '(b)) 0)
    )
    (let ([g (setup)])
      (graph-set-expr! g '(a) "0")
      (check-equal? (graph-result g '(a)) 0)
      (check-equal? (graph-result g '(b)) 1)
    ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit/text-ui)
(define-syntax-rule (run-named-tests tests)
  (begin
    (printf "~a\n    " 'tests)
    (run-tests tests)))

(exit (bitwise-ior
  (run-named-tests lookup-tests)
  (run-named-tests topolist-tests)
  (run-named-tests datum-tests)
  (run-named-tests graph-tests)
))
