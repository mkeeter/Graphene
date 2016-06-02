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

  (test-case "Recording a single lookup"
    (let ([t (make-lookup)])
      (lookup-record! t 'child 'parent)
      ;; Test something here
      ))
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

(require rackunit/text-ui)
(run-tests lookup-tests)
(run-tests topolist-tests)
(run-tests datum-tests)
