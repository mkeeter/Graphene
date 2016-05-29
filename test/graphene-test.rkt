#!/usr/bin/env racket
#lang racket
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

(require rackunit "../src/hset.rkt")

(define-test-suite hset-tests
    "Tests for hset.rkt"

    (check-not-false (make-hset) "make-hset")

    (test-case "Inserting a value and checking it"
        (let ([h (make-hset)])
            (hset-insert! h 'a)
            (check hset-has-key? h 'a)
            (check-false (hset-has-key? h 'b))))

    (test-case "Removing an inserted value"
        (let ([h (make-hset)])
            (hset-insert! h 'a)
            (check hset-has-key? h 'a)
            (hset-remove! h 'a)
            (check-false (hset-has-key? h 'a))))

    (test-case "Union of two hash-sets"
        (let ([a (make-hset)]
              [b (make-hset)])
            (hset-insert! a 'x)
            (hset-insert! b 'y)
            (hset-insert! b 'z)
            (hset-union! a b)
            (check hset-has-key? a 'x)
            (check hset-has-key? a 'y)
            (check hset-has-key? a 'z)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit/text-ui)
(run-tests hset-tests)
