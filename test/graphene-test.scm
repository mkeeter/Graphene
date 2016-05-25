#!/usr/bin/env guile
!#

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tweak the load path to find the ggspec and graphene module
(add-to-load-path (dirname (current-filename)))
(add-to-load-path (string-append (dirname (current-filename)) "/../"))

(use-modules (ggspec lib))
(use-modules (srfi srfi-1))

(use-modules (graphene lookup) (graphene hset) (graphene datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-lset-eq? a b)
    (assert-= (lambda (a b) (lset= eq? a b)) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(suite "hset.scm (hash-map based set)"
    (tests
    (test "hset-empty" env
        (assert-true (not (null? (hset-empty)))))
    (test "insert" env
        (let ((h (hset-empty)))
            (hset-insert! h 'a)
            (assert-equal (hset-list h) '(a))))
    (test "insert duplicates" env
        (let ((h (hset-empty)))
            (hset-insert! h 'a)
            (hset-insert! h 'a)
            (assert-equal (hset-list h) '(a))))
    (test "insert multiple" env
        (let ((h (hset-empty)))
            (hset-insert! h 'a)
            (hset-insert! h 'b)
            (assert-lset-eq? (hset-list h) '(a b))))
    (test "remove" env
        (let ((h (hset-empty)))
            (hset-insert! h 'a)
            (hset-remove! h 'a)
            (assert-equal (hset-list h) '())))
))

(suite "lookup.scm (bidirectional lookup table)"
    (tests
    (test "make-lookup-table" env
        (assert-true (not (null? (make-lookup-table)))))

    (test "record-lookup" env
        (let ((t (make-lookup-table)))
            (t 'record 'child 'parent)
            (assert-all
                (assert-lset-eq? (t 'forward 'child) '(parent))
                (assert-lset-eq? (t 'inverse 'child) '())
                (assert-lset-eq? (t 'inverse 'parent) '(child))
                (assert-lset-eq? (t 'forward 'parent) '()))))

    (test "clear-lookups" env
        (let ((t (make-lookup-table)))
            (t 'record 'child 'parent)
            (t 'clear 'child)
            (assert-all
                (assert-lset-eq? (t 'forward 'child) '())
                (assert-lset-eq? (t 'inverse 'child) '())
                (assert-lset-eq? (t 'inverse 'parent) '())
                (assert-lset-eq? (t 'forward 'parent) '()))))
))

(suite "datum.scm"
    (tests

    (test "Setting expr" env
    (let ((d (make-datum)))
        (assert-true (datum-set-expr! d "12"))))

    (test "Setting expr (unchanged)" env
    (let ((d (make-datum)))
        (datum-set-expr! d "12")
        (assert-false (datum-set-expr! d "12"))))

    (test "datum-eval!" env
    (let ((d (make-datum)))
        (datum-set-expr! d "12")
        (assert-all
            (assert-true (datum-eval! d (interaction-environment)))
            (assert-equal (datum-value d) 12))))

    (test "datum-eval! (constant value)" env
    (let ((d (make-datum)))
        (datum-set-expr! d "3")
        (datum-eval! d (interaction-environment))
        (datum-set-expr! d "(+ 1 2)")
        (assert-all
            (assert-false (datum-eval! d (interaction-environment)))
            (assert-equal (datum-value d) 3))))
))
