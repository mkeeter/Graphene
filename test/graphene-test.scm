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

(use-modules (graphene lookup)
             (graphene hset)
             (graphene datum)
             (graphene graph)
             (graphene topolist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-lset-eq? a b)
    (assert-same (lambda (a b) (lset= eq? a b)) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(suite "hset.scm (hash-map based set)"
    (tests
    (test "hset-empty" env
        (assert-true (not (null? (hset-empty)))))
    (test "insert" env
        (let ((h (hset-empty)))
            (hset-insert! h 'a)
            (assert-equal (hset-list h) '(a))))
    (test "hset-contains" env
        (let ((h (hset-empty)))
            (hset-insert! h 'a)
            (assert-all
                (assert-true (hset-contains? h 'a))
                (assert-true (not (hset-contains? h 'b))))))
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
    (test "hset-union!" env
        (let ((a (hset-empty))
              (b (hset-empty)))
            (hset-insert! a 'x)
            (hset-insert! b 'y)
            (hset-insert! b 'z)
            (hset-union! a b)
            (assert-lset-eq? (hset-list a) '(x y z))))
))

(suite "lookup.scm (bidirectional lookup table)"
    (tests
    (test "make-lookup-table" env
        (assert-true (not (null? (make-lookup-table)))))

    (test "lookup-record!" env
        (let ((t (make-lookup-table)))
            (lookup-record! t 'child 'parent)
            (assert-all
                (assert-lset-eq? (lookup-forward t 'child) '(parent))
                (assert-lset-eq? (lookup-inverse t 'child) '())
                (assert-lset-eq? (lookup-inverse t 'parent) '(child))
                (assert-lset-eq? (lookup-forward t 'parent) '()))))

    (test "lookup-clear!" env
        (let ((t (make-lookup-table)))
            (lookup-record! t 'child 'parent)
            (lookup-clear! t 'child)
            (assert-all
                (assert-lset-eq? (lookup-forward t 'child) '())
                (assert-lset-eq? (lookup-inverse t 'child) '())
                (assert-lset-eq? (lookup-upstream t 'child) '(child))
                (assert-lset-eq? (lookup-inverse t 'parent) '())
                (assert-lset-eq? (lookup-forward t 'parent) '()))))

    (test "lookup-upstream" env
        (let ((t (make-lookup-table)))
            (lookup-record! t 'parent 'grandparent)
            (lookup-record! t 'child 'parent)
            (assert-all
                (assert-lset-eq? '(child parent grandparent)
                                  (lookup-upstream t 'child))
                (assert-lset-eq? '(parent grandparent)
                                  (lookup-upstream t 'parent))
                (assert-lset-eq? '(grandparent)
                                 (lookup-upstream t 'grandparent)))))
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

(suite "graph.scm"
    (tests

    (test "graph-can-insert?" env
    (let ((g (make-graph)))
        (graph-insert! g '(x) "12")
        (graph-insert! g '(a x) "13")
        (assert-all
            (assert-true (graph-can-insert? g '(y)))
            (assert-false (graph-can-insert? g '(x)))
            (assert-true (graph-can-insert? g '(a y)))
            (assert-false (graph-can-insert? g '(a x))))))

    (test "graph-datum-ref" env
    (let ((g (make-graph)))
        (graph-insert! g '(x) "12")
        (graph-insert! g '(a x) "13")
        (assert-all
            (assert-true (graph-datum-ref g '(x)))
            (assert-false (graph-datum-ref g '(y)))
            (assert-true (graph-datum-ref g '(a x)))
            (assert-false (graph-datum-ref g '(a y))))))

    (test "graph-env" env
    (let ((g (make-graph)))
        (graph-insert! g '(x) "1")
        (let ((env (graph-env g '(dummy))))
            (assert-true (module-ref env 'x)))))

    (test "graph-eval-datum (independent)" env
    (let ((g (make-graph)))
        (graph-freeze! g)
        (graph-insert! g '(x) "12")
        (assert-all
            (assert-true (graph-eval-datum g '(x)))
            (assert-equal (graph-datum-value g '(x)) 12))))

    (test "graph-eval-datum (dependent)" env
    (let ((g (make-graph)))
        (graph-freeze! g)
        (graph-insert! g '(x) "1")
        (graph-insert! g '(y) "(+ 1 (x))")
        (graph-eval-datum g '(x))
        (assert-all
            (assert-true (graph-eval-datum g '(y)))
            (assert-equal (graph-datum-value g '(y)) 2))))
))

(suite "topolist.scm (topologically sorted list class)"
    (tests

    (test "topolist-insert!" env
    (let ((t (make-topolist <)))
        (topolist-insert! t 1)
        (topolist-insert! t 10)
        (topolist-insert! t 5)
        (assert-all
            (assert-equal 10 (topolist-pop! t))
            (assert-equal 5 (topolist-pop! t))
            (assert-equal 1 (topolist-pop! t))
            (assert-equal #nil (topolist-pop! t)))))
))
