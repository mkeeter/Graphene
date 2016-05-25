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

(define-module (graphene graph)
    #:export (make-graph graph-can-insert? graph-insert! graph-datum-ref))

(use-modules (ice-9 r5rs))
(use-modules (oop goops))
(use-modules (graphene lookup) (graphene datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <graph> ()
    (children #:init-thunk make-hash-table)
    (lookups #:init-thunk make-lookup-table))

(define (make-graph) (make <graph>))

(define-method (graph-env (g <graph>) (a <pair>))
    "graph-env graph a
    Returns a module in which all children g are no-argument lambda functions
    When called, they record a lookup by 'a"
    (let ((env (scheme-report-environment 5))
          (prefix (list-head a (1- (length a)))))

    ;; Local lookup function records that 'a' did the looking up
    (define (perform-lookup-of name)
        (lookup-record! (slot-ref g 'lookups) a name)
        (let ((d (hash-ref (slot-ref g 'children) (car a))))
            (if (datum-error d)
                (error "Datum is invalid" d)
                (datum-value d))))

    (hash-for-each (lambda (name child)
        (module-define! env name (lambda () (perform-lookup-of name))))
        (slot-ref g 'children))
    env))

(define-method (graph-can-insert? (g <graph>) (name <pair>))
    "graph-can-insert graph name
    Checks to see whether the given name can be inserted into the graph"
    (define (recurse hash name)
        (let* ((head (car name))
               (tail (cdr name))
               (ref (hash-ref hash head)))
        (cond ((null? tail) (not ref))
              ((datum? ref ) #f)
              (ref (recurse ref tail))
              (else #t))))
    (recurse (slot-ref g 'children) name))

(define-method (graph-insert! (g <graph>) (name <pair>) (expr <string>))
    "graph-insert-datum graph name expr
    Inserts a datum into the graph
    Recursively inserts subgraph tables if name has length > 1"
    (define (recurse hash name expr)
        (let* ((head (car name))
               (tail (cdr name))
               (ref (hash-ref hash head)))

        (if (null? tail)
            ;; If we've reached the bottom of our recursion, make a new datum
            ;; and assign it (as graph-can-insert? already checked that the
            ;; name isn't already in use)
             (let ((d (make-datum)))
                 (hash-set! hash head d)
                 (datum-set-expr! d expr))

            ;; Otherwise, construct a subgraph if necessary and then recurse
            ;; into it with the remaining name symbols
            (begin
            (if (not ref)
                (begin (set! ref (make-hash-table))
                       (hash-set! hash head ref)))
            (recurse ref tail expr)))))

    (if (not (graph-can-insert? g name))
        (error "Invalid or duplicate name" name)
        (recurse (slot-ref g 'children) name expr)))

(define-method (graph-datum-ref (g <graph>) (name <pair>))
    "graph-datum-ref graph name
    Looks up a datum by name, returning it"
    (define (recurse hash name)
        (let* ((head (car name))
               (tail (cdr name))
               (ref (hash-ref hash head)))
        (cond ((datum? ref) ref)
              ((hash-table? ref) (recurse ref tail))
              (else #f))))
    (recurse (slot-ref g 'children) name))

;(define-method (graph-eval-datum (g <graph>) (name <pair>))
