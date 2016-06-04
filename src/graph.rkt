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

(require "topolist.rkt" "datum.rkt" "lookup.rkt")
(provide make-graph graph-env graph-insert-datum! graph-insert-subgraph!
         graph-sub-ref graph-datum-ref graph-eval-datum! graph-frozen?
         graph-freeze! graph-unfreeze! graph-datum-value graph-datum-error
         graph-set-datum-expr!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct graph (children lookup dirty frozen) #:mutable)

(define (make-graph)
  (let* ([lookup (make-lookup)]
         [comp (lambda (a b) (lookup-downstream? lookup a b))]
         [dirty (make-topolist comp)])
    (graph (make-hash) lookup dirty #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-frozen? g)
  ;; Predicate ending in ?, looks up frozen value
  (graph-frozen g))

(define (graph-freeze! g)
  ;; Marks graph as frozen
  (set-graph-frozen! g #t))

(define (graph-unfreeze! g)
  ;; Unfreezes graph and flushes dirty queue
  (set-graph-frozen! g #f)
  (graph-sync! g))

(define (graph-dirty! g id)
  ;; Marks the given id as dirty
  (topolist-insert! (graph-dirty g) id))

(define (split-id id)
  ;; Splits an identifier into a prefix and name
  (values (take id (- (length id) 1))
          (last id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-sub-ref g path)
  ;; Looks up a subgraph by name, returning its hash table
  ;; Raises an error if no such name is found
  (let recurse ([name path] [hash (graph-children g)])
    (cond [(not (hash? hash)) (error "Not a subgraph" path)]
          [(null? name) hash]
          [(hash-has-key? hash (car name))
            (recurse (cdr name) (hash-ref hash (car name)))]
          [else (error "No such subgraph" name)])))

(define (graph-datum-ref g id)
  ;; Looks up a datum by name, returning it
  (let*-values ([(prefix name) (split-id id)]
                [(sub) (graph-sub-ref g prefix)])
  (if (hash-has-key? sub name)
    (let ([ref (hash-ref sub name)])
      (if (datum? ref)
        ref
        (error "Not a datum" id)))
    (error "No such datum" id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-env g caller)
  ;; Returns a namespace in which the caller should be executed
  ;;
  ;; The namespace includes all datums in the same subgraph as caller
  ;; as no-argument thunks.  When called, each thunk records a lookup
  ;; by the caller
  ;;
  ;;   caller should be a full path to a datum in the graph

  (define (local-lookup id datum)
    ;; Returns a thunk that records that the caller looked up id,
    ;; then attempts to return the value from datum.
    ;;   id should be a full path to the target datum
    (lambda ()
      (lookup-record! (graph-lookup g) caller id)
      (if (datum-error datum)
          (error "Datum is invalid" datum)
          (datum-value datum))))

  (let-values ([(prefix _) (split-id caller)]
               [(env) (make-base-namespace)])

    (hash-for-each
      (graph-sub-ref g prefix)
      (lambda (name child)
        (when (datum? child)
          (namespace-set-variable-value!
            name (local-lookup (append prefix (list name)) child)
            #f env))))
    env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-insert-datum! g id expr)
  ;; Inserts a new datum into the graph
  ;; id must point into an existing subgraph
  (let*-values ([(prefix name) (split-id id)]
                [(ref) (graph-sub-ref g prefix)])
    (if (hash-has-key? ref name)
        (error "Id already exists" id)
        (begin
          (hash-set! ref name (make-datum))
          (graph-set-datum-expr! g id expr)))))

(define (graph-insert-subgraph! g id)
  ;; Inserts a new subgraph into the graph
  ;; id must point into an existing subgraph
  (let*-values ([(prefix name) (split-id id)]
                [(ref) (graph-sub-ref g prefix)])
    (if (hash-has-key? ref name)
      (error "Id already exists" id)
      (hash-set! ref name (make-hash)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-sync! g)
  (unless (topolist-empty? (graph-dirty g))
    ;; Get the first item from the list and evaluate it
    (let* ([head (topolist-pop! (graph-dirty g))]
           [changed (graph-eval-datum! g head)])
      ;; If it changed, add all of its children to the dirty list
      (when changed
        (map (lambda (k) (graph-dirty! g k))
             (lookup-inverse->list (graph-lookup g) head)))
      ;; Recurse until the dirty list is empty
      (graph-sync! g))))

(define (graph-set-datum-expr! g id expr)
  ;; Sets a datum's expression and triggers evaluation
  ;; (unless the graph is frozen)
  (set-datum-expr! (graph-datum-ref g id) expr)
  (graph-dirty! g id)
  (unless (graph-frozen? g)
    (graph-sync! g)))

(define (graph-eval-datum! g id)
  ;; Evaluates the target datum
  (lookup-clear! (graph-lookup g) id)
  (datum-eval! (graph-datum-ref g id) (graph-env g id)))

(define (graph-datum-value g id)
  ;; Returns the value for the given datum
  (datum-value (graph-datum-ref g id)))

(define (graph-datum-error g id)
  ;; Returns the value for the given datum
  (datum-error (graph-datum-ref g id)))
