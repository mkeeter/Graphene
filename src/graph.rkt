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
(provide ; Public API
         make-graph graph-insert-datum! graph-insert-subgraph!
         graph-has-datum? graph-has-subgraph?
         graph-result graph-expr graph-set-expr! graph-delete!
         graph-datums->list graph-datums->tree
         format-graph

         ; Other functions exposed for testing purposes
         graph-sub-ref graph-datum-ref
         graph-frozen? graph-freeze! graph-unfreeze!
         graph-lookup)

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
  (let-values ([(prefix name) (split-at-right id 1)])
    (values prefix (car name))))

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
    (unless (hash-has-key? sub name)
      (error "No such datum" id))
    (let ([ref (hash-ref sub name)])
      (unless (datum? ref)
        (error "Not a datum" id))
      ref)))

(define (graph-has-datum? g id)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (graph-datum-ref g id)
    #t))

(define (graph-has-subgraph? g id)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (graph-sub-ref g id)
    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-env g caller [input #f])
  ;; Returns a namespace in which the caller should be executed
  ;;
  ;; The namespace includes all datums in the same subgraph as caller
  ;; as no-argument thunks.  When called, each thunk records a lookup
  ;; by the caller
  ;;
  ;;   caller should be a full path to a datum in the graph
  ;;   input is true if the datum is an input
  ;;     If this is the case, it executes in the parent's environment

  (define (datum-lookup-thunk id datum)
    ;; Returns a thunk that records that the caller looked up id,
    ;; then attempts to return the value from datum.
    ;;   id should be a full path to the target datum
    (if (lookup-downstream? (graph-lookup g) id caller)
      (lambda ()
        (lookup-record! (graph-lookup g) caller id)
        (error "Circular reference" caller id))
      (lambda ()
        (lookup-record! (graph-lookup g) caller id)
        (if (not (datum-valid? datum))
            (error "Datum is invalid" datum)
            (datum-result datum)))))

  ;; Returns a thunk that looks up a variable in the given subgraph hash
  (define (subgraph-lookup-thunk id subgraph)
    (let ([children (make-hash)])

      ;; Add a datum-lookup thunk for every datum in the subgraph
      (hash-for-each subgraph
        (lambda (name child)
          (let ([id (append id (list name))])
            ;; Only expose datums that are tagged as outputs
            (when (and (datum? child) (datum-is-output? child))
              (hash-set! children name (datum-lookup-thunk id child))))))

      ;; The top-level thunk just calls the function in the hash
      (lambda (key)
        (if (hash-has-key? children key)
          ((hash-ref children key))
          (let ([path (append id (list key))])
            (lookup-record! (graph-lookup g) caller path))))))

  ;; Find the subgraph prefix of the caller variable
  (let-values ([(prefix _) (split-id caller)]
               [(env) (make-base-namespace)])

    ;; Execute input datums in parent graph environment
    (when input
      (set! prefix (drop-right prefix 1)))

    (hash-for-each
      (graph-sub-ref g prefix)
      (lambda (name child)
        (let ([id (append prefix (list name))])
        (namespace-set-variable-value! name
          (cond [(datum? child) (datum-lookup-thunk id child)]
                [(hash? child) (subgraph-lookup-thunk id child)]
                [else (error "Invalid item in graph" name child)])
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
          (graph-set-expr! g id expr)))))

(define (graph-insert-subgraph! g id)
  ;; Inserts a new subgraph into the graph
  ;; id must point into an existing subgraph
  (let*-values ([(prefix name) (split-id id)]
                [(ref) (graph-sub-ref g prefix)])
    (if (hash-has-key? ref name)
      (error "Id already exists" id)
      (hash-set! ref name (make-hash)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-delete-subgraph! g id)
  ;; Recursively deletes a subgraph
  (let-values ([(prefix name) (split-id id)]
               [(frozen) (graph-frozen? g)])
    (when (not frozen) (graph-freeze! g))
    (hash-map (graph-sub-ref g id)
      (lambda (k v)
        (let ([path (append id (list k))])
        (cond [(datum? v) (graph-delete-datum! g path)]
              [(hash? v)  (graph-delete-subgraph! g path)]))))
    (hash-remove! (graph-sub-ref g prefix) name)
    (when (not frozen) (graph-unfreeze! g))))

(define (graph-delete-datum! g id)
  ;; Deletes a datum from the graph
  (let-values ([(prefix name) (split-id id)]
               [(frozen) (graph-frozen? g)])
    (when (not frozen) (graph-freeze! g))
    (hash-remove! (graph-sub-ref g prefix) name)
    ;; Remove all record of lookups performed by this datum
    (lookup-clear! (graph-lookup g) id)
    ;; Then mark everything that this datum looked up as dirty
    (map (lambda (k) (graph-dirty! g k))
         (lookup-inverse->list (graph-lookup g) id))
    (when (not frozen) (graph-unfreeze! g))))

(define (graph-delete! g id)
  ;; Erases a datum or subgraph by absolute path
  (cond [(graph-has-datum? g id) (graph-delete-datum! g id)]
        [(graph-has-subgraph? g id) (graph-delete-subgraph! g id)]
        [else (error "~a is neither a subgraph nor a datum" id)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph-datums->list g)
  (let recurse ([prefix '()])
  ;; Returns a flat structure of absolute datum ids
    (apply append (hash-map (graph-sub-ref g prefix)
      (lambda (k v)
        (let ([path (append prefix (list k))])
          (cond [(datum? v) (list path)]
                [(hash? v) (recurse path)])))))))

(define (graph-datums->tree g)
  ;; Returns a recursive structure with relative datum ids
  (let recurse ([target (graph-sub-ref g '())])
    (hash-map target
      (lambda (k v)
          (cond [(datum? v) k]
                [(hash? v) (cons k (recurse v))])))))

(define (format-graph g)
  ;; Formats a graph as a tree structure
  (let recurse ([target (graph-datums->tree g)]
                [indent 0]
                [prefix '()])
    (let ([t (make-string indent #\ )])
    (string-append t
    (string-replace (string-join (map (lambda (k)
      (cond [(list? k) (format "- ~a:\n~a" (car k)
                               (recurse (cdr k) (+ 2 indent)
                               (append prefix (list (car k)))))]
            [else
            (let* ([path (append prefix (list k))]
                   [_result (graph-result g path)]
                   [result (if (exn:fail? _result)
                           (exn-message _result) _result)]
                   [expr (graph-expr g path)])
                  (string-replace (format "- ~a\n~a\n= ~a" k expr result)
                    "\n" "\n| "))]))
      target)
    "\n") "\n" (string-append "\n" t))))))

(define (print-graph g)
  ;; Prints the given graph to stdout
  (display (format-graph g)))

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

(define (graph-set-expr! g id expr)
  ;; Sets a datum's expression and triggers evaluation
  ;; (unless the graph is frozen)
  (set-datum-expr! (graph-datum-ref g id) expr)
  (graph-dirty! g id)
  (unless (graph-frozen? g)
    (graph-sync! g)))

(define (graph-eval-datum! g id)
  ;; Evaluates the target datum
  ;; Returns #t if the value changed, #f otherwise
  (lookup-clear! (graph-lookup g) id)
  (let* ([d (graph-datum-ref g id)]
         [env (graph-env g id (datum-is-input? d))]
         [changed (datum-eval! d env)]
         [res (graph-result g id)])

      ;; If evaluation failed when looking up a (missing) value, record the
      ;; attempted lookup so that if the value is created, this datum can be
      ;; re-evaluated.
      (when (exn:fail:contract:variable? res)
        (let*-values ([(prefix _) (split-id id)]
                      [(var) (exn:fail:contract:variable-id res)]
                      [(path) (append (if (datum-is-input? d)
                                          (drop-right prefix 1) prefix)
                                      (list var))])
        (lookup-record! (graph-lookup g) id path)))
      changed))

(define (graph-result g id)
  ;; Returns the value for the given datum
  (datum-result (graph-datum-ref g id)))

(define (graph-expr g id)
  ;; Returns the expression for the given datum
  (datum-expr (graph-datum-ref g id)))
