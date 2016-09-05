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
(require xrepl)

(require "graph.rkt")

(define __g (make-graph))
(define (printer p)
  (if (void? p) (print-graph __g) (display p)))

;; Create a set of local bindings that use the graph
(define (insert-datum! id expr) (graph-insert-datum! __g id expr))
(define (insert-subgraph! id) (graph-insert-subgraph! __g id))
(define (has-datum? id) (graph-has-datum? __g id))
(define (has-subgraph? id) (graph-has-subgraph? __g id))
(define (result id) (result __g id))
(define (expr id) (expr __g id))
(define (set-expr! id expr) (graph-set-expr! __g id expr))
(define (delete! id) (graph-delete! __g id))
(define (datums->list id) (graph-datums->list __g id))
(define (datums->tree id) (graph-datums->tree __g id))

(current-print printer)
(printf "Entered Graphene environment\n")
