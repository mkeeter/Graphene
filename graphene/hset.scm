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

(define-module (graphene hset)
    #:export (hset-empty hset-list hset-insert! hset-remove! hset-union!))

(use-modules (oop goops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <hset> ()
    (table #:init-thunk make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hset-empty) (make-instance <hset>))

(define-method (hset-list (h <hset>))
    (hash-map->list (lambda (k v) k) (slot-ref h 'table)))

(define-method (hset-insert! (h <hset>) value)
    (hash-set! (slot-ref h 'table) value #t))

(define-method (hset-union! (self <hset>) (other <hset>))
    "hset-union! self other
    Inserts every element from other into self"
    (hash-for-each (lambda (k v) (hset-insert! self k))
                   (slot-ref other 'table)))

(define-method (hset-remove! (h <hset>) value)
    (hash-remove! (slot-ref h 'table) value))
