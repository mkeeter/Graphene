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

(define-module (graphene lookup)
    #:export (make-lookup-table
              lookup-record!
              lookup-clear!
              lookup-forward
              lookup-inverse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (oop goops)
             (graphene hset))

(define-class <lookup-table> ()
    (forward #:init-thunk make-hash-table)
    (inverse #:init-thunk make-hash-table))

(define (make-lookup-table) (make <lookup-table>))

(define-method (hset-ref (table <lookup-table>) (dir <symbol>) key)
    "hset-ref table dir key
    Returns the hset for the given direction and key
    dir is 'forward or 'inverse
    key is the hash-set key"
    (if (not (hash-ref (slot-ref table dir) key))
        (hash-set! (slot-ref table dir) key (hset-empty)))
    (hash-ref (slot-ref table dir) key))

(define-method (lookup-record! (table <lookup-table>) a b)
    "lookup-record table a b
    Records that a looked up b"
    (hset-insert! (hset-ref table 'forward a) b)
    (hset-insert! (hset-ref table 'inverse b) a))

(define-method (lookup-clear! (table <lookup-table>) a)
    "lookup-clear table a
    Clears all lookups performed by a"
    (map (lambda (b) (hset-remove! (hset-ref table 'inverse b) a))
         (hset-list (hset-ref table 'forward a)))
    (hash-remove! (slot-ref table 'forward) a))

(define-method (lookup-forward (table <lookup-table>) a)
    "lookup-forward table a
    Records all of the things that a has looked up"
    (hset-list (hset-ref table 'forward a)))

(define-method (lookup-inverse (table <lookup-table>) b)
    "lookup-inverse table a
    Records all of the things that looked up b"
    (hset-list (hset-ref table 'inverse b)))
