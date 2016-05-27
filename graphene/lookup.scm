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
              lookup-record! lookup-clear! is-downstream?
              lookup-forward lookup-upstream lookup-inverse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (oop goops)
             (graphene hset))

;;  <lookup-table> is a bidirectional table of lookups
;;      forward records things that a particular name looked up
;;      upstream records things that a particular name looked up, and things
;;          that those things looked up, etc (so it's the union of all the
;;          upstream values)
;;      inverse records the names that looked up a particular thing"
(define-class <lookup-table> ()
    (forward  #:init-thunk make-hash-table)
    (upstream #:init-thunk make-hash-table)
    (inverse  #:init-thunk make-hash-table))

(define (make-lookup-table) (make <lookup-table>))

(define-method (hset-ref (table <lookup-table>) (dir <symbol>) key)
    "hset-ref table dir key
    Returns the hset for the given direction and key
    dir is 'forward, 'inverse, or 'upstream
    key is the hash-set key"
    (let ((ref (hash-ref (slot-ref table dir) key)))
    (if (not ref)
        (begin
            (set! ref (hset-empty))
            (hash-set! (slot-ref table dir) key ref)
            (if (eq? dir 'upstream)
                (hset-insert! ref key))))
    ref))

(define-method (lookup-record! (table <lookup-table>) a b)
    "lookup-record table a b
    Records that a looked up b"
    (hset-insert! (hset-ref table 'forward a) b)
    (hset-union!  (hset-ref table 'upstream a)
                  (hset-ref table 'upstream b))
    (hset-insert! (hset-ref table 'inverse b) a))

(define-method (lookup-clear! (table <lookup-table>) a)
    "lookup-clear table a
    Clears all lookups performed by a"
    (map (lambda (b) (hset-remove! (hset-ref table 'inverse b) a))
         (hset-list (hset-ref table 'forward a)))
    (hash-remove! (slot-ref table 'forward) a)
    (hash-remove! (slot-ref table 'upstream) a))

(define-method (lookup-forward (table <lookup-table>) a)
    "lookup-forward table a
    Lists all of the things that a has looked up"
    (hset-list (hset-ref table 'forward a)))

(define-method (lookup-upstream (table <lookup-table>) a)
    "lookup-upstream table a
    Lists all of the things upstream of a"
    (hset-list (hset-ref table 'upstream a)))

(define-method (is-downstream? (table <lookup-table>) a b)
    "is-downstream? table a b
    Returns true if a is downstream of b
    (i.e. if b is in a's upstream set)"
    (hset-contains? (hset-ref table 'upstream a) b))

(define-method (lookup-inverse (table <lookup-table>) b)
    "lookup-inverse table a
    Lists all of the things that looked up b"
    (hset-list (hset-ref table 'inverse b)))
