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

(define-module (graphene topolist)
    #:export (make-topolist topolist-insert! topolist-pop!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (oop goops))

;; This would be more efficient as a doubly linked list
(define-class <topolist> ()
    (head #:init-form '())
    (func #:init-form (lambda (a b) #t )))

(define (make-topolist func)
    "make-topolist func
    A topolist stores items such that for every pairwise set of items (a, b),
    a is after b in the list if (func a b) is true"
    (let ((t (make <topolist>)))
    (slot-set! t 'func func)
    t))

(define-method (topolist-insert! (t <topolist>) key)
    "topolist-insert! t key
    Inserts the given key into the list"
    (let ((p (cons key '()))) ;; This is the pair to insert

    ;; Helper function to walk the list, inserting the key after
    ;; the last element for which (func key (car head)) is true
    (define (recurse head key)
        ;; If the list is empty, return immediately with the fact
        ;; that we didn't insert the pair anywhere
        (if (null? head) #f

        ;; Check to see if the key got added later in the list
        (let ((r (recurse (cdr head) key)))
            (cond
                ;; If the key was added later in the list, then there's
                ;; nothing to do here; return #t
                (r #t)

                ;; Otherwise, check to see if the key should be added
                ;; right after the head node, returning true if so
                (((slot-ref t 'func) key (car head))
                    (set-cdr! p (cdr head))
                    (set-cdr! head p)
                    #t)
                (else #f)))))

    ;; If we didn't insert the pair later in the list, it will have
    ;; to be inserted at the head of the list
    (if (not (recurse (slot-ref t 'head) key))
        (begin
        (set-cdr! p (slot-ref t 'head))
        (slot-set! t 'head p)))))

(define-method (topolist-pop! (t <topolist>))
    "topolist-pop! t
    Returns the next available item in the list
    This is the item a for which (func a b) is false for
    every other item b in the list."
    (let ((out (slot-ref t 'head)))
        (slot-set! t 'head (car out))
        out))
