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

(require scheme/mpair)

(provide make-topolist topolist-empty? topolist-insert! topolist-pop!)

(define (make-topolist comp)
  (mcons comp '()))

(define (topolist-comp t) (mcar t))
(define (topolist-list t) (mcdr t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (topolist-empty? t)
  ;; Checks if the given list is empty
  (null? (topolist-list t)))

(define (topolist-pop! t)
  ;; Pops an item from the fromt of the list, removing and returning it
  (let ([out (mcar (mcdr t))])
    (set-mcdr! t (mcdr (topolist-list t)))
    out))

(define (topolist-insert! t key)
  ;; Inserts the given key into the list

  (define (splice! head key)
    ;; Inserts the given key between the head and tail of the list
    (set-mcdr! head (mcons key (mcdr head))))

  (define (recurse head key)
    ;; Helper function to walk the list, inserting the key after
    ;; the last element for which (func key (car head)) is true
    (cond
      ;; If the list is empty, return immediately with the fact
      ;; that we didn't insert the pair anywhere
      [(null? head) #f]

      ;; Check to see if the key got added later in the list
      [(recurse (mcdr head) key) #t]

      ;; Otherwise, check to see if the key should be added
      ;; right after the head node, returning true if so
      [((topolist-comp t) key (mcar head))
          (splice! head key)
          #t]
      [else #f]))

  ;; If we didn't insert the pair later in the list, it will have
  ;; to be inserted at the head of the list
  (unless (recurse (topolist-list t) key)
    (splice! t key)))
