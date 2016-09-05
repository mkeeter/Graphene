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

(require racket/set racket/hash)

(provide make-lookup lookup-record! lookup-clear! lookup-downstream?
         lookup-inverse->list lookup-forward)

;; Makes an empty lookup table
(struct lookup (forward inverse upstream) #:mutable)
(define (make-lookup) (lookup (make-hash) (make-hash) (make-hash)))

(define (lookup-ref table dir key)
  ;;  Returns the hash for the given direction and key
  ;;    dir is 'forward, 'inverse, or 'upstream
  ;;    key is the hash-set key
  (let* ([hash (cond [(eq? dir 'forward)  (lookup-forward table)]
                     [(eq? dir 'inverse)  (lookup-inverse table)]
                     [(eq? dir 'upstream) (lookup-upstream table)]
                     [else (error "Invalid key" dir)])]
         [out (hash-ref hash key #f)])
    (unless out
      (set! out (mutable-set))
      (hash-set! hash key out)
      ;; Special-case for the 'upstream hash, which contains itself
      (when (eq? dir 'upstream)
        (set-add! out key)))
    out))

(define (lookup-record! table a b)
  ;; Records that a looked up b
  (set-add! (lookup-ref table 'forward a) b)
  (set-add! (lookup-ref table 'inverse b) a)
  (set-union! (lookup-ref table 'upstream a)
              (lookup-ref table 'upstream b)))

(define (lookup-clear! table a)
  ;; Clears all lookups performed by a
  (set-for-each (lookup-ref table 'forward a)
                (lambda (b) (set-remove! (lookup-ref table 'inverse b) a)))
  (hash-remove! (lookup-forward table) a)
  (hash-remove! (lookup-upstream table) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup-downstream? table a b)
  ;; Checks if a is downstream of b
  ;; (i.e. checking if b is in a's upstream set)
  (set-member? (lookup-ref table 'upstream a) b))

(define (lookup-inverse->list table a)
  ;; Returns a list of a's inverse lookups
  (set->list (lookup-ref table 'inverse a)))
