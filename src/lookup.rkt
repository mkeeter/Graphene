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

(provide make-lookup lookup-record! lookup-clear! is-downstream?)

;; Makes an empty lookup table
(define (make-lookup) (list (make-hash) (make-hash) (make-hash)))

;; Access individual elements in the table
(define (lookup-forward-ref table) (car table))
(define (lookup-inverse-ref table) (cadr table))
(define (lookup-upstream-ref table) (caddr table))

(define (lookup-ref table dir key)
  ;;  Returns the hash for the given direction and key
  ;;    dir is 'forward, 'inverse, or 'upstream
  ;;    key is the hash-set key
  (let* ([hash (cond [(eq? dir 'forward)  (lookup-forward-ref  table)]
                     [(eq? dir 'inverse)  (lookup-inverse-ref  table)]
                     [(eq? dir 'upstream) (lookup-upstream-ref table)]
                     [else (error "Invalid key" dir)])]
         [out (hash-ref hash key #f)])
    (unless out
      (set! out (make-hash))
      (hash-set! hash key out)
      ;; Special-case for the 'upstream hash, which contains itself
      (when (eq? dir 'upstream)
        (hash-set! out key #t)))
    out))

(define (lookup-record! table a b)
  ;; Records that a looked up b
  (hash-set! (lookup-ref table 'forward a) b #t)
  (hash-set! (lookup-ref table 'inverse b) a #t)
  (hash-union! (lookup-ref table 'upstream a)
               (lookup-ref table 'upstream b)))

(define (lookup-clear! table a)
  ;; Clears all lookups performed by a
  (hash-for-each (lambda (b) (hash-remove! (lookup-ref table 'inverse b) a))
                 (lookup-ref table 'forward a))
  (hash-remove! (lookup-ref table 'forward) a)
  (hash-remove! (lookup-ref table 'upstream) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-downstream? table a b)
  ;; Checks if a is downstream of b
  ;; (i.e. checking if b is in a's upstream set)
  (hash-has-key? (lookup-ref table 'upstream a) b))
