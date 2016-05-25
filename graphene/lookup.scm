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
    #:export (make-lookup-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (graphene hset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-lookup-table)
    (let ((forward (make-hash-table))
          (inverse (make-hash-table)))

    (define (get h k)
        (if (not (hash-ref h k))
            (hash-set! h k (hset-empty)))
        (hash-ref h k))

    (define (flatten h)
        (hash-map->list (lambda (k v) (cons k (hset-list v))) h))

    (define (record-lookup a b)
        ;; Record that a looked up b
        (hset-insert! (get forward a) b)
        (hset-insert! (get inverse b) a))

    (define (clear-lookups a)
        ;; Clear all lookups performed by a
        (map (lambda (b) (hset-remove! (get inverse b) a))
             (hset-list (get forward a)))
        (hash-remove! forward a))

    (define (dispatch-lookup key . args)
        (cond ((eq? key 'record) (record-lookup (car args) (cadr args)))
              ((eq? key 'clear)  (clear-lookups (car args)))
              ((eq? key 'forward)
                    (if (null? args) (flatten forward)
                                     (hset-list (get forward (car args)))))
              ((eq? key 'inverse)
                    (if (null? args) (flatten inverse)
                                     (hset-list (get inverse (car args)))))
              (else (error "Invalid key" key))))

    dispatch-lookup))
