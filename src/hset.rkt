#lang racket
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

(require racket/set)

(provide make-hset hset-has-key? hset-insert! hset-remove! hset-union!)

(struct hset (table))

(define (make-hset)
    (hset (make-hash)))
(define (hset-has-key? h key)
    (hash-has-key? (hset-table h) key))
(define (hset-insert! h key)
    (hash-set! (hset-table h) key #t))
(define (hset-remove! h key)
    (hash-remove! (hset-table h) key))
(define (hset-union! self other)
    (hash-for-each (hset-table other)
        (lambda (k v) (hset-insert! self k))))
