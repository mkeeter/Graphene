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

(provide make-datum datum? datum-expr datum-result
         datum-eval! set-datum-expr! datum-valid?)

(struct datum (expr result) #:mutable)
(define (make-datum) (datum "" eof))

(define (datum-eval! d [env #f])
  ;; Evaluates a datum in the given environment
  ;; If successful, sets value to result and error to #nil
  ;; Otherwise, sets value to #nil and error to the error
  ;; Returns true if value has changed, false otherwise
  (let ([prev (datum-result d)]
        [env (or env (make-base-namespace))])
  (with-handlers ([exn:fail?
    ;; Error handler for all normal errors
    (lambda (v) (set-datum-result! d v))])
    ;; Try to evaluate the datum's expression
    (let ([out (eval (read (open-input-string (datum-expr d))) env)])
      (set-datum-result! d out)))
  ;; Return a boolean indicating whether the value has changed
  (not (equal? prev (datum-result d)))))

(define (datum-valid? d)
  ;; Checks to see if the given datum is valid
  (not (exn:fail? (datum-result d))))
