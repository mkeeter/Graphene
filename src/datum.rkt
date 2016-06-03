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

(provide make-datum datum? datum-expr datum-value datum-error
         datum-eval! set-datum-expr!)

(struct datum (expr value error) #:mutable)
(define (make-datum) (datum "" #f #f))

(define (datum-eval! d [env #f])
  ;; Evaluates a datum in the given environment
  ;; If successful, sets value to result and error to #nil
  ;; Otherwise, sets value to #nil and error to the error
  ;; Returns true if value has changed, false otherwise
  (let ([prev (datum-value d)]
        [env (or env (make-base-namespace))])
  (with-handlers ([exn:fail?
    ;; Error handler for all normal errors
    (lambda (v) (set-datum-error! d v)
                (set-datum-value! d #f))])
    ;; Try to evaluate the datum's expression
    (let ([out (eval (read (open-input-string (datum-expr d))) env)])
      (set-datum-value! d out)
      (set-datum-error! d #f)))
  ;; Return a boolean indicating whether the value has changed
  (not (equal? prev (datum-value d)))))

