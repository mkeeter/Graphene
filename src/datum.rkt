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
         datum-eval! set-datum-expr! datum-valid?
         datum-is-output? datum-is-input?)

(struct datum (expr result) #:mutable)
(define (make-datum) (datum "" eof))

(define (datum-eval! d [env #f])
  ;; Evaluates a datum in the given environment
  ;; If successful, sets value to result and error to #nil
  ;; Otherwise, sets value to #nil and error to the error
  ;; Returns true if value has changed, false otherwise
  (let ([prev (datum-result d)]
        [env (or env (make-base-namespace))]
        [input (open-input-string (datum-expr d))]
        [id (lambda (i) i)])

  ;; Insert input and output as dummy functions
  ;; (since they are used as tags in datum expressions)
  (namespace-set-variable-value! 'input  id #f env)
  (namespace-set-variable-value! 'output id #f env)

  (with-handlers ([exn:fail?
    ;; Error handler for all normal errors
    (lambda (v) (set-datum-result! d v))])
    ;; Try to evaluate the datum's expression
    (let ([expr (read input)])
      ;; If there are multiple forms in the expression, raise an error
      (when (not (equal? (read input) eof))
            (error "Too many forms" (datum-expr d)))
      ;; Otherwise attempt to eval and store the result
      (set-datum-result! d (eval expr env))))

  ;; Check for changes, either of the form
  ;;    value -> error
  ;;    value -> different value
  (let ([res (datum-result d)])
    (not (if (exn:fail? res)
      (exn:fail? prev)
      (eq? prev res))))))

(define (datum-valid? d)
  ;; Checks to see if the given datum is valid
  (not (exn:fail? (datum-result d))))

(define (datum-starts-with? d sym)
  ;; Checks if a datum starts with a particular symbol
  ;; If the datum's expression is incomplete, returns false
  (let ([input (open-input-string (datum-expr d))])
    (with-handlers ([exn:fail? (lambda (v) #f)])
      (equal? (car (read input)) sym))))

(define (datum-is-output? d)
  ;; Checks to see if the datum is an output form
  (datum-starts-with? d 'output))

(define (datum-is-input? d)
  ;; Checks to see if the datum is an output form
  (datum-starts-with? d 'input))
