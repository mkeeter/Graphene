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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (graphene datum)
    #:export (datum? make-datum datum-set-expr! datum-eval! datum-value datum-error))

(use-modules (oop goops))

(define-class <datum> ()
    (expr #:init-form "")
    (value #:init-form #nil)
    (error #:init-form #nil))

(define (make-datum) (make <datum>))
(define (datum? d) (eq? (class-of d) <datum>))

(define-method (datum-set-expr! (d <datum>) (e <string>))
    "datum-set-expr! d s
    Sets the datum's expr to the given value
    Returns true if the expr changed, false otherwise"
    (if (equal? (slot-ref d 'expr) e) #f
        (begin (slot-set! d 'expr e) #t)))

(define-method (datum-value (d <datum>))
    (slot-ref d 'value))

(define-method (datum-error (d <datum>))
    (slot-ref d error))

(define-method (datum-eval! (d <datum>) (env <module>))
    "datum-eval! d env
    Evaluates a datum in the given environment
    If successful, sets value slot to result and error to nil
    Otherwise, sets values slot to nil and error to the error
    Returns true if value has changed, otherwise false"
    (let ((prev (slot-ref d 'value)))
    (catch #t
    ;; Evaluate function in the provided environment
    (lambda ()
        (let ((out (eval (read (open-input-string (slot-ref d 'expr))) env)))
            (slot-set! d 'error #nil)
            (slot-set! d 'value out)
            (not (equal? out prev))))
    ;; On error, record the error message
    (lambda (key . args)
        (slot-set! d 'error (cons key args))
        (slot-set! d 'value #nil)))
    (not (equal? prev (slot-ref d 'value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
