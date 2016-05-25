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
    #:export (make-datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-datum)
    (let ((expr #nil)
          (value #nil)
          (error #nil))
        (define (eval-expr env)
            (set! value
                (catch #t
                (lambda ()
                    (let ((out (eval expr env)))
                    (set! error #nil) out))
                (lambda (key . args)
                    (set! error (cons key args))
                    #nil))))

        (define (dispatch-datum key . args)
            (cond ((eq? key 'value) value)
                  ((eq? key 'error) error)
                  ((eq? key 'expr) expr)
                  ((eq? key 'type) 'datum)
                  ((eq? key 'set-expr!) (set! expr (car args)))
                  ((eq? key 'eval-expr) (eval-expr (car args)))
                  (else (error "Invalid key" key))))
        dispatch-datum))
