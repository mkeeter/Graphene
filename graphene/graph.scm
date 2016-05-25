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

(define-module (graphene graph)
    #:export (make-graph))

(use-modules (ice-9 r5rs))
(use-modules (graphene lookup) (graphene datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-graph)
    (let ((children (make-hash-table))
          (lookups (make-lookup-table)))

    (define (make-env a)
        (define (perform-lookup name)
            (lookups 'record a name)
            (let ((c (hash-ref children name)))
            (cond ((eq? (c 'type) 'datum)
                    (if (c 'error) (error "Datum is invalid" child)
                                   (c 'value)))
                  (else (error "Invalid child" name c)))))

        (let ((env (scheme-report-environment 5)))

        (hash-for-each (lambda (name child)
            (module-define! env name (lambda () (perform-lookup name))))
            children)
        env))

    (define (add-datum name expr)
        (if (hash-ref children name)
            (error "Duplicate names are not allowed"))
        (hash-set! children name (make-datum))
        (set-datum-expr name expr)

    (define (set-datum-expr name expr)
        (if (not (hash-ref children name))
            (error "Could not find datum" name))
        (if (not (string? expr))
            (error "expr must be a string" expr))
        (datum 'set-expr! expr))

    (define (dispatch-graph key . args)
        (cond ((eq? key 'env) (make-env (car args)))
              ((eq? key 'add-datum) (add-datum (car args) (cadr args)))
              (else (error "Invalid key" key))))

    dispatch-graph))
