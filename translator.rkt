#lang racket

(define (translate prog)
  (cond
    [(number? prog)
     (number->string prog)]
    [(symbol? prog)
     (symbol->string prog)]
    [(eq? (first prog) 'println)
     (string-append "print(" (translate (second prog)) ")")]
    [(eq? (first prog) 'λ)
     (string-append "lambda " (symbol->string (first (second prog))) ": " (translate (third prog)))]
    [(or (eq? (first prog) '+) (eq? (first prog) '*))
     (string-append (translate (second prog)) (symbol->string (first prog)) (translate (third prog)))]
    [(eq? (first prog) 'ifleq0)
     (string-append "("(translate (third prog))
                    ") if (" (translate (second prog)) " <= 0) else ("
                    (translate (fourth prog)) ")")]
    [else
     (string-append "(" (translate (first prog)) ")(" (translate (second prog)) ")")]))

(define (translate-file name)
  (translate (read (open-input-file name))))
