(define indent 0)
(define col 0)

(define (print-indent n)
  (if (> n 0)
      (begin (my-print " ") (print-indent (- n 1)))
      '()))

(define (print-code code)
  (if (pair? code)
      (print-list code #t)
      (print-atom code)))

(define (print-list code head)
  (case (car code)
    ((let) (print-let code))
    ((cond) (print-cond code))
    ((case) (print-case code))
    ((begin) (print-begin-like code))
    (else (my-print code))))
  
(define (print-atom code)
  (my-print code))

(define (print-let code)
  (let ((old-indent indent)
        (bindings (cadr code))
        (body (cddr code)))
    (my-print "(")
    (my-print (car code))
    (my-print " ")
    (my-print "(")
    (set! indent col)
    (if (null? bindings)
        (my-print ")")
        (begin
          (my-print "(")
          (my-print (caar bindings))
          (my-print " ")
          (print-code (cadar bindings))
          (my-print ")")
          (let loop ((bs (cdr bindings)))
            (if (null? bs)
                (my-print ")")
                (begin
                  (my-newline)
                  (print-indent indent)
                  (my-print "(")
                  (my-print (car (car bs)))
                  (my-print " ")
                  (print-code (cadr (car bs)))
                  (my-print ")"))))))
    (my-print ")")
    (set! indent (+ 2 old-indent))
    (print-body body)
    (my-print ")")
    (set! indent old-indent)))


(define (print-begin-like c)
  (let ((old-indent indent))
    (my-print "(")
    (print-code (car c))
    (my-print " ")
    (set! indent col)
    (print-code (cadr c))
    (for-each
     (lambda (c)
       (my-newline)
       (print-indent indent)
       (print-code c))
     (cddr c))
    (my-print ")")
    (set! indent old-indent)))

(define (print-cases code)
  (let ((clause-indent col))
    (set! indent clause-indent)
    (print-begin-like (car code))
    (for-each
     (lambda (c)
       (my-newline)
       (print-indent indent)
       (print-begin-like c))
     (cdr code))))

(define (print-case code)
  (let ((old-indent indent)
        (clauses (cddr code)))
    (my-print "(case ")
    (print-code (cadr code))
    (set! indent (+ indent 2))
    (my-newline)
    (print-indent indent)
    (print-cases clauses)
    (my-print ")")
    (set! indent old-indent)))


(define (print-cond code)
  (let ((old-indent indent)
        (clauses (cdr code)))
    (my-print "(cond ")
    (print-cases clauses)
    (my-print ")")
    (set! indent old-indent)))

(define (print-body codes)
  (for-each
   (lambda (code)
     (my-newline)
     (print-indent indent)
     (print-code code))
   codes))

(define (my-print x)
  (cond
   ((symbol? x) (let* ((out (symbol->string x))
                       (len (string-length out)))
                  (set! col (+ col len))
                  (display out)))
   ((number? x) (let* ((out (number->string x))
                       (len (string-length out)))
                  (set! col (+ col len))
                  (display out)))
   ((string? x) (display x) (set! col (+ col (string-length x))))
   ((pair? x) (begin
                (set! col (+ col 1)) (display "(")
                (my-print-list x)
                (set! col (+ col 1)) (display ")")))
   (else (error "don't know how to print" x))))

(define (my-print-list x)
  (cond
   ((pair? (cdr x)) (begin
                      (my-print (car x))
                      (my-print " ")
                      (my-print-list (cdr x))))
   ((null? (cdr x)) (my-print (car x)))
   (else (my-print (car x))
         (my-print " . ")
         (my-print (cdr x)))))

(define (my-newline)
  (set! col 0)
  (newline))

(define (print-code-top c)
  (print-code c)
  (my-newline))

(print-code-top '(let ((x 1) (y 2)) (cond ((foo? a) b) (else a (begin b c d)))))
#|
(let ((x 1)
      (y 2)
  (cond ((foo? a) b)
        (else a
              (begin b
                     c
                     d)))))
|#

(print-code-top '(case a ((a) b c) ((d) e) (else f)))

(quit)
