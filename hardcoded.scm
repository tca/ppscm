(define indent 0)
(define line 0)
(define col 0)

(define (print-indent) (print-indent-aux indent))

(define (print-indent-aux n)
  (if (> n 0)
      (begin (my-print " ") (print-indent-aux (- n 1)))
      '()))

(define (print-code code)
  (if (pair? code)
      (print-list code #t)
      (print-atom code)))

(define (print-list code head)
  (case (car code)
    ((lambda) (print-lambda code))
    ((let) (print-let code))
    ((cond) (print-cond code))
    ((case) (print-case code))
    ((begin) (print-begin-like code))
    (else (print-combination code))))
  
(define (print-atom code)
  (my-print code))

(define (print-combination code)
  (my-print "(")
  (print-code (car code))
  (for-each
   (lambda (e)
     (my-print " ")
     (print-code e))
   (cdr code))
  (my-print ")"))

(foo (lambda (a)
       b
       c)
     d)

(define (print-lambda code)
  (let ((parameters (cadr code))
        (body (cddr code)))
    (fluid-let ((indent (+ 2 col)))
      (my-print "(lambda ")
      (my-print parameters)
      (for-each
       (lambda (e)
         (my-newline)
         (print-indent)
         (my-print e))
       body)
      (my-print ")"))))
      

(define (print-let code)
  (let ((bindings (cadr code))
        (body (cddr code)))
    (my-print "(")
    (my-print (car code))
    (my-print " ")
    (my-print "(")
    (fluid-let ((indent col))
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
                    (print-indent)
                    (my-print "(")
                    (my-print (car (car bs)))
                    (my-print " ")
                    (print-code (cadr (car bs)))
                    (my-print ")")
                    (loop (cdr bs))))))))
    (fluid-let ((indent  (+ 2 indent)))
      (print-body body)
      (my-print ")"))))

(define (print-begin-like c)
  (my-print "(")
  (print-code (car c))
  (my-print " ")
  (fluid-let ((indent col))
    (print-code (cadr c))
    (for-each (lambda (c)
                (my-newline)
                (print-indent)
                (print-code c))
              (cddr c)))
  (my-print ")"))

(define (print-cases code)
  (fluid-let ((indent col))
    (print-begin-like (car code))
    (for-each
     (lambda (c)
       (my-newline)
       (print-indent)
       (print-begin-like c))
     (cdr code))))

(define (print-case code)
  (let ((clauses (cddr code)))
    (my-print "(case ")
    (print-code (cadr code))
    (fluid-let ((indent (+ 2 indent)))
      (my-newline)
      (print-indent)
      (print-cases clauses))
    (my-print ")")))


(define (print-cond code)
  (let ((clauses (cdr code)))
    (my-print "(cond ")
    (print-cases clauses)
    (my-print ")")))

(define (print-body codes)
  (for-each
   (lambda (code)
     (my-newline)
     (print-indent)
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
   ((string? x) (begin
                  (set! col (+ col (string-length x)))
                  (display x)))
   ((pair? x) (begin
                (my-print "(")
                (my-print-list x)
                (my-print ")")))
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
  (set! line (+ 1 line))
  (newline))

(define (print-code-top c)
  (print-code c)
  (my-newline))

(print-code-top '(let ((x 1) (y 2) (z 3)) (cond ((foo? a) b) (else a (begin b c d)))))
#|
(let ((x 1)
      (y 2)
      (z 3))
  (cond ((foo? a) b)
        (else a
              (begin b
                     c
                     d))))
|#

(print-code-top '(case a ((a) b c) ((d) e) (else f)))
#|
(case a
  ((a) b
       c)
  ((d) e)
  (else f))
|#

(print-code-top '(map (lambda (x) (print x) (+ x 1)) (list 1 2 3)))

(quit)
