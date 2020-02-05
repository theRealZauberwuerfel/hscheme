(define (not x)
  (if x
      #f
      #t))

(define (null? obj)
  (if (eqv? obj '())
      #t
      #f))

(define (list . objs)
  objs)

(define (id objs)
  obj)

(define (flip func)
  (lambda (arg1 arg2)
    (func arg2 arg1)))

(define (curry func arg1)
  (lambda (arg)
    (apply func (cons arg1 arg))))

(define (compose f g)
  (lambda (arg)
    (f (apply g arg))))

(define zero?
  (curry = 0))

(define positive?
  (curry < 0))

(define negative?
  (curry > 0))

(define (odd? num)
  (= (mod num 2) 1))

(define (even? num)
  (= (mod num 2) 0))

(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define fold foldl)

(define reduce fold)

(define (lstlisting func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (filter pred lst)
  (foldr (lambda (x y)
    (if (pred x)
      (cons x y)
      y))
    '() lst))
