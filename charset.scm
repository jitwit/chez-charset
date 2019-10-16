
(define greatest-char 55296) ;; for now

(define-record-type charset
  (fields members +/-?))

(define (pos C)
  (make-charset C #t))

(define (neg C)
  (make-charset C #f))

(define empty-set
  (pos set:empty-set))

(define (charset=? A B)
  (let ((a+? (charset-+/-? A))
        (b+? (charset-+/-? B))
        (a (charset-members A))
        (b (charset-members B)))
    (cond ((eq? a+? b+?) (set:set-equal? a b))
          (else
           (= greatest-char
              (set:set-size (set:symmetric-difference a b)))))))

(define (charset-empty? A)
  (if (charset-+/-? A)
      (set:empty? (charset-members A))
      (= greatest-char
         (set:set-size (charset-members A)))))

(define (charset-member? x A)
  (let ((x-A? (set:member? (char->integer x) (charset-members A)))
        (pos? (charset-+/-? A)))
    (if pos?
        x-A?
        (not x-A?))))

(define (charset-not-member? x A)
  (not (charset-member? x A)))

(define (charset-insert x A)
  (let ((a (charset-members A)))
    (if (charset-+/-? A)
        (pos (set:insert (char->integer x) a))
        (neg (set:delete (char->integer x) a)))))

(define (charset-delete x A)
  (let ((a (charset-members A)))
    (if (charset-+/-? A)
        (pos (set:delete (char->integer x) a))
        (neg (set:insert (char->integer x) a)))))

(define (charset-complement A)
  (make-charset (charset-members A)
                (not (charset-+/-? A))))

(define (charset-union A B)
  (let ((a (charset-members A))
        (b (charset-members B))
        (+a? (charset-+/-? A))
        (+b? (charset-+/-? B)))
    (cond ((and +a? +b?)
           (pos (set:union a b)))
          ((and +a? (not +b?))
           (neg (set:difference a b)))
          ((and +b? (not +a?))
           (neg (set:difference b a)))
          (else
           (neg (set:intersection a b))))))

(define (charset-intersection A B)
  (let ((a (charset-members A))
        (b (charset-members B))
        (+a? (charset-+/-? A))
        (+b? (charset-+/-? B)))
    (cond ((and +a? +b?)
           (pos (set:intersection a b)))
          ((and +a? (not +b?))
           (pos (set:difference a b)))
          ((and +b? (not +a?))
           (pos (set:difference b a)))
          (else
           (neg (set:union a b))))))

(define (charset-difference A B)
  (let ((a (charset-members A))
        (b (charset-members B))
        (+a? (charset-+/-? A))
        (+b? (charset-+/-? B)))
    (cond ((and +a? +b?)
           (pos (set:difference a b)))
          ((and +a? (not +b?))
           (pos (set:intersection a b)))
          ((and +b? (not +a?))
           (neg (set:union b a)))
          (else
           (neg (set:difference b a))))))

(define (charset-symmetric-difference A B)
  (charset-union (charset-difference A B)
                 (charset-difference B A)))

(define (list->charset xs)
  (fold-right charset-insert empty-set xs))

(define (integers->charset xs)
  (fold-right (lambda (x A)
                (charset-insert (integer->char x)
                                A))
              empty-set
              xs))

(define (string->charset s)
  (do ((j (1- (string-length s)) (1- j))
       (A empty-set (charset-insert (string-ref s j) A)))
      ((< j 0) A)))

(define charset:unicode
  (list->charset (map integer->char (iota greatest-char))))

(define (charset-build predicate)
  (pos (set:set-filter (lambda (x)
                         (predicate (integer->char x)))
                       (charset-members charset:unicode))))

(define (charset-filter predicate A)
  (let ((a (charset-members A)))
    (if (charset-+/-? A)
        (pos (set:set-filter (lambda (x)
                               (predicate (integer->char x)))
                             a))
        (neg (integers->charset
              (filter (lambda (x)
                        (and (not (predicate x))
                             (not (set:member? x a))))
                      (iota greatest-char)))))))

(define charset:upper-case
  (charset-build char-upper-case?))

(define charset:lower-case
  (charset-build char-lower-case?))

(define (charset->list A)
  (if (charset-+/-? A)
      (map integer->char (set:set->list (charset-members A)))
      (error 'charset->list "figure out all unicode ints" A)))





