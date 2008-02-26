(define (log2) 

  ;; x = 1/log(2)*10^68
  (define x 144269504088896340735992468100189213742664595415298593413544940693110)

  (let loop ((x (* (/ x) (expt 10 69))) (n 70) (s '()))
    (if (zero? n)
        (string-append "0." (apply string-append (reverse s)))
        (loop (* 10 (- x (floor x))) (- n 1) (cons (number->string (floor x)) s)))))

(define (sqrt1/2)

  ;; x = 1/sqrt(1/2)
  (define x 141421356237309504880168872420969807856967187537694807317667973799073)

  (let loop ((x (* (/ x) (expt 10 69))) (n 70) (s '()))
    (if (zero? n)
        (string-append "0." (apply string-append (reverse s)))
        (loop (* 10 (- x (floor x))) (- n 1) (cons (number->string (floor x)) s)))))

