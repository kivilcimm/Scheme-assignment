#lang racket

        ;1: procedure that read and return a strings list which include line of wine.data as a string
(define (file->string_list path)      
  (call-with-input-file path
    (lambda (input-port)
      (let loop ((x (read-line input-port)))
        (cond
          ((eof-object? x) '())
          (else (begin(cons x (loop(read-line input-port))))))))))

         ;2:procedure that return an int list which are divided \, in a large string.
(define (split_line str)

  (define (string-first str)
    (make-string 1 (string-ref str 0)))

  (define (string-rest str)
    (substring str 1 (string-length str)))

  (define (split-line-helper str a lst)
  (cond 
    [(string=? str "") (reverse (cons (string->number a) lst))]
    [else
     (cond
       [(char=? (string-ref str 0) #\,) (split-line-helper (string-rest str) "" (cons (string->number a) lst))]
       [else
        (split-line-helper (string-rest str) (string-append a (string-first str)) lst)]
       )
     ]
    )
  )

  (split-line-helper str "" empty))



        ;3:procedure that return a list that include all line as a sub int-list.
(define (string_list->int_list lst string_list)
  (cond
    ((null? string_list)(reverse lst))
    (else(string_list->int_list (cons (split_line (car string_list)) lst) (cdr string_list)))))


       ;4:procedure that return an attribute on a given index(n) in a given list.
(define(attribute lst n)
    (if (<= n 0)
        (car lst)
        (attribute (cdr lst)(- n 1))))

        ;5:procedure that return an attribute list according to n(attribute number that start 0(class) to 13)
(define (attribute_list lst line_lst  n)
  (cond
    ((null? line_lst) (reverse lst))
    (else(attribute_list (cons (attribute (car line_lst) n) lst) (cdr line_lst) n))))
  




        ;6:for paragraph 1 in homework: procedure that return mean of a attibute list
(define(attribute_mean attribute_list)
  (let loop ((a_sum 0) (count 0) (lst attribute_list))
    (cond
      ((null? lst) (/ a_sum count))
      (else(loop (+ a_sum (car lst)) (+ count 1)(cdr lst))))))



       ;7:for paragraph 1 in homework: procedure that return median of a attribute list
(define (attribute_median attribute_list)
  (let ([index (/ (length attribute_list) 2)])
  (cond
    ((integer? index) (/(+ (attribute attribute_list (- index 1)) (attribute attribute_list (+ index 1)) ) 2))
    (else (attribute attribute_list (- index 0.5))))))



       ;procedure that calculate square ,it is used to calculate variance in attribute_variance procedure
(define (square x) (* x x))

       ;8:for paragraph 1 in homework: procedure that return variance of a attribute list 
(define(attribute_variance attribute_list)
  (let loop ((a_sum 0) (index 0) (n (length attribute_list)) )
    (cond
      ((equal? index (- n 1)) (/ a_sum n))
      (else(loop (+ a_sum (square (- (attribute attribute_list index) (attribute_mean attribute_list)))) (+ index 1) n)))))


      ;9:for paragraph 1 in homework: procedure that return lists for mean, median and variance of all attributes of all data instances
(define (attribute_lists lst)
    (let loop ((l lst) (mean '()) (median '()) (variance '()) (n 1))
      (cond
        ((equal? n 14) (values 'means: (reverse mean) 'medians: (reverse median) 'variance: (reverse variance)))
        (else
         (loop  lst (cons (attribute_mean (attribute_list '() l n)) mean)
                (cons (attribute_median (attribute_list '() l n)) median)
                (cons(attribute_variance (attribute_list '() l n)) variance)
                (+ n 1))))))





      ;10:for paragraph 2 in homework: procedure that divide all data instance two list(training(%80) and test(%20)) and return one of these list according to list name(lst_name)
(define (training_test_list lst lst_name)
  (let loop ((train_l '()) (test_l '())(index 0) )
    (cond
      ((equal? (+ index 1) (length lst))
       (if(equal? lst_name "test")
          test_l
          train_l))
      (else(cond
         ((> index (/ (* (length lst) 80) 100)) (loop train_l (cons (attribute lst index) test_l) (+ index 1)))
         (else(loop (cons (attribute lst index) train_l) test_l (+ index 1))))))))




        ;11:for paragraph 3 in homework: procedure that compare estimate class to real class , and calculate percentage of accuracy
(define (calculate_percentage lst)

        ;procedure that randomly classify data instance, and return a list that include pairs(estimate_class data_instance)
  (define (random_classify lst)
    (let loop ((e '()) (l lst))
      (cond((null? l) (reverse e)) 
        (else(loop (cons (list (random 1 4)(car l)) e) (cdr l))))))

  
  (let loop ((t 0) (l (random_classify lst)) )
    (cond((null? l)
         (values 'true_number: t 'length_of_test_list: (length lst) 'accuracy: (*(/ t (length lst)) 100))) ;calculate percentage
         (else(cond
                ((equal? (caar l) (caar (cdr (car l)))) (loop (+ t 1) (cdr l)))
                (else(loop t (cdr l))))))))



       ;call procedure to read txt and create/print a list that include all line in wine.data.txt
;(string_list->int_list '() (file->string_list "wine.data.txt"))


       ;call procedure to print list of attribute 1(Alcohol) 
;(attribute_list '() (string_list->int_list '() (file->string_list "wine.data.txt")) 1)

       ;call procedure to print all list of means, medians and variances of all attributes(homework 1 paragraph)
;(attribute_lists (string_list->int_list '() (file->string_list "wine.data.txt")) )


       ;call procedure to print list of test data instances which have been chosen at shuffled list of all data instances(homework 2 paragraph)
;(training_test_list (shuffle (string_list->int_list '() (file->string_list "wine.data.txt"))) "test")

       ;call procedure to calculate accuracy of estimate class of data instances in test list(homework 3 paragraph)
;(calculate_percentage (training_test_list (shuffle (string_list->int_list '() (file->string_list "wine.data.txt"))) "test"))












