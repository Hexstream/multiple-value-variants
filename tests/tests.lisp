(cl:defpackage #:multiple-value-variants_tests
  (:use #:cl #:parachute)
  (:import-from #:multiple-value-variants #:multiple-value))

(cl:in-package #:multiple-value-variants_tests)

(defmacro are (comp expected form &optional description &rest format-args)
  `(is ,comp ,expected (multiple-value-list ,form) ,description ,@format-args))

(define-test "featured examples"
  (is = 3
      (multiple-value () (progn 1 2 3)))
  (is = 0
      (length (multiple-value-list (multiple-value () (progn)))))
  (are equal '(nil other values)
       (multiple-value ()
         (and (values nil 'other 'values)
              t)))
  (are equal '(or :external)
       (multiple-value ()
         (or (find-symbol "OR" '#:cl)
             (find-symbol "XOR" '#:cl))))
  (is = 0
      (length (multiple-value-list (let ((hash (make-hash-table)))
                                     (multiple-value () (cond ((gethash 'key hash))
                                                              ((values))))))))
  (are equal '(value t)
       (let ((hash (make-hash-table)))
         (setf (gethash 'key hash) 'value)
         (multiple-value () (cond ((gethash 'key hash))
                                  ((values))))))
  (is = 0
      (length (multiple-value-list (multiple-value ()
                                     (when nil
                                       (print "side-effect"))))))
  (are equal '((3 1 5) (0 1/4 0.5))
       (multiple-value (2)
         (mapcar #'truncate '(3 5/4 5.5))))
  (are equal '((0 2 3/4) (a c))
       (multiple-value 2
         (mapcan (lambda (object)
                   (if (numberp object)
                       (values (list object) nil)
                       (values nil (list object))))
                 '(0 a 2 3/4 c))))
  (are equal '(((a b c d e) (b c d e) (c d e) (d e) (e))
               ((e d c b a) (e d c b) (e d c) (e d) (e))
               ((a b) (b c) (c d) (d e) (e nil)))
       (multiple-value 3
         (maplist (lambda (tail)
                    (values tail
                            (reverse tail)
                            (list (first tail) (second tail))))
                  '(a b c d e)))))
