(cl:defpackage #:multiple-value-variants_tests
  (:use #:cl #:parachute)
  (:import-from #:multiple-value-variants #:multiple-value))

(cl:in-package #:multiple-value-variants_tests)

(define-test "featured examples"
  (is =
      (multiple-value () (progn 1 2 3))
      3)
  (is =
      (length (multiple-value-list (multiple-value () (progn))))
      0)
  (is equal
      (multiple-value-list (multiple-value ()
                             (and (values nil 'other 'values)
                                  t)))
      '(nil other values))
  (is equal
      (multiple-value-list (multiple-value ()
                             (or (find-symbol "OR" '#:cl)
                                 (find-symbol "XOR" '#:cl))))
      '(or :external))
  (is =
      (length (multiple-value-list (let ((hash (make-hash-table)))
                                     (multiple-value () (cond ((gethash 'key hash))
                                                              ((values)))))))
      0)
  (is equal
      (multiple-value-list (let ((hash (make-hash-table)))
                             (setf (gethash 'key hash) 'value)
                             (multiple-value () (cond ((gethash 'key hash))
                                                      ((values))))))
      '(value t))
  (is =
      (length (multiple-value-list (multiple-value ()
                                     (when nil
                                       (print "side-effect")))))
      0)
  (is equal
      (multiple-value-list (multiple-value (2)
                             (mapcar #'truncate '(3 5/4 5.5))))
      '((3 1 5) (0 1/4 0.5)))
  (is equal
      (multiple-value-list (multiple-value 2
                             (mapcan (lambda (object)
                                       (if (numberp object)
                                           (values (list object) nil)
                                           (values nil (list object))))
                                     '(0 a 2 3/4 c))))
      '((0 2 3/4) (a c)))
  (is equal
      (multiple-value-list (multiple-value 3
                             (maplist (lambda (tail)
                                        (values tail
                                                (reverse tail)
                                                (list (first tail) (second tail))))
                                      '(a b c d e))))
      '(((a b c d e) (b c d e) (c d e) (d e) (e))
        ((e d c b a) (e d c b) (e d c) (e d) (e))
        ((a b) (b c) (c d) (d e) (e nil)))))
