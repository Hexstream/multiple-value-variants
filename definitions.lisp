(in-package #:multiple-value-variants)

(defun %recursively (forms last-transform function)
  (if forms
      (labels ((recurse (forms)
                 (destructuring-bind (current . rest) forms
                   (if rest
                       (let ((values (gensym (string '#:values))))
                         `(multiple-value-call
                              (lambda (&rest ,values)
                                ,(funcall function values (recurse rest)))
                            ,current))
                       (funcall last-transform current)))))
        (recurse forms))
      '(values)))

(define and () (&rest forms)
  (%recursively forms #'identity
                (lambda (values rest)
                  `(if (car ,values)
                       ,rest
                       (values-list ,values)))))

(define or () (&rest forms)
  (%recursively forms #'identity
                (lambda (values rest)
                  `(if (car ,values)
                       (values-list ,values)
                       ,rest))))

#+nil(define cond () (&whole whole &rest clauses)
       (if (every #'cdr clauses)
           whole
           (%recursively clauses ?
                         (lambda (values rest)
                           `(if (car ,values)
                                (values-list ,values)
                                ,else)))
           (and clauses
                (map-bind (reduce) (((clause else) clauses)
                                    (() :from-end t :initial-value nil))
                  (destructuring-bind (test &rest forms) clause
                    (if forms
                        `(cond (test ,@forms)
                               (t else))
                        (let ((values (gensym (string '#:values))))
                          `(multiple-value-call (lambda (&rest ,values)
                                                  (if (car ,values)
                                                      (values-list ,values)
                                                      ,else))
                             ,test))))))))

(define when () (test &body forms)
  `(cond (,test ,@forms)
         (t (values))))

(define unless () (test &body forms)
  `(cond ((not ,test) ,@forms)
         (t (values))))

(define prog1 () (result &body body)
  `(multiple-value-prog1 ,result ,@body))

;;; multiple-value-bind? aref? array-row-major-index? assoc? assoc-if? assoc-if-not? bit? sbit? butlast? nbutlast? car? cdr? cddr? rest? char? schar? count? count-if? count-if-not? delete delete-if delete-if-not remove remove-if remove-if-not destructuring-bind? dolist dotimes elt? every some notevery notany fill find find-if find-if-not first second third fourth fifth sixth seventh eighth ninth tenth gethash? map map-into? mapcan mapcar mapcon maplist member? member-if? member-if-not? mismatch? nth? nth-value? position? position-if? position-if-not? rassoc? rassoc-if? rassoc-if-not? reduce? replace? search?
