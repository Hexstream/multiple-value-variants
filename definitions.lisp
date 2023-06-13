(in-package #:multiple-value-variants)

(define (multiple-value progn) (&key (identity '(values))) (&body forms)
  (if forms
      `(progn ,@forms)
      identity))

(define (multiple-value prog1) () (result &body body)
  `(multiple-value-prog1 ,result ,@body))

(defun %recursively (forms function &key
                     (last #'identity))
  (if forms
      (labels ((recurse (forms)
                 (destructuring-bind (current . rest) forms
                   (if rest
                       (funcall function current (recurse rest))
                       (funcall last current)))))
        (recurse forms))
      '(values)))

(defun %catching-values (function values-form)
  (let ((values (gensym (string '#:values))))
    `(multiple-value-bind (&rest ,values) ,values-form
       ,(funcall function values))))

(defun %handling-identity (name identity identityp forms function)
  (if forms
      (funcall function)
      (if identityp
          identity
          (error "(~S ~S) requires at least one form or an explicit ~S."
                 'multiple-value name :identity))))

(define (multiple-value and) (&key (identity nil identityp) (nth 0)) (&rest forms)
  (%handling-identity
   'and identity identityp forms
   (lambda ()
     (%recursively forms (lambda (current rest)
                           (%catching-values
                            (lambda (values)
                              `(if (nth ,nth ,values)
                                   ,rest
                                   (values-list ,values)))
                            current))))))

(define (multiple-value or) (&key (identity nil identityp) (nth 0)) (&rest forms)
  (%handling-identity
   'or identity identityp forms
   (lambda ()
     (%recursively forms (lambda (current rest)
                           (%catching-values
                            (lambda (values)
                              `(if (nth ,nth ,values)
                                   (values-list ,values)
                                   ,rest))
                            current))))))

(define (multiple-value cond) (&key (nth 0)) (&rest clauses)
  (%recursively
   clauses
   (lambda (current else)
     (destructuring-bind (condition &body body) current
       (if body
           (let ((test `(nth-value ,nth ,condition)))
             (if (rest body)
                 `(cond (,test ,@body)
                        (t ,else))
                 `(if ,test
                      ,(first body)
                      ,else)))
           (%catching-values
            (lambda (values)
              `(if (nth ,nth ,values)
                   ,(if body
                        `(progn ,@body)
                        `(values-list ,values))
                   ,else))
            condition))))
   :last (lambda (last)
           (destructuring-bind (condition &body body) last
             (if body
                 `(when (nth-value ,nth ,condition) ,@body)
                 condition)))))


(define (multiple-value when) (&key (else '(values)) (identity '(values) identityp))
    (test &body forms)
  `(if ,test
       (multiple-value (,@(when identityp (list :identity identity)))
         (progn ,@forms))
       ,else))

(define (multiple-value unless) (&key (else '(values)) (identity '(values) identityp))
    (test &body forms)
  `(if ,test
       ,else
       (multiple-value (,@(when identityp (list :identity identity)))
         (progn ,@forms))))


(defun %caselike (operator keyform cases &optional otherwisep)
  (list* operator
         keyform
         (let ((new-cases
                 (mapcar (lambda (case)
                           (destructuring-bind (keys &body body) case
                             `(,keys (multiple-value () (progn ,@body)))))
                         cases)))
           (if (and otherwisep
                    (not (member (car (first (last cases)))
                                 '(t otherwise))))
               (nconc new-cases (list '(t (values))))
               new-cases))))

(define (multiple-value case) () (keyform &body cases)
  (%caselike 'case keyform cases t))

(define (multiple-value ccase) () (keyplace &body cases)
  (%caselike 'ccase keyplace cases))

(define (multiple-value ecase) () (keyform &body cases)
  (%caselike 'ecase keyform cases))

(define (multiple-value typecase) () (keyform &body cases)
  (%caselike 'typecase keyform cases t))

(define (multiple-value ctypecase) () (keyplace &body cases)
  (%caselike 'ctypecase keyplace cases))

(define (multiple-value etypecase) () (keyform &body cases)
  (%caselike 'etypecase keyform cases))


(defun %make-list-accumulator ()
  '(values accumulate finish)
  (let* ((accumulated (cons :head nil))
         (tail accumulated))
    (values (lambda (new-value)
              (let ((new-cons (cons new-value nil)))
                (setf (cdr tail) new-cons
                      tail new-cons)
                new-value))
            (lambda ()
              (cdr accumulated)))))

(defun %make-nconc-accumulator ()
  '(values accumulate finish)
  (let* ((accumulated (cons :head nil))
         (tail accumulated))
    (values (lambda (new-values)
              (setf (cdr tail) new-values
                    tail (last tail))
              (values))
            (lambda ()
              (cdr accumulated)))))

(defun %make-gensym-generator (&optional default-base)
  (let ((count 0))
    (lambda (&optional
             (base
              (or default-base
                  (error "override-base must be specified because no default-base."))))
      (gensym (format nil "~A~D-" base (incf count))))))

(defun %expand-multiple-value-mapper (mapper accumulator-maker
                                      multiple-values-count function lists)
  (let ((element-vars (map-into (make-list (length lists))
                                (%make-gensym-generator '#:list)))
        (function-var (gensym (string '#:function))))
    (multiple-value-bind (value-vars accumulate-vars finish-vars)
        (flet ((vars (base) (map-into (make-list multiple-values-count)
                                      (%make-gensym-generator (string base)))))
          (values (vars '#:value) (vars '#:accumulate) (vars '#:finish)))
      `(let ((,function-var ,function) ,@accumulate-vars ,@finish-vars)
         (setf ,@(mapcan (let ((make-accumulator (list accumulator-maker)))
                           (plambda (list (list 'values :1 :2) make-accumulator)))
                         accumulate-vars
                         finish-vars))
         (,mapper (lambda (,@element-vars)
                    (multiple-value-bind (,@value-vars)
                        (funcall ,function-var ,@element-vars)
                      ,@(mapcar (plambda (list 'funcall :1 :2))
                                accumulate-vars
                                value-vars)))
                  ,@lists)
         (values ,@(mapcar (plambda (list 'funcall :1))
                           finish-vars))))))

(define (multiple-value mapcar) (multiple-values-count) (function list &rest more-lists)
  (%expand-multiple-value-mapper 'mapc '%make-list-accumulator
                                 multiple-values-count
                                 function (cons list more-lists)))

(define (multiple-value mapcan) (multiple-values-count) (function list &rest more-lists)
  (%expand-multiple-value-mapper 'mapc '%make-nconc-accumulator
                                 multiple-values-count
                                 function (cons list more-lists)))

(define (multiple-value maplist) (multiple-values-count) (function list &rest more-lists)
  (%expand-multiple-value-mapper 'mapl '%make-list-accumulator
                                 multiple-values-count
                                 function (cons list more-lists)))

(define (multiple-value mapcon) (multiple-values-count) (function list &rest more-lists)
  (%expand-multiple-value-mapper 'mapl '%make-nconc-accumulator
                                 multiple-values-count
                                 function (cons list more-lists)))

;;; aref? array-row-major-index? assoc? assoc-if? assoc-if-not? bit? sbit? butlast? nbutlast? car? cdr? cddr? rest? char? schar? count? count-if? count-if-not? delete delete-if delete-if-not remove remove-if remove-if-not destructuring-bind? dolist dotimes elt? every some notevery notany fill find find-if find-if-not first second third fourth fifth sixth seventh eighth ninth tenth gethash? map map-into? member? member-if? member-if-not? mismatch? nth? nth-value? position? position-if? position-if-not? rassoc? rassoc-if? rassoc-if-not? reduce? replace? search?
