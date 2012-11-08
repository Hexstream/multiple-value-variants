(in-package #:multiple-value-variants)

(define progn (&key (identity '(values))) (&body forms)
  (if forms
      `(progn ,@forms)
      identity))

(define prog1 () (result &body body)
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

(define and (&key (identity nil identityp) (nth 0)) (&rest forms)
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

(define or (&key (identity nil identityp) (nth 0)) (&rest forms)
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

(define cond (&key (nth 0)) (&rest clauses)
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


(define when (&key (else '(values)) (identity '(values) identityp))
    (test &body forms)
  `(if ,test
       (multiple-value (,@(when identityp (list :identity identity)))
         (progn ,@forms))
       ,else))

(define unless (&key (else '(values)) (identity '(values) identityp))
    (test &body forms)
  `(if ,test
       ,else
       (multiple-value (,@(when identityp (list :identity identity)))
         (progn ,@forms))))


(defun %caselike (operator keyform cases &optional otherwisep)
  (list* operator
         keyform
         (let ((new-cases
                (map-bind (mapcar) ((case cases))
                  (destructuring-bind (keys &body body) case
                    `(,keys (multiple-value () (progn ,@body)))))))
           (if (and otherwisep
                    (not (member (car (first (last cases)))
                                 '(t otherwise))))
               (nconc new-cases (list '(t (values))))
               new-cases))))

(define case () (keyform &body cases)
  (%caselike 'case keyform cases t))

(define ccase () (keyplace &body cases)
  (%caselike 'ccase keyplace cases))

(define ecase () (keyform &body cases)
  (%caselike 'ecase keyform cases))

(define typecase () (keyform &body cases)
  (%caselike 'typecase keyform cases t))

(define ctypecase () (keyplace &body cases)
  (%caselike 'ctypecase keyplace cases))

(define etypecase () (keyform &body cases)
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

(defun %make-gensym-generator (&optional default-base)
  (let ((count 0))
    (lambda (&optional
             (base
              (or default-base
                  (error "override-base must be specified because no default-base."))))
      (gensym (format nil "~A~D-" base (incf count))))))

#+nil
(define mapcar (multiple-values-count) (function list &rest more-lists)
  (let ((lists (cons list more-lists)))
    (multiple-value-bind (accumulate-vars finish-vars)
        (flet ((vars (base) (map-into (make-list multiple-values-count)
                                      (%make-gensym-generator (string base)))))
          (values (vars '#:accumulate) (vars '#:finish)))
      `(let ?
         (mapc)
         (values )))))

;;; aref? array-row-major-index? assoc? assoc-if? assoc-if-not? bit? sbit? butlast? nbutlast? car? cdr? cddr? rest? char? schar? count? count-if? count-if-not? delete delete-if delete-if-not remove remove-if remove-if-not destructuring-bind? dolist dotimes elt? every some notevery notany fill find find-if find-if-not first second third fourth fifth sixth seventh eighth ninth tenth gethash? map map-into? mapcan mapcar mapcon maplist member? member-if? member-if-not? mismatch? nth? nth-value? position? position-if? position-if-not? rassoc? rassoc-if? rassoc-if-not? reduce? replace? search?
