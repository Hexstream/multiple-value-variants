(in-package #:multiple-value-variants)

(defclass definitions-system (defsys:standard-system)
  ())

(defvar *definitions* (make-instance 'definitions-system))

(setf (defsys:locate (defsys:root-system) 'multiple-value)
      *definitions*)

(defgeneric form-lambda-list (object))
(defgeneric options-lambda-list (object))
(defgeneric expander (object))

(defclass definition () ())

(defclass standard-definition (definition defsys:name-mixin)
  ((%options-lambda-list :initarg :options-lambda-list
                         :reader options-lambda-list
                         :type list)
   (%form-lambda-list :initarg :form-lambda-list
                      :reader form-lambda-list
                      :type list)
   (%expander :initarg :expander
              :reader expander
              :type (or function symbol))
   (%atom-options-transformer :initarg :atom-options-transformer
                              :reader atom-options-transformer
                              :type (or function symbol)
                              :initform #'list)))

;; Maybe export at some point.
(defun %canonicalize-options (options definition)
  (if (listp options)
      options
      (let ((transformed
             (funcall (atom-options-transformer definition)
                      options)))
        (if (listp transformed)
            transformed
            (error "atom-options-transformer for ~S ~
                    returned ~S, which is not a list."
                   (defsys:name definition) transformed)))))

(define-condition not-found-chain (error)
  ((%form :initarg :form
          :reader form)
   (%chain :initarg :chain
           :reader chain
           :type list))
  (:report (lambda (not-found stream)
             (format stream "~S expansion of the following form failed:~@
                             ~S~2%Chain of attempted expansions: ~S"
                     'multiple-value-variants:multiple-value
                     (form not-found)
                     (chain not-found)))))

;; Maybe export at some point.
(defun %locate-expand (form env &aux chain (initial-form form))
  (check-type form (or cons symbol))
  (labels ((recurse (form)
             (etypecase form
               ((cons symbol)
                (let* ((operator (first form))
                       (definition (defsys:locate *definitions* operator :errorp nil)))
                  (if definition
                      (values definition form)
                      (attempt-macroexpansion form (list :macro operator)))))
               (symbol
                (attempt-macroexpansion form (list :symbol-macro form)))))
           (attempt-macroexpansion (form new-link)
             (push new-link chain)
             (multiple-value-bind (expansion expandedp) (macroexpand-1 form env)
               (if expandedp
                   (recurse expansion)
                   (error 'not-found-chain
                          :form initial-form
                          :chain (nreverse chain))))))
    (recurse form)))

;; Maybe export at some point.
(defun %canonicalize (options form &optional env)
  (multiple-value-bind (definition form) (%locate-expand form env)
    (values (%canonicalize-options options definition)
            form
            definition)))

(defun %expand (options form &optional env)
  (multiple-value-bind (options form definition) (%canonicalize options form env)
    (funcall (expander definition) options form env)))

(defun %extract-&whole (lambda-list)
  '(values whole-var lambda-list)
  (if (eq (first lambda-list) '&whole)
      (destructuring-bind (whole-var &rest lambda-list) (rest lambda-list)
        (values whole-var lambda-list))
      (values nil lambda-list)))

;; Not robust in the face of misplaced &environment.
;; Doesn't support dotted lambda-lists.
(defun %extract-&environment (macro-lambda-list)
  '(values env-var ordinary-lambda-list)
  (let ((tail (member '&environment macro-lambda-list)))
    (cond (tail
           (when (member '&environment tail)
             (error "More than one ~S parameter in ~S."
                    '&environment macro-lambda-list))
           (values (second tail)
                   (append (ldiff macro-lambda-list tail)
                           (cddr tail))))
          (t (values nil macro-lambda-list)))))

(defun %extract-&whole-&environment (macro-lambda-list)
  '(values lambda-list whole-var environment-var)
  (multiple-value-bind (whole-var lambda-list)
      (%extract-&whole macro-lambda-list)
    (multiple-value-bind (environment-var lambda-list)
        (%extract-&environment lambda-list)
      (values lambda-list whole-var environment-var))))

(defun %check-expected-operator (actual expected)
  (unless (eq actual expected)
    (error "Wrong operator ~S, expected ~S." actual expected)))

(defun %make-expander (name options-lambda-list form-lambda-list body)
  (let ((options-var (gensym (string '#:options)))
        (form-var (gensym (string '#:form)))
        (operator-var (gensym (string '#:operator))))
    (multiple-value-bind (options-env-var
                          options-lambda-list
                          form-lambda-list
                          form-whole-var
                          form-env-var)
        (multiple-value-call #'values
          (%extract-&environment options-lambda-list)
          (%extract-&whole-&environment form-lambda-list))
      (let* ((env-var (gensym (string '#:env)))
             (options-env-template
              (if options-env-var
                  (lambda (fill-in)
                    (list `(let ((,options-env-var ,env-var))
                             ,@fill-in)))
                  #'identity))
             (form-env-template
              (if form-env-var
                  (lambda (fill-in)
                    (list `(let ((,form-env-var ,env-var))
                             ,@fill-in)))
                  #'identity)))
        `(lambda (,options-var ,form-var ,env-var)
           ,@(unless (or options-env-var form-env-var)
                     (list `(declare (ignore ,env-var))))
           (destructuring-bind ,options-lambda-list ,options-var
             ,@(funcall
                options-env-template
                `((destructuring-bind (,@(and form-whole-var
                                              `(&whole ,form-whole-var))
                                       ,operator-var ,@form-lambda-list)
                      ,form-var
                    (%check-expected-operator ,operator-var ',name)
                    ,@(funcall form-env-template body))))))))))

(defun %ensure (name form-lambda-list options-lambda-list expander)
  (setf (defsys:locate *definitions* name)
        (make-instance 'standard-definition
                       :name name
                       :options-lambda-list options-lambda-list
                       :form-lambda-list form-lambda-list
                       :expander expander)))

(defmethod defsys:expand-definition ((system definitions-system) name environment args &key)
  (destructuring-bind (options-lambda-list form-lambda-list &body body) args
    `(%ensure ',name
              ',options-lambda-list
              ',form-lambda-list
              ,(%make-expander name options-lambda-list form-lambda-list body))))

(defmacro multiple-value (options &body form &environment env)
  (check-type form (cons t null))
  (%expand options (first form) env))
