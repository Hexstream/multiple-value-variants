(in-package #:multiple-value-variants)

(defvar *infos* (make-hash-table :test 'eq))

(defgeneric multiple-value-variants:name (object))
(defgeneric multiple-value-variants:form-lambda-list (object))
(defgeneric multiple-value-variants:options-lambda-list (object))
(defgeneric multiple-value-variants:expander (object))

(defclass multiple-value-variants:info () ())

(defclass multiple-value-variants:standard-info (info)
  ((%name :initarg :name
          :reader multiple-value-variants:name
          :type symbol)
   (%options-lambda-list :initarg :options-lambda-list
                         :reader multiple-value-variants:options-lambda-list
                         :type list)
   (%form-lambda-list :initarg :form-lambda-list
                      :reader multiple-value-variants:form-lambda-list
                      :type list)
   (%expander :initarg :expander
              :reader multiple-value-variants:expander
              :type (or function symbol))
   (%atom-options-transformer :initarg :atom-options-transformer
                              :reader multiple-value-variants:atom-options-transformer
                              :type (or function symbol)
                              :initform #'list)))

(defmethod print-object ((info multiple-value-variants:standard-info) stream)
  (print-unreadable-object (info stream :type t)
    (prin1 (multiple-value-variants:name info) stream)))

(defun multiple-value-variants:locate (name &key (errorp t))
  (check-type name symbol)
  (or (gethash name *infos*)
      (and errorp
           (error "No multiple-value-variant with name ~S." name))))

(defun (setf %locate) (new name &key (errorp t))
  (declare (ignore errorp))
  (check-type name symbol)
  (check-type new info)
  (setf (gethash name *infos*) new))

(defun multiple-value-variants:expand (options form &optional env)
  (check-type form cons)
  (let* ((operator (first form))
         (info (multiple-value-variants:locate operator))
         (options
          (if (listp options)
              options
              (let ((transformed
                     (funcall (multiple-value-variants:atom-options-transformer info)
                              options)))
                (if (listp transformed)
                    transformed
                    (error "atom-options-transformer for ~S ~
                            returned ~S, which is not a list."
                           operator transformed))))))
    (funcall (multiple-value-variants:expander info) options form env)))

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

(defun %remove-keys (keys plist)
  (let ((keys (if (listp keys) keys (list keys)))
        (processp nil))
    (map-bind (mapcan) ((key plist) (value (cdr plist)))
      (when (setf processp (not processp))
        (unless (member key keys)
          (list key value))))))

(defun multiple-value-variants:ensure
    (name form-lambda-list options-lambda-list expander
     &rest keys &key (class 'multiple-value-variants:standard-info) &allow-other-keys)
  (setf (%locate name)
        (apply #'make-instance class
               :name name
               :options-lambda-list options-lambda-list
               :form-lambda-list form-lambda-list
               :expander expander
               (%remove-keys :class keys))))

(defmacro multiple-value-variants:define
    (name options-lambda-list form-lambda-list &body body)
  `(multiple-value-variants:ensure
    ',name
    ',options-lambda-list
    ',form-lambda-list
    ,(%make-expander name options-lambda-list form-lambda-list body)))
