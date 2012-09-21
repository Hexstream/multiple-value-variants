(in-package #:multiple-value-variants)

(defmacro multiple-value (options &body form &environment env)
  (check-type form (cons t null))
  (multiple-value-variants:expand options (first form) env))
