(asdf:defsystem #:multiple-value-variants

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Gives access to multiple-value variants of operators through one macro: MULTIPLE-VALUE. There are built-in variants for some standard operators; it's easy to create your own variants for other operators. The multiple-value mapping operators are especially useful."

  :depends-on (#:definitions-systems
               #:map-bind
               #:positional-lambda
               #:enhanced-multiple-value-bind)

  :version "1.0.1"
  :serial cl:t
  :components ((:file "package")
               (:file "defsys")
               (:file "definitions"))

  :in-order-to ((asdf:test-op (asdf:test-op #:multiple-value-variants_tests))))
