(asdf:defsystem #:multiple-value-variants

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Gives access to multiple-value variants of operators through one macro: MULTIPLE-VALUE. There are built-in variants for some standard operators; it's easy to create your own variants for other operators. The multiple-value mapping operators are especially useful."

  :depends-on (#:map-bind
               #:positional-lambda
               #:enhanced-multiple-value-bind)

  :version "1.0.1"
  :serial cl:t
  :components ((:file "package")
               (:file "info")
               (:file "definitions")
               (:file "main")))
