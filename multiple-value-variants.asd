(asdf:defsystem #:multiple-value-variants

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Gives access to multiple-value variants of operators through one macro: MULTIPLE-VALUE."

  :depends-on (#:map-bind #:enhanced-multiple-value-bind)

  :version "0.1"
  :serial cl:t
  :components ((:file "package")
               (:file "info")
               (:file "definitions")
               (:file "main")))
