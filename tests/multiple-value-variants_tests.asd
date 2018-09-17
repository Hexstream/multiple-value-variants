(asdf:defsystem #:multiple-value-variants_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "multiple-value-variants unit tests."

  :depends-on ("multiple-value-variants"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:multiple-value-variants_tests)))
