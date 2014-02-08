(cl:defpackage #:multiple-value-variants
  (:nicknames #:mv-variants #:mv-variant #:multiple-value-variant)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:import-from #:positional-lambda #:plambda)
  (:shadowing-import-from #:enhanced-multiple-value-bind #:multiple-value-bind)
  (:export #:multiple-value ; import this single symbol for normal usage.

           #:info
           #:standard-info
           #:name
           #:form-lambda-list
           #:options-lambda-list
           #:expander
           #:atom-options-transformer

           #:not-found
           #:not-found-chain
           #:form
           #:chain

           #:locate
           #:expand
           #:ensure
           #:define))
