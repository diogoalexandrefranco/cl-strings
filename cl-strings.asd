(defsystem #:cl-strings
           :name "cl-strings"
           :version "0.0.1"
           :description "A set of utilities for manipulating strings in CL."
           :author "Diogo Franco"
           :license "MIT"
           :serial t
           :components ((:file "package")
                        (:file "cl-strings")))

(defsystem #:cl-strings-tests
           :name "cl-strings-tests"
           :version "0.0.1"
           :description "The tests for the cl-strings system"
           :author "Diogo Franco"
           :depends-on (#:cl-strings #:prove)
           :components ((:file "tests")))
