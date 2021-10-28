(defsystem "nominal"
  :version "0.1.0"
  :author "John Jacobsen"
  :license "MIT"
  :depends-on ("arrow-macros"
               "cl-oju"
               "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "animals"))))
  :description "A library for random name generation")
