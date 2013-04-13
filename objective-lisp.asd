;;;; objective-lisp.asd

(asdf:defsystem #:objective-lisp
  :serial t
  :description "A :cl replacement that sands off some annoying things about Common Lisp while maintaining compatibility with CL libraries."
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "objective-lisp")))

