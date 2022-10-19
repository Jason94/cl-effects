(asdf:defsystem #:effects
  :description "A prototype effects library for Comomn Lisp."
  :author "Jason Walker <JasonW94@gmail.com>"
  :depends-on (#:alexandria)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "effects")))
