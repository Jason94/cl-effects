(in-package :cl-user)

(defpackage :effects
  (:use #:cl)
  (:import-from #:alexandria
    #:ensure-list)
  (:export
    #:def-effect
    #:def-handler
    #:with-handler

    #:resume
    #:finish))
