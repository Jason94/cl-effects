(in-package :cl-user)
(defpackage :effects-lambda-motivating
  (:use :cl)
  (:shadow #:print #:read))
(defpackage :effects-lambda-library
  (:use :cl)
  (:shadow #:print #:read))

;; https://www.sciencedirect.com/science/article/pii/S1571066115000705
;; https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/algeff-tr-2016-v3.pdf
;; https://www.youtube.com/watch?v=6OFhD_mHtKA

;;
;; Motivating Example
;;
(in-package :effects-lambda-motivating)
