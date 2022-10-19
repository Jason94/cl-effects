(in-package :cl-user)
(defpackage :effects-expanded
  (:use :cl)
  (:shadow #:print #:read))
(in-package :effects-expanded)

;; Not generated
(define-condition effect-condition (condition)
  ((handler-generic-func
     :initarg :handler-generic-func
     :reader handler-generic-func)))

(defgeneric handle (handler condition))

(defun resume (&optional value)
  (invoke-restart 'resume value))

(defun finish (&optional value)
  (invoke-restart 'finish value))

;; Generated
(PROGN
 (DEFGENERIC |handle-READ|
     (HANDLER)
   (:METHOD (GENERIC-HANDLER) NIL))
 (DEFINE-CONDITION |READ-effect|
     (EFFECT-CONDITION)
     NIL)
 (DEFMETHOD HANDLE (HANDLER (CONDITION |READ-effect|)) (|handle-READ| HANDLER))
 (DEFUN READ ()
   (RESTART-CASE (SIGNAL '|READ-effect| :HANDLER-GENERIC-FUNC #'|handle-READ|)
     (RESUME (VALUE) VALUE)))
 (DEFGENERIC |handle-PRINT|
     (HANDLER MESSAGE)
   (:METHOD (GENERIC-HANDLER MESSAGE) NIL))
 (DEFINE-CONDITION |PRINT-effect|
     (EFFECT-CONDITION)
     ((MESSAGE :INITARG :MESSAGE :READER MESSAGE)))
 (DEFMETHOD HANDLE (HANDLER (CONDITION |PRINT-effect|))
   (|handle-PRINT| HANDLER (MESSAGE CONDITION)))
 (DEFUN PRINT (MESSAGE)
   (RESTART-CASE (SIGNAL '|PRINT-effect| :HANDLER-GENERIC-FUNC #'|handle-PRINT|
                         :MESSAGE MESSAGE)
     (RESUME (VALUE) VALUE))))

(PROGN
 (DEFCLASS TERMINAL-IO NIL NIL)
 (DEFMETHOD |handle-READ| ((HANDLER TERMINAL-IO))
   (DECLARE (IGNORABLE HANDLER))
   (RESUME (READ-LINE)))
 (DEFMETHOD |handle-PRINT| ((HANDLER TERMINAL-IO) TEXT)
   (DECLARE (IGNORABLE HANDLER))
   (FORMAT T TEXT)
   (RESUME)))

;; Not generated
(defun greet ()
  (print "Please enter your name: ")
  (let ((name (read)))
    (print (concatenate 'string "Hello, " name "!~%"))))

;; Function body generated
(defun greet-with-constant-read ()
  (HANDLER-BIND ((EFFECT-CONDITION
                  #'(LAMBDA (CONDITION)
                      (HANDLE (MAKE-INSTANCE 'CONSTANT-READ) CONDITION)))
                 (EFFECT-CONDITION
                  #'(LAMBDA (CONDITION)
                      (HANDLE (MAKE-INSTANCE 'TERMINAL-IO) CONDITION))))
    (RESTART-CASE (PROGN (GREET))
      (FINISH (VALUE) VALUE))))
