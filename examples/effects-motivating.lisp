(in-package :cl-user)
(defpackage :effects-motivating
  (:use :cl)
  (:shadow #:print #:read))
(in-package :effects-motivating)

;; This file contains a small, non-macro Common Lisp program demonstrating
;; how a subset of algebraic effects could be implemented using CL signals and
;; restarts.
;;
;; The full library (such as it is) has developed further functionality
;; beyond this program, such as giving effect handlers a return clause.
;; However this code is still useful to get a sense of the basic idea behind
;; the implementation, which can be obscured behind all of the macros.

;; Here are some papers and presentations on Algebraic Effects in Koka that
;; inspired this library:
;;
;; https://www.sciencedirect.com/science/article/pii/S1571066115000705
;; https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/algeff-tr-2016-v3.pdf
;; https://www.youtube.com/watch?v=6OFhD_mHtKA

(define-condition effect-condition (condition)
  ((handler-generic-func
     :initarg :handler-generic-func
     :reader handler-generic-func)))

(define-condition print-effect (effect-condition)
  ((text :initarg :text :reader text)))
(define-condition read-effect (effect-condition)
  ())

(defgeneric handle (handler condition))

(defmethod handle (handler (condition print-effect))
  (handle-print handler (text condition)))

(defmethod handle (handler (condition read-effect))
  (handle-read handler))

(defun resume (&optional value)
  (invoke-restart 'resume value))

(defun finish (&optional value)
  (invoke-restart 'finish value))

(defgeneric handle-print (handler text))
(defgeneric handle-read (handler))

(defun print (text)
  (restart-case
      (signal 'print-effect :handler-generic-func #'handle-print
                            :text text)
    (resume (value) value)))

(defun read ()
  (restart-case
      (signal 'read-effect :handler-generic-func #'handle-read)
    (resume (value) value)))

(defclass constant-io ()
  ())

(defmethod handle-print ((handler constant-io) text)
  (resume))

(defmethod handle-read ((handler constant-io))
  (resume "Steve"))

(defclass terminal-io ()
  ())

(defmethod handle-print ((handler terminal-io) text)
  (format t text)
  (resume))

(defmethod handle-read ((handler terminal-io))
  (resume (read-line)))

(defun greet ()
  (print "Please enter your name:")
  (let ((name (read)))
    (print (concatenate 'string "Hello, " name "!~%"))))

(defun greet-constant ()
  (handler-bind ((effect-condition
                   #'(lambda (condition)
                       (handle (make-instance 'constant-io)
                               condition))))
    (restart-case
        (progn
          (greet)
          (format t "After greet-constant"))
      (finish (value) value))))

(defun greet-terminal ()
  (handler-bind ((effect-condition
                   #'(lambda (condition)
                       (handle (make-instance 'terminal-io)
                               condition))))
    (restart-case
        (progn
          (greet)
          (format t "After greet-terminal"))
      (finish (value) value))))
