(in-package :cl-user)
(defpackage :effects-examples
  (:use :cl :effects)
  (:shadow #:print #:read))
(in-package :effects-examples)

;;
;; Basic IO Example
;;

(def-effect io
  (read ())
  (print (message)))

(def-handler terminal-io
  (read ()
    (resume (read-line)))
  (print (text)
    (cl:print text)
    (resume)))

(def-handler constant-read
  (read ()
    (resume "Steve")))

(defun greet ()
  (print "Please enter your name: ")
  (let ((name (read)))
    (print (concatenate 'string "Hello, " name "!~%"))))

(defun greet-with-terminal-io ()
  (with-handler (terminal-io)
    (greet)))

(defun greet-with-constant-read ()
  (with-handler (constant-read terminal-io)
    (greet)))

;;
;; Example: https://koka-lang.github.io/koka/doc/book.html#sec-abstracting-handlers
;;
(def-effect emit
  (emit (msg)))

(def-effect return
  (return-val ()))

(defun ehello ()
  (emit "hello")
  (emit "world"))

(def-handler console-emitter
  (emit (msg)
    (cl:print msg)))

(defun with-console-emitter ()
  (with-handler (console-emitter)
    (ehello)))

(def-handler (collecting-emitter
              handler
              ((lines :initform nil :accessor lines)))
  (emit (msg)
    (push msg (lines handler)))
  (:return (x)
    (cons x (reverse (lines handler)))))

(defun with-collecting-emitter ()
  (with-handler (collecting-emitter console-emitter)
    (ehello)))

;;
;; Iterators
;;

(def-effect iterate
  (yield (value)))

(defun iterate (lst)
  (when lst
    (yield (first lst))
    (iterate (rest lst))))

(def-handler stop-at-10
  (yield (value)
    (when (>= value 10)
      (finish))))

(def-handler print-yielded
  (yield (value)
    (print value)
    (resume)))

(defun print-elements-to-10 ()
  (with-handler (terminal-io)
    (with-handler (stop-at-10 print-yielded)
      (iterate (list 7 8 9 10 11 12 13)))))
