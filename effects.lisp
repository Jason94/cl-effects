(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package :cl-user)
(ql:quickload "alexandria")
(defpackage :effects-motivating
  (:use :cl)
  (:import-from :alexandria
    #:ensure-list)
  (:shadow #:print #:read))
(defpackage :effects-library
  (:use :cl)
  (:import-from :alexandria
    #:ensure-list)
  (:shadow #:print #:read))
;; https://www.sciencedirect.com/science/article/pii/S1571066115000705
;; https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/algeff-tr-2016-v3.pdf
;; https://www.youtube.com/watch?v=6OFhD_mHtKA

;;
;; Motivating Example
;;
(in-package :effects-motivating)

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


;;
;; Library
;;
(in-package :effects-library)

(define-condition effect-condition (condition)
  ((handler-generic-func
     :initarg :handler-generic-func
     :reader handler-generic-func)))

(defgeneric handle (handler condition))

(defun resume (&optional value)
  (invoke-restart 'resume value))

(defun finish (&optional value)
  (invoke-restart 'finish value))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defun condition-symbol (op-name)
    (intern (concatenate 'string (string op-name)
                                 "-effect")))

  (defun handle-generic-func-symbol (op-name)
    (intern (concatenate 'string "handle-"
                                 (string op-name))))

  (defun expand-operation-condition (op-name op-lambda-list)
    ;; TODO: handle optional, key, etc here
    (let ((effect-args (mapcar
                         #'(lambda (arg-sym)
                             `(,arg-sym
                               :initarg ,(intern (string arg-sym) :keyword)
                               :reader ,arg-sym))
                         op-lambda-list)))
      `(define-condition ,(condition-symbol op-name) (effect-condition)
         (,@effect-args))))

  (defun expand-condition-handler (op-name op-lambda-list)
    "Generate the method that specializes on the condition to dispatch the
     operation-specific generic function. The operation-specific generic function
     is then specialized on by the different handler classes. Also unpacks
     the values from the condition instance and uses them as function arguments
     to the operation call."
    ;; TODO: handle optional, key, etc here
    (let ((arguments (mapcar #'(lambda (arg-sym)
                                 `(,arg-sym condition))
                             op-lambda-list)))
      `(defmethod handle (handler (condition ,(condition-symbol op-name)))
           (,(handle-generic-func-symbol op-name) handler ,@arguments))))

  (defun expand-signaling-function (op-name op-lambda-list)
    ;; TODO: handle optional, key, etc here
    (let ((signal-args (mapcan #'(lambda (arg-sym)
                                   (list (intern (string arg-sym) :keyword)
                                         arg-sym))
                               op-lambda-list)))
      `(defun ,op-name (,@op-lambda-list)
         (restart-case
            (signal ',(condition-symbol op-name)
                     :handler-generic-func #',(handle-generic-func-symbol
                                                 op-name)
                     ,@signal-args)
           (resume (value) value)))))

  (defun expand-operation (operation-spec)
    (destructuring-bind (op-name op-lambda-list)
                        operation-spec
      (list
        `(defgeneric ,(handle-generic-func-symbol op-name) (handler ,@op-lambda-list)
           (:method (generic-handler ,@op-lambda-list)
             nil))
        (expand-operation-condition op-name op-lambda-list)
        (expand-condition-handler op-name op-lambda-list)
        (expand-signaling-function op-name op-lambda-list)))))

(defmacro def-effect (effect &body body)
  (declare (ignorable effect))
  `(progn
    ,@(mapcan #'expand-operation body)))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-handler-class (handler-class handler-class-slots)
    `(defclass ,handler-class ()
       (,@handler-class-slots)))

  (defun expand-operation-handle (handler-class handler-sym op-handle-spec)
    (destructuring-bind (op-name lambda-list &body body)
                        op-handle-spec
      `(defmethod ,(handle-generic-func-symbol op-name)
                  ((,handler-sym ,handler-class) ,@lambda-list)
         (declare (ignorable ,handler-sym))
         ,@body))))

(defmacro def-handler (handler-spec &body op-handle-specs)
  "handler-spec : handler-class OR
                  (handler-class) OR
                  (handler-class handler-bind-sym) OR
                  (handler-class handler-bind-sym handler-class-slots)"
  (destructuring-bind (handler-class &optional raw-handler-sym handler-class-slots)
                      (ensure-list handler-spec)
    (let ((handler-sym (or raw-handler-sym
                           (gensym "handler"))))
      `(progn
         ,(expand-handler-class handler-class handler-class-slots)
         ,@(mapcar #'(lambda (op-handle-spec)
                       (expand-operation-handle handler-class handler-sym op-handle-spec))
                   op-handle-specs)))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-handler-let-clause (h-class-name-and-sym)
    `(,(car h-class-name-and-sym) (make-instance ',(cdr h-class-name-and-sym))))

  (defun expand-handler-effect-clause (h-class-name-and-sym)
    `(effect-condition
       #'(lambda (condition)
           (handle ,(car h-class-name-and-sym)
                   condition)))))

;; TODO: Add a lower-level macro that allows you to use your own handler object
(defmacro with-handler (handler-class-names &body body)
  (let ((h-class-name-and-sym-lst (mapcar #'(lambda (name)
                                              (cons
                                                (gensym (string name))
                                                name))
                                          handler-class-names)))
    `(let (,@(mapcar #'expand-handler-let-clause h-class-name-and-sym-lst))
       (handler-bind (,@(mapcar #'expand-handler-effect-clause
                                h-class-name-and-sym-lst))
         (restart-case
             (progn
               ,@body)
           (finish (value) value))))))

;;
;; Library Example
;;

(def-effect io
  (read ())
  (print (message)))

(def-handler terminal-io
  (read ()
    (resume (read-line)))
  (print (text)
    (format t text)
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
    (push msg (lines handler))
    (resume))
  (return-val ()
    (resume (reverse (lines handler)))))

(defun with-collecting-emitter ()
  (with-handler (collecting-emitter)
    (ehello)
    (return-val)))
