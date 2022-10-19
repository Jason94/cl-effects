(in-package :effects)

(define-condition effect-condition (condition)
  ((handler-generic-func
     :initarg :handler-generic-func
     :reader handler-generic-func)))

(defgeneric handle (handler condition))

(defgeneric effect-return (handler value)
  (:method (handler value)
    value))

(defun resume (&optional value)
  (invoke-restart 'resume value))

(defun finish (&optional value)
  (invoke-restart 'finish value))

(eval-when (:compile-toplevel :load-toplevel :execute)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-handler-class (handler-class handler-class-slots)
    `(defclass ,handler-class ()
       (,@handler-class-slots)))

  (defun expand-operation-handle (handler-class handler-sym op-handle-spec)
    (destructuring-bind (op-name lambda-list &body body)
                        op-handle-spec
      `(defmethod ,(handle-generic-func-symbol op-name)
                  ((,handler-sym ,handler-class) ,@lambda-list)
         (declare (ignorable ,handler-sym))
         ,@body)))

  (defun expand-return-spec (handler-class handler-sym return-spec)
    (when return-spec
      (destructuring-bind (return-keyword lambda-list &body body) return-spec
        (declare (ignorable return-keyword)) ;; Just used to show return op
        `(defmethod effect-return ((,handler-sym ,handler-class) ,@lambda-list)
           ,@body)))))

(defmacro def-handler (handler-spec &body return-and-op-handle-specs)
  "handler-spec : handler-class OR
                  (handler-class) OR
                  (handler-class handler-bind-sym) OR
                  (handler-class handler-bind-sym handler-class-slots)
  op-handle-specs : (:return or normal function spec)
  return spec : (:return (|VAL-SYMBOL|) forms...)
          Note that the return spec will capture the handler-bind-sym, if given"
  (destructuring-bind (handler-class &optional raw-handler-sym handler-class-slots)
                      (ensure-list handler-spec)
    (let ((handler-sym (or raw-handler-sym
                           (gensym "handler")))
          (op-handle-specs (remove-if #'(lambda (op-spec)
                                          (equal (first op-spec) :return))
                                      return-and-op-handle-specs))
          (return-spec (find-if #'(lambda (op-spec)
                                     (equal (first op-spec) :return))
                                return-and-op-handle-specs)))
      `(progn
         ,(expand-handler-class handler-class handler-class-slots)
         ,@(mapcar #'(lambda (op-handle-spec)
                       (expand-operation-handle handler-class handler-sym op-handle-spec))
                   op-handle-specs)
         ,(expand-return-spec handler-class handler-sym return-spec)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-handler-let-clause (h-class-name h-class-sym)
    `(,h-class-sym (make-instance ',h-class-name)))

  (defun expand-handler-effect-clause (handler-obj-sym)
    `(effect-condition
       #'(lambda (condition)
           (handle ,handler-obj-sym
                   condition)))))

;; TODO: Add a lower-level macro that allows you to use your own handler object
(defmacro with-handler (handler-class-names &body body)
  (let ((handler-obj-syms (mapcar #'(lambda (name)
                                      (gensym (string name)))
                                  handler-class-names)))
    `(let (,@(mapcar #'expand-handler-let-clause handler-class-names
                                                 handler-obj-syms))
       (reduce #'(lambda (handler value)
                   (effect-return handler value))
               (list ,@handler-obj-syms)
               :from-end t
               :initial-value
               (handler-bind (,@(mapcar #'expand-handler-effect-clause
                                        handler-obj-syms))
                 (restart-case
                     (progn
                       ,@body)
                   (finish (value) value)))))))
