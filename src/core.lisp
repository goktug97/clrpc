(defvar *procedures* (make-hash-table :test 'equal))
(defvar *ts-types-file* #P"generated/types.ts")

(defun ensure-generated-dir ()
  (ensure-directories-exist *ts-types-file*))

(defun generate-ts-types (name args)
  (ensure-generated-dir)
  (with-open-file (stream *ts-types-file*
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (format stream "~%export interface ~A_Input {~%" name)
    (loop for (arg-name arg-type) in args
          do (format stream "  ~A: ~A;~%"
                    (string-downcase (symbol-name arg-name))
                    (clrpc.types:cl-type-to-ts arg-type)))
    (format stream "}~%")))

(defmacro define-procedure (name (&rest args) &body body)
  (let ((arg-names (mapcar #'car args))
        (arg-types (mapcar #'cadr args)))
    `(progn
       (setf (gethash ,(string-downcase (symbol-name name)) *procedures*)
             (list :args ',args
                   :fn (lambda ,arg-names ,@body)))
       (generate-ts-types ',name ',args)
       ',name)))

