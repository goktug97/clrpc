(in-package #:clrpc.core)

(defparameter *procedures* (make-hash-table :test 'equal))
(defparameter *ts-types-file* #P"generated/types.ts")
(defparameter *ts-procedures-file* #P"generated/procedures.ts")

(defun ensure-generated-dir ()
  (ensure-directories-exist *ts-types-file*)
  (ensure-directories-exist *ts-procedures-file*))

(defun normalize-procedure-name (name)
  (substitute #\_ #\- (string name)))

(defun write-stored-clos-types (stream)
  "Write all stored CLOS type definitions to the stream"
  (maphash (lambda (type-name interface-def)
             (declare (ignore type-name))
             (format stream "~A" interface-def))
           clrpc.types:*clos-types*))

(defun clear-generated-files ()
  (ensure-generated-dir)
  (with-open-file (stream *ts-types-file*
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (format stream "// Generated TypeScript types~%"))
  (with-open-file (stream *ts-procedures-file*
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (format stream "import { RPCProcedure } from './client';~%")))

(defun generate-ts-types ()
  (with-open-file (stream *ts-types-file*
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (maphash (lambda (name procedure)
               (loop for (arg-name arg-type) in (getf procedure :args)
                     do (clrpc.types:generate-ts-interface arg-type)))
             *procedures*)
    (write-stored-clos-types stream)

    (maphash (lambda (name procedure)
               (let ((args (getf procedure :args)))
                 (format stream "~%export interface ~A_Input {~%" (string-upcase (normalize-procedure-name name)))
                 (loop for (arg-name arg-type) in args
                       do (format stream "  ~A: ~A;~%"
                                arg-name
                                (clrpc.types:cl-type-to-ts arg-type)))
                 (format stream "}~%")))
             *procedures*)))

(defun generate-ts-procedures ()
  (with-open-file (stream *ts-procedures-file*
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (format stream "import { RPCProcedure } from './client';~%")
    (maphash (lambda (name procedure)
               (declare (ignore procedure))
               (format stream "import type { ~A_Input } from './types';~%"
                      (string-upcase (normalize-procedure-name name))))
             *procedures*)
    (format stream "~%")
    ;; Generate procedure interfaces
    (maphash (lambda (name procedure)
               (declare (ignore procedure))
               (format stream "export interface ~A_Procedure extends RPCProcedure<~A_Input, string> {}~%"
                      (string-upcase (normalize-procedure-name name))
                      (string-upcase (normalize-procedure-name name))))
             *procedures*)))

(defun generate-ts-procedures-export ()
  (with-open-file (stream *ts-procedures-file*
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (format stream "~%export interface Procedures {~%")
    (maphash (lambda (name procedure)
               (declare (ignore procedure))
               (format stream "  ~(~A~): ~A_Procedure;~%"
                      (normalize-procedure-name name)
                      (string-upcase (normalize-procedure-name name))))
             *procedures*)
    (format stream "}~%")))

(defmacro define-procedure (name (&rest args) &body body)
  (let ((arg-names (mapcar #'car args))
        (arg-types (mapcar #'cadr args)))
    `(progn
       (setf (gethash ,(string-downcase (normalize-procedure-name (symbol-name name))) *procedures*)
             (list :args (list ,@(loop for (name type) in args
                                     collect `(list ,(string-downcase (normalize-procedure-name (symbol-name name))) ',type)))
                   :fn (lambda ,arg-names ,@body)))
       (clear-generated-files)
       (generate-ts-types)
       (generate-ts-procedures)
       (generate-ts-procedures-export)
       ',name)))
