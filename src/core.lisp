(in-package #:clrpc.core)

(defparameter *procedures* (make-hash-table :test 'equal))
(defparameter *ts-types-file* #P"generated/types.ts")
(defparameter *ts-procedures-file* #P"generated/procedures.ts")

(defun ensure-generated-dir ()
  (ensure-directories-exist *ts-types-file*)
  (ensure-directories-exist *ts-procedures-file*))

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
               (let ((args (getf procedure :args)))
                 (format stream "~%export interface ~A_Input {~%" (string-upcase name))
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
    ;; Import all input types
    (maphash (lambda (name procedure)
               (declare (ignore procedure))
               (format stream "import type { ~A_Input } from './types';~%"
                      (string-upcase name)))
             *procedures*)
    (format stream "~%")
    ;; Generate procedure interfaces
    (maphash (lambda (name procedure)
               (declare (ignore procedure))
               (format stream "export interface ~A_Procedure extends RPCProcedure<~A_Input, string> {}~%"
                      (string-upcase name)
                      (string-upcase name)))
             *procedures*)))

(defun generate-ts-procedures-export ()
  (with-open-file (stream *ts-procedures-file*
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (format stream "~%export interface Procedures {~%")
    (maphash (lambda (name procedure)
               (declare (ignore procedure))
               (format stream "  ~A: ~A_Procedure;~%"
                      name
                      (string-upcase name)))
             *procedures*)
    (format stream "}~%")))

(defmacro define-procedure (name (&rest args) &body body)
  (let ((arg-names (mapcar #'car args))
        (arg-types (mapcar #'cadr args)))
    `(progn
       ;; Add procedure to hash table
       (setf (gethash ,(string-downcase (symbol-name name)) *procedures*)
             (list :args (list ,@(loop for (name type) in args
                                     collect `(list ,(string-downcase (symbol-name name)) ',type)))
                   :fn (lambda ,arg-names ,@body)))
       ;; Generate all TypeScript files from scratch
       (clear-generated-files)
       (generate-ts-types)
       (generate-ts-procedures)
       (generate-ts-procedures-export)
       ',name)))
