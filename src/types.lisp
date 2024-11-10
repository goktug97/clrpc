(in-package #:clrpc.types)

(defvar *type-mappings* (make-hash-table :test 'equal))

(defun register-type-mapping (cl-type ts-type)
  (setf (gethash cl-type *type-mappings*) ts-type))

(defun cl-type-to-ts (cl-type)
  (or (gethash cl-type *type-mappings*)
      "any"))

(register-type-mapping :string "string")
(register-type-mapping :number "number")
(register-type-mapping :boolean "boolean")
(register-type-mapping 'string "string")
(register-type-mapping 'number "number")
(register-type-mapping 'boolean "boolean")
(register-type-mapping 'integer "number")
(register-type-mapping 'float "number")

