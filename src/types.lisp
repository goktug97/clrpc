(in-package #:clrpc.types)

(defvar *type-mappings* (make-hash-table :test 'equal))
(defvar *clos-types* (make-hash-table :test 'equal))

(defun register-type-mapping (cl-type ts-type)
  (setf (gethash cl-type *type-mappings*) ts-type))

(defun normalize-type-name (name)
  (string-upcase (substitute #\_ #\- (string name))))

(defun cl-type-to-ts (cl-type)
  (cond
    ;; Check if it's a built-in type mapping
    ((gethash cl-type *type-mappings*)
     (gethash cl-type *type-mappings*))
    ;; Check if it's a CLOS class
    ((and (not (member cl-type '(string number boolean integer float)))
          (find-class cl-type nil))
     (string-capitalize (string cl-type)))
    ;; Default fallback
    (t "any")))

(defun should-generate-interface-p (type)
  "Returns true if we should generate a TypeScript interface for this type"
  (and (not (member type '(string number boolean integer float)))
       (find-class type nil)))

(defun generate-ts-interface (class-name)
  (when (should-generate-interface-p class-name)
    (let* ((class (find-class class-name))
           (slots (closer-mop:class-direct-slots class)))
      ;; Store the interface definition for later regeneration
      (setf (gethash class-name *clos-types*)
            (with-output-to-string (s)
              (format s "~%export interface ~A {~%" (string-capitalize (string class-name)))
              (loop for slot in slots
                    for slot-name = (closer-mop:slot-definition-name slot)
                    for slot-type = (closer-mop:slot-definition-type slot)
                    do
                    (progn
                      (generate-ts-interface slot-type)
                      (format s "  ~(~A~): ~A;~%" slot-name (cl-type-to-ts slot-type))))
              (format s "}~%"))))))

;; Register basic type mappings
(register-type-mapping :string "string")
(register-type-mapping :number "number")
(register-type-mapping :boolean "boolean")
(register-type-mapping 'string "string")
(register-type-mapping 'number "number")
(register-type-mapping 'boolean "boolean")
(register-type-mapping 'integer "number")
(register-type-mapping 'float "number")
