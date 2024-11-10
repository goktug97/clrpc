(in-package #:clrpc.server)

(defvar *server* nil)

(defun parse-request-body (raw-body)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (key . value) in (jonathan:parse 
                               (babel:octets-to-string raw-body)
                               :as :alist)
          do (setf (gethash key table) value))
    table))

(defun convert-to-clos (value type)
  "Convert JSON hash-table VALUE to a CLOS object of TYPE if TYPE is a CLOS class."
  (cond
    ((and (consp value) (find-class type nil))
     (let* ((hash-table (make-hash-table :test 'equal))
            (instance (make-instance type)))
       (loop for (k . v) in value
             do (setf (gethash (string-downcase (string k)) hash-table) v))
       (loop for slot in (closer-mop:class-slots (find-class type))
             for slot-name = (closer-mop:slot-definition-name slot)
             for slot-type = (closer-mop:slot-definition-type slot)
             for json-value = (gethash (string-downcase (string slot-name)) hash-table)
             when json-value
             do (setf (slot-value instance slot-name)
                      (if (find-class slot-type nil)
                          (convert-to-clos json-value slot-type)
                          json-value)))
       instance))
    (t value)))

(defun handle-procedure-call (procedure-name body)
  (let ((procedure (gethash procedure-name *procedures*)))
    (if procedure
        (let* ((args (getf procedure :args))
               (fn (getf procedure :fn))
               (parsed-body (parse-request-body body))
               (arg-values (mapcar (lambda (arg)
                                   (let* ((arg-name (first arg))
                                          (arg-type (second arg))
                                          (json-value (gethash arg-name parsed-body)))
                                     (convert-to-clos json-value arg-type)))
                                 args)))
          (apply fn arg-values))
        (error "Procedure not found: ~A" procedure-name))))

(defun start-server (&key (port 3000))
  (when *server*
    (stop-server))
    
  (setf hunchentoot:*catch-errors-p* nil)
    
  (setf *server*
        (make-instance 'hunchentoot:easy-acceptor :port port))
        
  (push 
   (hunchentoot:create-prefix-dispatcher 
    "/query/" 
    (lambda ()
      (setf (hunchentoot:content-type*) "application/json")
      (handler-case
          (let* ((uri (hunchentoot:script-name*))
                 (procedure-name (subseq uri (length "/query/"))))
            (jonathan:to-json
             (handle-procedure-call 
              procedure-name
              (hunchentoot:raw-post-data))))
        (error (e)
          (setf (hunchentoot:return-code*) 500)
          (jonathan:to-json 
           (list :|error| (format nil "~A" e)))))))
   hunchentoot:*dispatch-table*)
   
  (hunchentoot:start *server*)
  *server*)

(defun stop-server ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)))
