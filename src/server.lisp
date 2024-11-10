(in-package #:clrpc.server)

(defvar *server* nil)

(defun parse-request-body (raw-body)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (key . value) in (jonathan:parse 
                               (babel:octets-to-string raw-body)
                               :as :alist)
          do (setf (gethash key table) value))
    table))

(defun handle-procedure-call (procedure-name body)
  (let ((procedure (gethash procedure-name *procedures*)))
    (if procedure
        (let* ((args (getf procedure :args))
               (fn (getf procedure :fn))
               (parsed-body (parse-request-body body))
               (arg-values (mapcar (lambda (arg)
                                   (let ((arg-name (first arg)))
                                     (gethash arg-name parsed-body)))
                                 args)))
          (apply fn arg-values))
        (error "Procedure not found: ~A" procedure-name))))

(defun start-server (&key (port 3000))
  (when *server*
    (stop-server))
    
  ;; For debugging
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
