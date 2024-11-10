(in-package #:clrpc.server)

(defvar *server* nil)

(defun parse-request-body (raw-body)
  (jonathan:parse raw-body))

(defun handle-procedure-call (procedure-name body)
  (let ((procedure (gethash procedure-name *procedures*)))
    (if procedure
        (let* ((args (getf procedure :args))
               (fn (getf procedure :fn))
               (parsed-body (parse-request-body body))
               (arg-values (mapcar (lambda (arg)
                                   (gethash (string-downcase 
                                            (symbol-name (car arg)))
                                           parsed-body))
                                 args)))
          (apply fn arg-values))
        (error "Procedure not found"))))

(defun start-server (&key (port 3000))
  (when *server*
    (stop-server))
  (setf *server*
        (hunchentoot:start 
         (make-instance 'hunchentoot:easy-acceptor :port port)))
  
  (hunchentoot:define-easy-handler 
      (handle-query :uri "/query/:procedure") (procedure)
    (setf (hunchentoot:content-type*) "application/json")
    (handler-case
        (jonathan:to-json
         (handle-procedure-call procedure
                               (hunchentoot:raw-post-data :force-text t)))
      (error (e)
        (setf (hunchentoot:return-code*) 500)
        (jonathan:to-json 
         (list :|error| (format nil "~A" e))))))
  *server*)

(defun stop-server ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)))
