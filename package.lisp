;;;; package.lisp

(defpackage #:clrpc.types
  (:use #:cl #:alexandria)
  (:export #:cl-type-to-ts
           #:register-type-mapping))

(defpackage #:clrpc.core
  (:use #:cl #:alexandria)
  (:export #:define-procedure
           #:*procedures*
           #:generate-ts-types))

(defpackage #:clrpc.server
  (:use #:cl #:alexandria #:clrpc.core)
  (:export #:start-server
           #:stop-server))

(defpackage #:clrpc
  (:use #:cl)
  (:import-from #:clrpc.core
                #:define-procedure
                #:*procedures*)
  (:import-from #:clrpc.server
                #:start-server
                #:stop-server)
  (:export #:define-procedure
           #:start-server
           #:stop-server))
