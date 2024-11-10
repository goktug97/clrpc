;;;; clrpc.asd

(asdf:defsystem #:clrpc
  :description "Type-safe RPC between Common Lisp and TypeScript"
  :author "Goktug <karakasligk@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:jonathan
               #:hunchentoot
               #:parenscript
               #:closer-mop
               #:babel)
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "types")
                 (:file "core")
                 (:file "server")))))

