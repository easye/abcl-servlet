(cl:in-package :cl-user)

(defpackage "SRV" (:use :CL))
(in-package :SRV)

(require 'asdf)
(push #P"~/work/slime/" asdf:*central-registry*)
(asdf:load-system 'swank)

(defun service (request response)
 (let ((*debugger-hook* #'swank:swank-debugger-hook))
   (with-simple-restart (abort "Abort")
     (servlet-api:set-content-type response "text/html")
     (let ((s (servlet-api:get-text-output-stream response)))
   (format s "<html><body><h1>Hello</h1>world</body></html>")))))