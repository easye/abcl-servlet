(cl:in-package :cl-user)

(defpackage "SRV" (:use :CL))
(in-package :SRV)

(require 'asdf)
(push #P"~/work/slime/" asdf:*central-registry*)
(asdf:load-system 'swank)

(defparameter *location* *load-pathname*)

(defun service (request response)
  (declare (ignore request))
  (let ((*debugger-hook* #'swank:swank-debugger-hook))
    (with-simple-restart (abort "Abort")
      (servlet-api:set-content-type response "text/html")
      (let ((s (servlet-api:get-text-output-stream response)))
        (format s "<html>
<title>ABCL</title>
<body>
<p>load-truename: ~A</p>
<p><a href='/abcl-servlet/swank'>Start the Swank server</a></p>
</body>
</html>"
               *location*)))))