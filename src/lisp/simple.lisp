(cl:in-package :cl-user)

(defpackage "SIMPLE-SERVLET" (:use :CL))
(in-package :simple-servlet)

(defun service (request response)
  (declare (ignore request))
  (with-simple-restart (abort "Abort")
    (servlet-api:set-content-type response "text/html")
    (let ((s (servlet-api:get-text-output-stream response))
          (context-path (servlet-api:get-context-path request)))
      (format s "<html>
<title>ABCL</title>
<body>
<p>Hello, world!</p>
<p>context-path: ~A</p>
</body>
</html>"
               context-path))))
