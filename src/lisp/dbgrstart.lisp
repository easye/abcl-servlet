(cl:in-package :cl-user)

(defvar *swank-loaded* nil)
(defvar *swank-server-started* nil)
(defparameter *swank-port* 4005)

;;; XXX HACK
(require 'asdf)
(push #p"~/work/slime/" asdf:*central-registry*)
(require 'swank)

(defun load-swank ()
  (format t "~&Loading Swank...")
  #+nil
  (load (merge-pathnames #p"packages/slime/swank-loader.lisp" *load-pathname*))
  (swank-loader::init
   :delete nil         ; delete any existing SWANK packages
   :reload nil         ; reload SWANK, even if the SWANK package already exists
   :load-contribs nil) ; load all contribs
  (format t "~&Starting swank server...")
;;uncomment for remote debugging
;;(setf swank::*loopback-interface* "0.0.0.0")
  (unless *swank-server-started*
    (swank:create-server :port *swank-port* :dont-close t)
    (setq *swank-server-started* t)))

(defpackage "DBGRSRV" 
  (:use :cl)
  (:nicknames "abcl-servlet-repl-server"))
(in-package :DBGRSRV)



(defun service (request response)
  (servlet-api:set-content-type response "text/html")
  (let ((s (servlet-api:get-text-output-stream response))
        (server-name (servlet-api:get-server-name request))
        (request-url (java:jcall "toString" (servlet-api:get-request-url request))))
    (format s 
              "
<html><body><h1>Swank server started</h1> 

<p>request-url: ~A</p>

<p>Now you can M-x slime-connect ~A:~A to start debugging session.</p>

<p>After SLIME is connected, you can start loading an application
servlet by <a href='whatever'>visiting any application page<a> -- this
ensures that you can debug the process of application loading.</p>

</body></html>" 
              request-url
              server-name cl-user::*swank-port*)))

(cl:in-package :cl-user)
(eval-when (:execute)
  (load-swank))
