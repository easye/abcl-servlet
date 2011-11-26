(cl:in-package :cl-user)

(print "lisp dbgr is starting..")
(terpri)

(defvar *swank-loaded* nil)
(defvar *swank-server-started* nil)

(print "loading swank..")
(terpri)

#+nil
(load (merge-pathnames #p"packages/slime/swank-loader.lisp" *load-pathname*))
(require 'asdf)
(push #p"~/work/slime/" asdf:*central-registry*)
(require 'swank)

(print "...")(terpri)

(swank-loader::init
 :delete nil         ; delete any existing SWANK packages
 :reload nil         ; reload SWANK, even if the SWANK package already exists
 :load-contribs nil) ; load all contribs

(print "starting swank server..")
(terpri)

;uncomment for remote debugging
;(setf swank::*loopback-interface* "0.0.0.0")

(unless *swank-server-started*
	(swank:create-server :port 4005 :dont-close t)
	(setq *swank-server-started* t))


(defpackage "DBGRSRV"
    (:use :CL))

(in-package :DBGRSRV)

(defun service (request response)
    (servlet-api:set-content-type response "text/html")
    (let ((s (servlet-api:get-text-output-stream response)))
    (format s "<html><body><h1>Debbuger was loaded.</h1>
<p>Now uou can M-x slime-connect 127.0.0.1 4005 to start debugging session.</p>
<p>After SLIME is connected, you can start loading application servlet by
<a href='whatever'>visting any application page<a> -- this ensures that you can
debug the process of application loading.</p>")))
