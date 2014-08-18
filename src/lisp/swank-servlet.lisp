(cl:in-package :cl-user)

(defvar *swank-loaded* nil)
(defvar *swank-server-started* nil)
(defparameter *swank-port* 4005)

(require :asdf)

;; As long as the user's ASDF can find the swank.asd definition, this
;; should work.
(require :swank)

(defun load-swank ()
  (format t "~&Loading Swank...")
  (swank-loader::init
   :delete nil         ; delete any existing SWANK packages
   :reload nil         ; reload SWANK, even if the SWANK package already exists
   :load-contribs nil) ; load all contribs
  (format t "~&Starting swank server...")
  
  ;; enable remote debugging
  (setf swank::*loopback-interface* "0.0.0.0")
  (unless *swank-server-started*
    (swank:create-server :port *swank-port* :dont-close t)
    (setq *swank-server-started* t)))

(cl:in-package :cl-user)
(eval-when (:execute)
  (load-swank))
