;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
(require 'asdf)
(require 'abcl-contrib)
(require 'abcl-asdf)
(in-package :asdf)

;;; XXX remove the need to bootstrap the javax.servlet definitions
#+nil
(add-to-classpath (abcl-asdf:resolve-dependencies "javax.servlet" "servlet-api" "3.0"))

(defsystem 
    :abcl-servlet :version "0.2" :components 
    ((:module javax.servlet.jar :components
              ((:mvn "javax.servlet/servlet-api" :version "3.0")))
     (:module api :depends-on (:javax.servlet.jar)
              :pathname "org/armedbear/servletbridge/" :components
              ((:file "servlet-api")))
     (:module src :pathname "" :depends-on (:api) :components 
              ((:file "loader") (:file "dbgrstart")))))


    