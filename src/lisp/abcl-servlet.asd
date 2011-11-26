;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
(require 'asdf)
(in-package :asdf)

(defsystem 
    :abcl-servlet :version "0.1" :components 
    ((:module api
              :pathname "org/armedbear/servletbridget/" :components
              ((:file "servlet-api")))
     (:module src :pathname "" :depends-on (:api) :components 
              ((:file "loader") (:file "dbgrstart")))))


    