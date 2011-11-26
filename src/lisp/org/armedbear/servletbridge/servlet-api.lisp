(cl:defpackage #:servlet-api
    (:nicknames #:servlet)
    (:use :cl #:java)
  (:export 
   ;; ServletResponse
   #:flush-buffer
   #:get-buffer-size
   #:get-binary-output-stream
   #:get-text-output-stream
   #:set-content-type
   #:set-content-length
   #:set-status
   #:set-header
   #:is-commited
   #:reset-request
   #:set-buffer-size
   #:set-locale

   ;; Request & Response
   #:get-character-encoding

   ;; ServletRequest

   #:forward-request
   #:get-content-length
   #:get-content-type
   #:get-dispatcher-type
   #:get-binary-input-stream
   #:get-text-input-stream
   #:get-parameter
   #:get-parameter-values-list
   #:get-parameter-names
   #:get-protocol
   #:get-remote-addr
   #:get-remote-host
   #:get-scheme
   #:get-server-name
   #:get-server-port
   #:include-request
   #:is-secure
   #:get-locale
   #:get-locales))
   
   
(cl:in-package :servlet-api)

(require "JAVA")

;;
;;  javax.servlet.ServletResponse
;;

(defmacro def-resp-fn (name params java-name)
  `(defun ,name (response ,@params)
    (jcall ,java-name response ,@params)))

(def-resp-fn flush-buffer () "flushBuffer")
(def-resp-fn get-buffer-size () "getBufferSize")

(defun get-binary-output-stream (response)
  (let ((java-output-stream (java:jcall (jmethod "getOutputStream" "javax.servlet.ServletResponse")  response)))
    (java:jnew (jconstructor "org.armedbear.lisp.Stream"
			     "org.armedbear.lisp.Symbol" "java.io.OutputStream")
	       'sys::system-stream java-output-stream)))

(defun get-text-output-stream (response)
  (let ((java-writer (java:jcall "getWriter" response)))
    (java:jnew (jconstructor "org.armedbear.lisp.Stream"
			     "org.armedbear.lisp.Symbol" "java.io.Writer") 
	       'sys::system-stream java-writer)))

(def-resp-fn set-content-type (content-type) "setContentType")
(def-resp-fn set-content-length (content-length) "setContentLength")

(defun set-status (response code &optional message)
  (if message
      (java:jcall "setStatus" response code message)
    (java:jcall "setStatus" response code)))

(def-resp-fn set-header (name value) "setHeader")
(def-resp-fn is-commited () "isCommited")
(def-resp-fn reset-request () "reset")
(def-resp-fn set-buffer-size () "setBufferSize")

(def-resp-fn %set-locale (locale) "setLocale")
(defun set-locale (request language &optional (country "") (variant ""))
  (let ((jlocale (jnew (jconstructor "java.util.Locale"
                                     #1="java.lang.String" #1# #1#)
                        language country variant)))
    (%set-locale request jlocale)))

;;
;; javax.servlet.ServletRequest
;;

(defmacro def-rq-fn (name params java-name)
  `(defun ,name (request ,@params) 
    (jcall ,java-name request ,@params)))

;;not implemented: getAttribute
;;not implemented: getAttributeNames
(def-rq-fn get-content-length () "getContentLength")
(def-rq-fn get-content-type () "getContentType")

(def-rq-fn %get-dispatcher-type () "getDispatcherType")
(defun get-dispatcher-type (request)
  (let ((dispatcher-type (%get-dispatcher-type request)))
    (cond
      ((jequal dispatcher-type (load-time-value (jfield "javax.servlet.DispatcherType" "REQUEST")))
       :REQUEST)
      ((jequal dispatcher-type (load-time-value (jfield "javax.servlet.DispatcherType" "INCLUDE")))
       :INCLUDE)
      ((jequal dispatcher-type (load-time-value (jfield "javax.servlet.DispatcherType" "FORWARD")))
       :FORWARD)
      ((jequal dispatcher-type (load-time-value (jfield "javax.servlet.DispatcherType" "ERROR")))
       :ERROR)
      ((jequal dispatcher-type (load-time-value (jfield "javax.servlet.DispatcherType" "ASYNC")))
       :ASYNC))))


(defun get-binary-input-stream (request)
  (let ((java-input-stream (jcall (jmethod "getInputStream" "javax.servlet.ServletRequest") request)))
    (jnew (jconstructor "org.armedbear.lisp.Stream"
			"org.armedbear.lisp.Symbol" "java.io.InputStream")
	  'sys::system-stream java-input-stream)))

(def-rq-fn get-parameter (name) "getParameter")
(def-rq-fn %get-parameter-values (name) "getParameterValues")
(defun get-parameter-values-list (request name)
    (java::list-from-jarray (%get-parameter-values request name)))

(def-rq-fn %get-parameter-names () "getParameterNames")
(defun get-parameter-names (request)
    (java::list-from-jenumeration (%get-parameter-names request)))

(def-rq-fn get-protocol () "getProtocol")

(defun get-text-input-stream (request)
  (let ((java-input-stream (jcall "getReader" request)))
    (jnew (jconstructor "org.armedbear.lisp.Stream"
			"org.armedbear.lisp.Symbol" "java.io.Reader")
	  'sys::system-stream java-input-stream)))

(def-rq-fn get-remote-addr () "getRemoteAddr")
(def-rq-fn get-remote-host () "getRemoteHost")


(define-condition dispatcher-not-available (error) ())

(def-rq-fn %get-request-dispatcher (path) "getRequestDispatcher")
(defun forward-request (request response path)
   (let ((dispatcher (%get-request-dispatcher request path)))
      (cond
        ((jnull-ref-p dispatcher)
         (cerror 'dispatcher-not-available))
        (t
         (jcall dispatcher "forward" request response)))))

(defun include-request (request response path)
   (let ((dispatcher (%get-request-dispatcher request path)))
      (cond
        ((jnull-ref-p dispatcher)
         (cerror 'dispatcher-not-available))
        (t
         (jcall dispatcher "include" request response)))))


(def-rq-fn get-scheme () "getScheme")
(def-rq-fn get-server-name () "getServerName")
(def-rq-fn get-server-port () "getServerPort")
(def-rq-fn is-secure () "isSecure")
;;not implemented: remove-attribute
;;not implemented: set-attribute

(defun %convert-locale (jlocale)
  (let ((lang (jcall jlocale (jmethod "java.util.Locale" "getLanguage")))
        (country (jcall jlocale (jmethod "java.util.Locale" "getCountry")))
        (variant (jcall jlocale (jmethod "java.util.Locale" "getVariant"))))
    (list (when (string/= "" lang) lang)
          (when (string/= "" country) country)
          (when (string/= "" variant) variant))))

(def-rq-fn %get-locale () "getLocale")
(defun get-locale (request)
  (%convert-locale (%get-locale request)))

(def-rq-fn %get-locales () "getLocales")
(defun get-locales (request)
  (mapcar #'%convert-locale
          (java::list-from-jenumeration (%get-locales request))))

;; common functions

(defun get-character-encoding (request-or-response)
  (jcall "getCharacterEncoding" request-or-response))

