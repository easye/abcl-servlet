(defun ensure-relative (source abcl-servlet)
  (let ((d (pathname-directory source)))
    (if (eq (first d) :absolute)
        ;;; /**/src/**/*.lisp -
        (make-pathname 
          :directory (cons :relative
                           (subseq d (position "src" d :test #'equal)))
          :defaults source)
        d)))
   
(defparameter *self* nil)

(defun compile.lisp ()
  (let* ((the-project (jcall "getProject" *self*))
         (src-iterator (jcall (jmethod "org.apache.tools.ant.types.Path" "iterator")
                              (jcall "getReference" the-project "abcl-servlet.lisp")))
	 (abcl-servlet (truename "~/work/abcl-servlet/"))
         (source 
          "src/lisp/**/*.lisp")
         (destination 
          "build/web/WEB-INF/classes/**/*.abcl"))
    (when (and (jcall "hasNext" src-iterator))
      (loop
         :for src-path 
           = (jcall "toString" (jcall "next" src-iterator))
         :for dst-path 
            = (translate-pathname 
               (ensure-relative src-path abcl-servlet)
               source destination)
         :do 
            (format t "~&Compiling ~A to ~A.~%" src-path dst-path)
         :do 
            (compile-file src-path :output-file dst-path)
         :while 
            (and (jcall "hasNext" src-iterator))))))

(eval-when (:execute)
  ;;; We're being executed in the context of an Ant build
  (setf *self* self)
  (trace ensure-relative translate-pathname)
;;  #+nil
  (require 'asdf)
  (push #p"~/work/slime/" asdf:*central-registry*)
  (asdf:load-system 'swank)
  (load "~/work/abcl-servlet/src/lisp/org/armedbear/servletbridge/servlet-api")
  (compile.lisp))
