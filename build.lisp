;;; XXX would be nice...  
#| 
(require 'abcl-contrib)
(require 'jss)

But currently produces the error under Netbeans Ant

    The value #<org.apache.tools.ant.loader.AntClassLoader5
    AntClassLoader[/home/evenson/wor.... {51CBEE}> is not of type LIST

which means that we need to MKLIST the ABCL-CONTRIB require mechanism code.

|#


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

(defun compile.lisp (build-xml-ref)
  (let* ((the-project 
          (jcall "getProject" *self*))
         (src-iterator 
          (jcall (jmethod "org.apache.tools.ant.types.Path" "iterator")
                 (jcall "getReference" the-project build-xml-ref)))
	 (abcl-servlet 
          (truename "~/work/abcl-servlet/"))
         (source 
          "src/lisp/**/*.lisp")
         (destination 
          "build/web/WEB-INF/classes/**/*.abcl"))
    (when (and (jcall "hasNext" src-iterator))
      (loop
         :for src-path 
           = (jcall "toString" (jcall "next" src-iterator))
         :for dst-path 
            = (merge-pathnames (translate-pathname 
                                (ensure-relative src-path abcl-servlet)
                                source destination)
                               abcl-servlet)
         :do 
            (format t "~&Compiling ~A to ~A.~%" src-path dst-path)
         :do 
            (compile-file src-path :output-file dst-path)
         :while 
            (and (jcall "hasNext" src-iterator))))))

(eval-when (:execute)
  ;;; We're being executed in the context of an Ant build, so glom on
  ;;; to that context in a global.
  (setf *self* self)

  ;;; DEBUG
  (trace ensure-relative translate-pathname compile-file)

  (require 'asdf)
  (let ((slime-dir (jcall "getProperty" 
                          (jcall "getProject" *self*)
                          "slime.dir")))
    (push 
     (if slime-dir 
         (pathname (concatenate 'string slime-dir "/"))
         #p"~/work/slime/")
     asdf:*central-registry*))
  (asdf:load-system 'swank)
  (compile.lisp "abcl-servlet.servlet-api.lisp")
  (compile.lisp "abcl-servlet.lisp"))

