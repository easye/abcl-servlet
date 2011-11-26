(defun compile.lisp ()
  (let* ((the-project (jcall "getProject" self))
         (src-iterator (jcall (jmethod "org.apache.tools.ant.types.Path" "iterator")
                              (jcall "getReference" the-project "abcl-servlet.lisp")))
	 (abcl-servlet (truename "~/work/abcl-servlet/"))
         (source 
	  (merge-pathnames "src/lisp/**/*.lisp" abcl-servlet))
         (destination 
          (merge-pathnames "build/web/WEB-INF/classes/**/*.abcl" abcl-servlet)))
    (when (and (jcall "hasNext" src-iterator))
      (loop
         :for src-path = (pathname (jcall "toString" (jcall "next" src-iterator)))

         :for dst-path = (translate-pathname src-path source destination)
         :do 
            (format t "~&Compiling ~A to ~A.~%" src-path dst-path)
         :do 
            (compile-file src-path :output-file dst-path)
         :while 
            (and (jcall "hasNext" src-iterator))))))

(eval-when (:execute)
;;  #+nil
  (require 'asdf)
  (push #p"~/work/slime/" asdf:*central-registry*)
  (asdf:load-system 'swank)
  (load "~/work/abcl-servlet/src/lisp/org/armedbear/servletbridge/servlet-api")
  (compile.lisp))
