#|
 A very rough stab at preparing ASDF depedencies for inclusion in a war artifact.

 ABCL-SERVLET/BUILD:PREPARE will attempt to use rsync to package all
 depedencies of an ASDF system.

 Under Windows, one must be running from a cygwin shell with rsync installed.
|#

#|
  The following Ant target will automate this process, as long as 

    <path id="abcl.runtime">
      <fileset dir="${abcl.dir}/dist">
        <include name="abcl.jar"/>
        <include name="abcl-contrib.jar"/>
      </fileset>

    <target name="prepare.asdf-dependencies">
      <property name="system" value=":abcl-servlet"/>  <!-- set this to the system you wish to package -->
      <java fork="true" dir="${basedir}"
            classpathref="abcl.runtime"
            classname="org.armedbear.lisp.Main">
        <!-- Run in 64bit mode-->
        <jvmarg value="-d64"/> 

        <!-- Enable JVM assertions -->
        <jvmarg value="-ea"/>  
        
        <arg value="--noinit"/> 
        <arg value="--eval"/><arg value="(require (quote asdf))"/>
        <arg value="--eval"/>
          <arg value="(asdf:initialize-source-registry `(:source-registry (:directory ,*default-pathname-defaults*) :inherit-configuration)))"/>
        <arg value="--eval"/><arg value="(asdf:load-system :abcl-servlet/build)"/>
        <arg value="--eval"/><arg value="(abcl-servlet/build:prepare ${system})"/>
        <arg value="--eval"/><arg value="(ext:exit)"/>
      </java>
    </target>
|#

(defpackage :abcl-servlet/build
  (:use :cl :jss)
  (:export
   #:prepare))

;;  Not working under Ant 
(require :abcl-contrib)
(require :quicklisp-abcl)
;; so hack it this way
;;(load "~/quicklisp/setup")

(in-package :abcl-servlet/build)

(defun destination-root (system)
  "Return the destination root for SYSTEM.

Currently this computes the pathname \"build/web/asdf/\" relative to
the location of the system asdf definition.

"
  (merge-pathnames #p"build/web/asdf/"
                   (asdf/system:system-source-directory (asdf:find-system system))))

(defun normalize-pathname (p)
  (if (not (find :windows *features*))
      p
      (let ((s (make-string-output-stream)))
        (uiop/run-program:run-program 
         (format nil "cygpath.exe --unix ~A" p)
         :output s)
        (string-trim '(#\Space #\Newline #\Return) (get-output-stream-string s)))))
  
(defun copy-system (system destination)
  (let* ((root (normalize-pathname 
                (make-pathname :defaults (slot-value (asdf:find-system system) 'asdf/component:source-file)
                               :name nil
                               :type nil)))
         (dest (normalize-pathname
                (merge-pathnames 
                 (concatenate 'string (first (last (pathname-directory root))) "/")
                 destination)))
         (rsync-command (if (find :windows *features*)
                            "rsync.exe"
                            "rsync"))
         (command (format nil "~A -avzP --exclude=.hg --exclude=ext --exclude=dist --exclude=var --exclude=web --exclude=build ~A ~A" rsync-command root dest)))
    (ensure-directories-exist dest)
    (format t "~&Copying '~A' to '~A'.~&" root dest) 
    (uiop/run-program:run-program command :output t)))

(defun prepare (system &optional ignored-systems)
  "Prepare all systems necessary for running SYSTEM for inclusion in a war artifact."
    (loop :for dependency :in (asdf-jar::dependent-systems system)
       :unless (find dependency ignored-systems)
       :do (copy-system dependency (destination-root system)))
    ;; always package SLIME as well (could be optional)
    (copy-system :swank (destination-root system))) 
