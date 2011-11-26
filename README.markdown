# ABCL-SERVLET

A jar that exports a REST API for managing a persistent ABCL instance
inside a Java Servlet container.

## OBTAINING

    Mark <evenson.not.org@gmail.com>
    Created: 22-NOV-2011
    Revised: 26-NOV-2011

##  INSTALL

Built via the Ant instructions in 'file:build.xml':

    cmd$ ant -find build.xml dist

##  SLIME

One specifies the SLIME installation to be pacakged by the
implementation 

To start the Swank server listening for incoming connections, the only
defined REST call is currently
  
    GET http://tomcat.local:8084/abcl/abcl-swank
    
which should respond with an HTML message containing markup with how
to rendevous with the SLIME port, usually via invoking the function
SLIME-CONNECT in Emacs lisp (interactively by the "M-x" prefix).


## BUGS

Due to the immature state of testing, this code will probably not work
at first in your local servelet container.  But if you do manage to
patch it, please get the patches back to the main repository so others
can share from your experience.

## CAVEAT

I am a little shocked that this actually works under Apache Tomcat, so
maybe it is not useful anywhere else.

As a seasoned Java developer, long conditioned to believe that threads
in a servlet container were never to be initiated, and would have
fairly restricted security capacities, I was surprised to find that I
could get ABCL threads to read/write FASLs off of the local
filesystem.  THen, being able to connect to the JVM hosting via SLIME


    


