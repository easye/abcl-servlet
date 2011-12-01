# ABCL-SERVLET

A jar that exports a REST API for managing a persistent ABCL instance
inside a Java Servlet container.

## OBTAINING

    http://bitbucket.org/easye/abcl-servlet
    http://slack.net/~evenson/abcl/abcl-servlet
    Mark <evenson.not.org@gmail.com>
    Created: 22-NOV-2011
    Revised: 01-DEC-2011

##  INSTALL

Built via the
[Ant instructions contained in 'file:build.xml'](build.xml).  First
copy `build.properties.in' to `build.properties', then edit to reflect
local variances.  To build the `abcl-servlet.war' issue: 

    cmd$ ant -find build.xml dist

##  SLIME

One specifies the SLIME installation available locally to be packaged
by the build process to create the delpoyment artifact
[abcl-servlet.war](dist/abcl-servlet.war) via the `slime.dir' property

To start the Swank server listening for incoming connections, the only
defined REST call is currently
  
    GET http://tomcat.local:8084/abcl-servlet/swank
    
where the "tomcat.local:8084" resolves to the authority and path of
the deployed 'abcl-servlet.war' artifact which should respond with an
HTML message containing markup with how to rendevous with the SLIME
port, usually via invoking the function SLIME-CONNECT in Emacs lisp
(interactively by the "M-x" prefix).


## BUGS

Due to the immature state of testing, this code will probably not work
at first in your local java servlet container.  But if you do manage
to patch it, please get the patches back to the main repository so
others can share from your experience.

## CAVEAT

I am a little shocked that this actually works under Apache Tomcat, so
maybe it is not useful anywhere else.  Opening the source to a common
trunk that encourages forking is the first step to distribute the
effort of testing the posibilites. I plan to at least test under
Glassfish, Weblogic, and JBoss to explore exactly where it might be
useful. 

As a seasoned Java developer who was long conditioned to believe that
threads in a servlet container were never to be initiated, and would
have fairly restricted security capacities, I was surprised to find
that I could get ABCL threads to read/write FASLs off of the local
filesystem.  Finding myself able to connect to the JVM hosting via
SLIME just gobsmacked me to exclaim "Everyone should be able to do
this!"

## LICENSE

As a mostly derived work of the work of Alex Mizrahi (aka
"killerstorm") and Erik Hülsmann, the code is licensed under the same
terms of abcl-web, namely those of LGPL.  These are the
[terms from the abcl-web website](http://abcl-web.sourceforge.net/#License)

    abcl-web is distributed under LGPL license, with clarifications
    from LLGPL applicable. (while components, such us ABCL itself, may
    come with different licenses). To clarify this: you can use this
    however you want with your web applications, but if you modify
    ABCL-web, we'd like to see your patches and apply them if they are
    useful in general.
    
## COLOPHON    

Remember to make Love, not War!
 
Mark Evenson

November 2011

 


