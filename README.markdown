# ABCL-SERVLET

A jar that exports a REST API for managing a persistent ABCL instance
inside a Java Servlet container.

## OBTAINING

    http://bitbucket.org/easye/abcl-servlet
    http://slack.net/~evenson/abcl/abcl-servlet
    Mark <evenson.not.org@gmail.com>
    Created: 22-NOV-2011
    Revised: 04-MAR-2014

##  INSTALL

### abcl-1.3.1

We require abcl-1.3.1 or later.

### Configuration

    The path designating Lisp code to run at server startup can be
    specified as web context initialization parameters of the form
    "abcl.servlet.load.0", "abcl.servlet.load.1", etc. in the
    <file:web/WEB-INF/web.xml> as follows:

        <init-param>
            <param-name>abcl.servlet.load.0</param-name>
            <param-value>/lisp/swank-servlet.lisp</param-value>
        </init-param>

    See org.abcl.lisp.servlet.Lisp.init() for how these parameters are
    interpreted.

## API


/get/*
  Implementation of resource servlet (currently unused), which
  returns things inside a valid resource so we can work in servlets
  which do not serve contents from the local filesystem making the
  ServletContext.getRealPath() methods fail.

/post/*
  Currently all requests shunt to the the SIMPLE-SERVLET::SERVICE symbol in
  <file:/src/lisp/simple.lisp>.

##  SLIME

### Connecting to container via Swank
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

 


