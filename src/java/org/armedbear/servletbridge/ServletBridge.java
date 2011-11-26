/*
 * ServletBridge.java - basic Java servlet to ABCL Common Lisp servlet bridge
 *
 * Copyright (C) Alex Mizrahi, 2005
 * Copyright (C) Erik Huelsmann, 2010
 *
 *
 *
 *
 */
package org.armedbear.servletbridge;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.logging.Logger;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.UnavailableException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.armedbear.lisp.Function;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.Load;
import org.armedbear.lisp.Packages;
import org.armedbear.lisp.SpecialBindingsMark;
import org.armedbear.lisp.Symbol;

public class ServletBridge extends HttpServlet {
  static Logger log = Logger.getLogger(ServletBridge.class.getCanonicalName());

    private boolean disallowSwankDebugger = false;
    private Symbol serviceSymbol;
    private LispObject debuggerHook;
    private LispObject throwingDebuggerHook;
    // Specials to bind when calling into Lisp
    Symbol contextSym;
    Symbol configSym;
    // Values to bind to the specials above
    JavaObject jContext;
    JavaObject jConfig;

    /* The init() method uses the web.xml init-param values to
     * set up a lisp environment to which it can forward its requests.
     *
     * These init-param/parameter names are supported:
     *   lisp.loader: the (fasl or lisp) file containing loader code
     *   lisp.package: the package name (string) containing the SERVICE function
     *          to forward the requests to
     *   lisp.function: overrides the default (SERVICE) name of the service function
     *   lisp.disallow.swank.debugger: When defined, prevents swank debugger from
     *          being selected, even if the swank package is available
     *
     * In addition, the function sets up a Lisp package which serves as
     * the API of the lisp side to the bridge.
     */
    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);

        synchronized (ServletBridge.class) {

            disallowSwankDebugger =
                    null != config.getInitParameter("lisp.disallow.swank.debugger");

            try {
                Interpreter.createInstance();
                synchronized (Interpreter.getInstance()) {
                    org.armedbear.lisp.Package bridgePackage = Packages.findPackage("SERVLET-BRIDGE");
                    if (bridgePackage == null) {
                        bridgePackage = Packages.createPackage("SERVLET-BRIDGE");
                    }

                    contextSym = bridgePackage.internAndExport("*CONTEXT*");
                    configSym = bridgePackage.internAndExport("*CONFIG*");
                }
            } catch (Throwable t) {
                throw new UnavailableException("Unable to instantiate Lisp");
            }

            jContext = new JavaObject(config.getServletContext());
            jConfig = new JavaObject(config);

            // Set debugger hook with whatever is available.
            // This gets overridden below.
            setDebuggerHook();

            LispThread thread = LispThread.currentThread();
            SpecialBindingsMark mark = thread.markSpecialBindings();
            thread.bindSpecial(Symbol.DEBUGGER_HOOK, debuggerHook);
            thread.bindSpecial(contextSym, jContext);
            thread.bindSpecial(configSym, jConfig);

            String servicePackageName = config.getInitParameter("lisp.package");
            if (servicePackageName == null || servicePackageName.length() == 0) {
                throw new UnavailableException("Initialization error: "
                        + "no valid 'lisp.package' parameter specified");
            }

            org.armedbear.lisp.Package servicePackage =
                    Packages.findPackage(servicePackageName);
            String loaderPath = config.getInitParameter("lisp.loader");
            if (loaderPath == null || loaderPath.length() == 0) {
                // can't load the loaderPath: it's invalid
                // however, this is a valid state if the servicePackage already exists
                if (servicePackage == null) {
                    throw new UnavailableException("Initialization error: "
                            + "no valid 'lisp.loader' parameter specified");
                }
            } else {
                if (loaderPath.charAt(0) != '/') // not an absolute path? Resolve relative to servlet context path
                {
                    loaderPath = config.getServletContext().getRealPath(loaderPath);
                }

                try { 
                      final String servletBridgeURI = ServletBridge.class.getResource("servlet-api").toString();
                      try {
                        Load.load(servletBridgeURI);
                      } catch (Throwable t) {
                        log.severe(MessageFormat.format("Failed to load SERVLET-API from {0}", servletBridgeURI));
                      }
                    Load.load(loaderPath);
                } catch (Interpreter.UnhandledCondition e) {
                    throw new UnavailableException("Initialization error from lisp code:" + lispConditionToString(e));
                } finally {
                    thread.resetSpecialBindings(mark);
                }
            }

            // The setup process might have changed our options
            // for the selection of a debugger hook. Set it again now that we're fully
            // operational.
            setDebuggerHook();

            servicePackage = Packages.findPackage(servicePackageName);
            if (servicePackage == null) {
                throw new UnavailableException("Initialization error: "
                        + "no package called '" + servicePackageName + "' available");
            }

            String serviceName = config.getInitParameter("lisp.function");
            if (serviceName == null) {
                serviceName = "SERVICE";
            }
            serviceSymbol = servicePackage.findAccessibleSymbol(serviceName);
            if (serviceSymbol == null) {
                throw new UnavailableException("Initialization error: "
                        + "Package '" + servicePackageName + "' doesn't contain "
                        + "a '" + serviceName + "' symbol");
            }

            LispObject maybeFunction = serviceSymbol.getSymbolFunction();
            if (maybeFunction == null) {
                throw new UnavailableException("Symbol '" + servicePackageName
                        + "::" + serviceName + "' is not fbound");
            }
            if (!(maybeFunction instanceof Function)) {
                throw new UnavailableException("Symbol '" + servicePackageName
                        + "::" + serviceName + "' is fbound, but the function slot "
                        + "isn't of java-type Function");
            }
        }
    }

    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws IOException, ServletException {
        LispThread thread = LispThread.currentThread();
        SpecialBindingsMark mark = thread.markSpecialBindings();
        thread.bindSpecial(Symbol.DEBUGGER_HOOK, debuggerHook);
        thread.bindSpecial(contextSym, jContext);
        thread.bindSpecial(configSym, jConfig);
        try {
            serviceSymbol.execute(new JavaObject(request), new JavaObject(response));
        } finally {
            thread.resetSpecialBindings(mark);
        }
    }

    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws IOException, ServletException {
        LispThread thread = LispThread.currentThread();
        SpecialBindingsMark mark = thread.markSpecialBindings();
        thread.bindSpecial(Symbol.DEBUGGER_HOOK, debuggerHook);
        thread.bindSpecial(contextSym, jContext);
        thread.bindSpecial(configSym, jConfig);
        try {
            serviceSymbol.execute(new JavaObject(request), new JavaObject(response));
        } finally {
            thread.resetSpecialBindings(mark);
        }
    }

    private void setDebuggerHook() throws ServletException {
        Symbol hookSym;

        // get throwing debugger hook first
        hookSym = Lisp.PACKAGE_SYS.findAccessibleSymbol("%DEBUGGER-HOOK-FUNCTION");
        if (hookSym == null) {
            throw new UnavailableException("Default hook symbol SYS::%DEBUGGER-HOOK-FUNCTION not found");
        }
        throwingDebuggerHook = hookSym.getSymbolFunction();

        // now check if we have SWANK and if we want to use SWANK's debugger
        org.armedbear.lisp.Package swank = Packages.findPackage("SWANK");
        if (swank != null && !disallowSwankDebugger) {
            hookSym = swank.findAccessibleSymbol("SWANK-DEBUGGER-HOOK");
            if (hookSym == null) {
                throw new UnavailableException("SWANK loaded, but symbol SWANK-DEBUGGER-HOOK not found.");
            }

            debuggerHook = hookSym.getSymbolFunction();

            log("using swank debugger hook");
        } else {
            debuggerHook = throwingDebuggerHook;
            log("using throwing debugger hook");
        }

        if (debuggerHook == null) {
            throw new UnavailableException("No debugger hook function found to catch Lisp errors.");
        }
    }

        private String lispConditionToString(Interpreter.UnhandledCondition e) {
        LispObject c = e.getCondition();
        String retval = "unable to print lisp condition";
        LispThread thread = LispThread.currentThread();
        SpecialBindingsMark mark = thread.markSpecialBindings();
        try {
            thread.bindSpecial(Symbol.DEBUGGER_HOOK, throwingDebuggerHook);
            retval = c.printObject();
            thread.bindSpecial(Symbol.PRINT_ESCAPE, Lisp.NIL);
            retval = retval + " " + c.printObject();
        } catch (Interpreter.UnhandledCondition x) {
            //nothing -- retval should be appropriate at this point
        } finally {
            thread.resetSpecialBindings(mark);
        }

        return retval;
    }
}
