package org.abcl.servlet;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.MessageFormat;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.UnavailableException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.Function;
import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.Load;
import org.armedbear.lisp.Packages;
import org.armedbear.lisp.SpecialBindingsMark;
import org.armedbear.lisp.Symbol;

public class Lisp
    extends HttpServlet 
{

  protected void diagnosticRequest(HttpServletRequest request,
          HttpServletResponse response)
          throws ServletException, IOException {
    response.setContentType("text/html;charset=UTF-8");
    PrintWriter out = response.getWriter();
    getServletContext().setAttribute("org.abcl.servlet.request",
            request);
    try {
      final String CRLF = "\n\r";
      Object results[] = {
        request.getServletPath(), getServletContext(), request
      };
      String messageBody
              = MessageFormat.format("<html>" + "<head>" + "<title>Servlet Lisp</title>" + "</head>" + "<body>"
                      + "<h1>"
                      + "Servlet Lisp at ServletPath: '{0}'\n\r"
                      + "</h1>" + "</body>" + "</html>", results);
      out.println(messageBody);
    } finally {
      out.close();
    }
  }

  Symbol contextSymbol;
  Symbol serviceSymbol;

  protected void processRequest(HttpServletRequest request,
                                HttpServletResponse response)
      throws ServletException, IOException 
    {
      LispThread thread = LispThread.currentThread();
      SpecialBindingsMark mark = thread.markSpecialBindings();
      ServletContext context = getServletContext();
      thread.bindSpecial(contextSymbol, new JavaObject(context));

      try {
          serviceSymbol.execute(new JavaObject(request), new JavaObject(response));
      } finally {
          thread.resetSpecialBindings(mark);
      }

  }

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
          throws ServletException, IOException {
    processRequest(request, response);
  }

  protected void doPost(HttpServletRequest request, HttpServletResponse response)
          throws ServletException, IOException {
    processRequest(request, response);
  }

  public String getServletInfo() {
    return "Short description";
  }

  /** Contains a static reference to the ServletContext this servlet initialized with.
      
      To assist Lisp code in bootstrapping we make a static reference
      to this context available here.
  */
  public static ServletContext servletContext;

  public void init(ServletConfig config)
      throws ServletException 
  {
    super.init(config);
    log("Starting Lisp initialization...");
    synchronized (Lisp.class) {
      try {
        Interpreter.createInstance();
        synchronized (Interpreter.getInstance()) {
          org.armedbear.lisp.Package bridgePackage = Packages.findPackage("SERVLET-BRIDGE");
          if (bridgePackage == null) {
            bridgePackage = Packages.createPackage("SERVLET-BRIDGE");
          }

          // Create a dynamic binding for the Java ServletContext at SERVLET-BRIDGE:*CONTEXT*
          contextSymbol = bridgePackage.internAndExport("*CONTEXT*");
          LispThread thread = LispThread.currentThread();
          servletContext = getServletContext();
          thread.bindSpecial(contextSymbol, new JavaObject(servletContext));
        }
        getServletContext().log("Succesfully started ABCL interpreter.");
      } catch (Throwable t) {
        throw new UnavailableException("Unable to instantiate Lisp");
      }
      loadLispResource("/lisp/servlet-api.lisp");

      int index = 0;

      while (true) {
        String path = servletContext.getInitParameter("abcl.servlet.load." + index);
        if (path == null) {
            break;
        }
        log(Message.format("Loading {0}.", path));
        loadLispResource(path);
        index += 1;
      }
      
      loadLispResource("/lisp/simple.lisp");

      String servicePackageName = "SIMPLE-SERVLET";
      org.armedbear.lisp.Package servicePackage 
        = Packages.findPackage(servicePackageName);
      if (servicePackageName == null || servicePackageName.length() == 0) {
        throw new UnavailableException("Initialization error: "
                                       + "invalid 'lisp.package' parameter specified");
      }
      
      // TODO Configuration!
      String serviceName = "SERVICE";
      serviceSymbol = servicePackage.findAccessibleSymbol(serviceName);
      
      if (serviceSymbol == null) {
        String msg 
          = MessageFormat.format("Initialization error: Package '{0}' doesn't contain a '{0}' symbol.", 
                                 servicePackageName, serviceName);
        throw new UnavailableException(msg);
      }

      LispObject maybeFunction = serviceSymbol.getSymbolFunction();
      if (maybeFunction == null) {
        String msg 
          = MessageFormat.format("The symbol '{0}::{0} is not bound to a function",
                                 servicePackageName, serviceName);
        throw new UnavailableException(msg);
      }
      if (!(maybeFunction instanceof Function)) {
        String msg 
          = MessageFormat.format("The symbol function slot of '{0}::{0}' isn't of java-type Function",
                                 servicePackageName, serviceName);
        throw new UnavailableException(msg);
      }
    
    }
    log("Initialization finished.");
  }

  void loadLispResource(String path) {
    try {
      InputStream input = getServletContext().getResourceAsStream(path);
      if (input == null) {
        String message 
          = MessageFormat.format("Unable to load Lisp resource from ''{0}''.", path);
        log(message);
       // throw new UnavailableException(message);
      }
      log(MessageFormat.format("Loading ''{0}''...", path));
      Load.load(input);
      input.close();
    } catch (IOException e) {
      log(MessageFormat.format("Proceeding despite IOException: {0}", e.toString()));
    }
  }
  
  public void destroy() {
    log("Servlet destruction initiated...");
    Interpreter.getInstance().dispose();
    log("Servlet destruction finished.");
  }

}
