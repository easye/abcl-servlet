package org.abcl.servlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.MessageFormat;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


public class Lisp extends HttpServlet {
  protected void processRequest(HttpServletRequest request, 
                                HttpServletResponse response)
    throws ServletException, IOException 
  {
    response.setContentType("text/html;charset=UTF-8");
    PrintWriter out = response.getWriter();
    getServletContext().setAttribute("org.abcl.servlet.request",
                                     request);
    try {
        final String CRLF = "\n\r";
	Object results[] = {
	    request.getServletPath()};
        String messageBody // XXX expensive, but much less verbose.
	    = MessageFormat.format("<html>" + "<head>" +"<title>Servlet Lisp</title>" + "</head>" +  "<body>" 
				   + "<h1>"
				   + "Servlet Lisp at ServletPath: '{0}'\n\r"
				   + "</h1>" + "</body>" + "</html>", results);
    out.println(messageBody);
    } finally {      
      out.close();
    }
  }

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
          throws ServletException, IOException 
  {
    processRequest(request, response);
  }

  protected void doPost(HttpServletRequest request, HttpServletResponse response)
          throws ServletException, IOException {
    processRequest(request, response);
  }

  public String getServletInfo() {
    return "Short description";
  }
}
