package org.abcl.servlet;

import java.io.IOException;
import java.io.PrintWriter;
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
      out.println("<html>");
      out.println("<head>");
      out.println("<title>Servlet Lisp</title>");  
      out.println("</head>");
      out.println("<body>");
      out.println("<h1>Servlet Lisp at " + request.getContextPath () + "</h1>");
      out.println("</body>");
      out.println("</html>");
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
