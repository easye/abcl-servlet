package org.abcl.servlet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class Resource extends HttpServlet {

  protected void processRequest(HttpServletRequest request, HttpServletResponse response)
          throws ServletException, IOException {
    response.setContentType("text/html;charset=UTF-8");
    PrintWriter out = response.getWriter();
    try {
      out.println("<!DOCTYPE html>");
      out.println("<html>");
      out.println("<head>");
      out.println("<title>Servlet Resource</title>");
      out.println("</head>");
      out.println("<body>");
      out.println("<h1>Servlet Resource at " + request.getContextPath() + "</h1>");
      out.println("</body>");
      out.println("</html>");
    } finally {
      out.close();
    }
  }

  protected void doGet(HttpServletRequest request, HttpServletResponse response)
          throws ServletException, IOException {
    String pathInfo = request.getPathInfo();
    if (pathInfo == null) {
      processRequest(request, response);
      return;
    }
    if (pathInfo.endsWith("/")) {
      outputIndex(pathInfo, response);
    }
    outputResource(pathInfo, response);
  }

  void outputResource(String path, HttpServletResponse r) throws IOException {
    ServletContext context = getServletContext();
    //  TODO cache me!
    InputStream stream = context.getResourceAsStream(path);
    
    if (stream == null) {
        r.setContentType("text/plain");
        r.getWriter().printf("Failed to find resource stream for '%1$s'.\n", path);
        return;
    }
    BufferedReader in
        = new BufferedReader(new InputStreamReader(stream));
    r.setContentType("text/plain");
    PrintWriter out = r.getWriter();
    String line = null;
    while (in.ready()) {
      line = in.readLine();
      out.printf("%1$s\n", line);
    }
    in.close();
  }

  void outputIndex(String path, HttpServletResponse r) throws IOException {
    ServletContext context = getServletContext();
    Set<String> paths = context.getResourcePaths(path);

    r.setContentType("text/plain");
    PrintWriter o;
    try {
      o = r.getWriter();
    } catch (IOException ex) {
      Logger.getLogger(Resource.class.getName()).log(Level.SEVERE, null, ex);
      throw ex;
    }
    if (paths == null) {
      o.printf("No paths found for '%1$s'\n", path);
    } else {
      for (String p : paths) {
        o.printf("%1$s\n", p);
      }
    }
  }

  @Override
  protected void doPost(HttpServletRequest request, HttpServletResponse response)
          throws ServletException, IOException {
    processRequest(request, response);
  }

  public String getServletInfo() {
    return "Return indexes and resources.";
  }

}
