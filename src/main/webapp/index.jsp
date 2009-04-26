<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ page import="au.com.loftinspace.scalachess.LoginSnippet" %>

<html>
    <head>
        <title>Just tooling around for now ...</title>
    </head>
    <body>
        <h1>Just a test</h1>
        <%= new LoginSnippet().render(request) %>
    </body>
</html>
