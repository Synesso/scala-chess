<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ page import="au.com.loftinspace.scalachess.db.Store" %>
<%@ page import="au.com.loftinspace.scalachess.game.*" %>
<%
    Game game = Store.loadGame(1).move(request.getParameter("from"), request.getParameter("to"));
    Store.saveGame(1, game);
%>
<%= game.movesAsJson() %>

