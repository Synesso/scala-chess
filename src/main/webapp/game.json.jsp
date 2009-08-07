<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ page import="au.com.loftinspace.scalachess.db.Store" %>
<%= Store.loadGame(1).toJson() %>