package au.com.loftinspace.scalachess

import _root_.com.google.appengine.api.users._

class LoginSnippet(getUserService: UserService) {

    def this() = this {
        UserServiceFactory.getUserService
    }

    def render(request: _root_.javax.servlet.http.HttpServletRequest) = {
        val userService = getUserService
        val user = userService.getCurrentUser
        user match {
            case null => <p>Hello! <a href={userService.createLoginURL(request.getRequestURI())}>Sign in</a> just for kicks.</p>
            case _ => <p>Hello, {user.getNickname}! Now you can <a href={userService.createLogoutURL(request.getRequestURI)}>sign out</a> again.</p>
        }
    }
}
