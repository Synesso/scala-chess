package au.com.loftinspace.scalachess

import org.specs.Specification
import org.specs.mock.Mockito

object snippetSpec extends Specification with Mockito {

    import _root_.com.google.appengine.api.users._
    val userService = mock[UserService]
    val request = mock[javax.servlet.http.HttpServletRequest]
    val snippet = new LoginSnippet(userService)

    "LoginSnippet with an unknown user" should {
        "Provide a login prompt" in {
/*
            userService.getCurrentUser returns null
            request.getRequestURI returns "requestURI"
            userService.createLoginURL("requestURI") returns "loginURL"
            val xhtml = snippet render request
            xhtml must ==/(<p>Hello! <a href="loginURL">Sign in</a> just for kicks.</p>)
*/
        }
    }

    "LoginSnippet with a known user" should {
        "Provide a logout prompt" in {
/*
            val user = new User("x", "y")
            userService.getCurrentUser returns user
            request.getRequestURI returns "requestURI"
            userService.createLogoutURL("requestURI") returns "logoutURL"
            val xhtml = snippet render request
            xhtml must ==/(<p>Hello, {"x"}! Now you can <a href="logoutURL">sign out</a> again.</p>)
*/
        }
    }
}