package au.com.loftinspace.scalachess

import _root_.com.google.appengine.api.users._
import game.Board
class LoginSnippet(getUserService: UserService) {
  def this() = this {
    UserServiceFactory.getUserService
  }

  def render(request: _root_.javax.servlet.http.HttpServletRequest) = {
    val userService = getUserService
    val user = userService.getCurrentUser
    user match {
      case null => <html><head><title>Just tooling around for now...</title></head>
              <body><h1>Just a test</h1><p>Hello!<a href={userService.createLoginURL(request.getRequestURI())}>Sign in</a>just for kicks.</p> </body></html>
      case _ => <html><head>
    <title>scratch</title>
    <link rel="stylesheet" type="text/css" media="screen" href="style.css"/>
    <script type="text/javascript" src="mootools-1.2.3-core.js"></script>
    <script type="text/javascript" src="mootools-1.2.3.1-more.js"></script>
    <script type="text/javascript" src="demo.js"></script>
</head>
<body>
<a href={userService.createLogoutURL(request.getRequestURI)}>sign out</a>
<div id="board">
    <div class="row">
        <div class="dark square" id="a8"></div>
        <div class="light square" id="b8"></div>
        <div class="dark square" id="c8"></div>
        <div class="light square" id="d8"></div>
        <div class="dark square" id="e8"></div>
        <div class="light square" id="f8"></div>
        <div class="dark square" id="g8"></div>
        <div class="light square" id="h8"></div>
        <div class="end"/>
    </div>
    <div class="row">
        <div class="light square" id="a7"></div>
        <div class="dark square" id="b7"></div>
        <div class="light square" id="c7"></div>
        <div class="dark square" id="d7"></div>
        <div class="light square" id="e7"></div>
        <div class="dark square" id="f7"></div>
        <div class="light square" id="g7"></div>
        <div class="dark square" id="h7"></div>
        <div class="end"/>
    </div>
    <div class="row">
        <div class="dark square" id="a6"></div>
        <div class="light square" id="b6"></div>
        <div class="dark square" id="c6"></div>
        <div class="light square" id="d6"></div>
        <div class="dark square" id="e6"></div>
        <div class="light square" id="f6"></div>
        <div class="dark square" id="g6"></div>
        <div class="light square" id="h6"></div>
        <div class="end"/>
    </div>
    <div class="row">
        <div class="light square" id="a5"></div>
        <div class="dark square" id="b5"></div>
        <div class="light square" id="c5"></div>
        <div class="dark square" id="d5"></div>
        <div class="light square" id="e5"></div>
        <div class="dark square" id="f5"></div>
        <div class="light square" id="g5"></div>
        <div class="dark square" id="h5"></div>
        <div class="end"/>
    </div>
    <div class="row">
        <div class="dark square" id="a4"></div>
        <div class="light square" id="b4"></div>
        <div class="dark square" id="c4"></div>
        <div class="light square" id="d4"></div>
        <div class="dark square" id="e4"></div>
        <div class="light square" id="f4"></div>
        <div class="dark square" id="g4"></div>
        <div class="light square" id="h4"></div>
        <div class="end"/>
    </div>
    <div class="row">
        <div class="light square" id="a3"></div>
        <div class="dark square" id="b3"></div>
        <div class="light square" id="c3"></div>
        <div class="dark square" id="d3"></div>
        <div class="light square" id="e3"></div>
        <div class="dark square" id="f3"></div>
        <div class="light square" id="g3"></div>
        <div class="dark square" id="h3"></div>
        <div class="end"/>
    </div>
    <div class="row">
        <div class="dark square" id="a2"></div>
        <div class="light square" id="b2"></div>
        <div class="dark square" id="c2"></div>
        <div class="light square" id="d2"></div>
        <div class="dark square" id="e2"></div>
        <div class="light square" id="f2"></div>
        <div class="dark square" id="g2"></div>
        <div class="light square" id="h2"></div>
        <div class="end"/>
    </div>
    <div class="row">
        <div class="light square" id="a1"></div>
        <div class="dark square" id="b1"></div>
        <div class="light square" id="c1"></div>
        <div class="dark square" id="d1"></div>
        <div class="light square" id="e1"></div>
        <div class="dark square" id="f1"></div>
        <div class="light square" id="g1"></div>
        <div class="dark square" id="h1"></div>
        <div class="end"/>
    </div>
</div>

<div class="clear"></div>
<div id="pieces">
    <div class="piece" id="black_knight_1"><img src="img/img_fantasy_shad/bn.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_knight_2"><img src="img/img_fantasy_shad/bn.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_queen_1"><img src="img/img_fantasy_shad/bq.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_king_1"><img src="img/img_fantasy_shad/bk.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_bishop_1"><img src="img/img_fantasy_shad/bb.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_bishop_2"><img src="img/img_fantasy_shad/bb.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_rook_1"><img src="img/img_fantasy_shad/br.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_rook_2"><img src="img/img_fantasy_shad/br.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_pawn_1"><img src="img/img_fantasy_shad/bp.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_pawn_2"><img src="img/img_fantasy_shad/bp.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_pawn_3"><img src="img/img_fantasy_shad/bp.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_pawn_4"><img src="img/img_fantasy_shad/bp.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_pawn_5"><img src="img/img_fantasy_shad/bp.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_pawn_6"><img src="img/img_fantasy_shad/bp.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_pawn_7"><img src="img/img_fantasy_shad/bp.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="black_pawn_8"><img src="img/img_fantasy_shad/bp.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="white_knight_1"><img src="img/img_fantasy_shad/wn.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_knight_2"><img src="img/img_fantasy_shad/wn.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_queen_1"><img src="img/img_fantasy_shad/wq.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_king_1"><img src="img/img_fantasy_shad/wk.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_bishop_1"><img src="img/img_fantasy_shad/wb.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_bishop_2"><img src="img/img_fantasy_shad/wb.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_pawn_1"><img src="img/img_fantasy_shad/wp.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_pawn_2"><img src="img/img_fantasy_shad/wp.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_pawn_3"><img src="img/img_fantasy_shad/wp.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_pawn_4"><img src="img/img_fantasy_shad/wp.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_pawn_5"><img src="img/img_fantasy_shad/wp.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_pawn_6"><img src="img/img_fantasy_shad/wp.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_pawn_7"><img src="img/img_fantasy_shad/wp.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_pawn_8"><img src="img/img_fantasy_shad/wp.png" alt="white pawn" height="60"/></div>
    <div class="piece" id="white_rook_1"><img src="img/img_fantasy_shad/wr.png" alt="black pawn" height="60"/></div>
    <div class="piece" id="white_rook_2"><img src="img/img_fantasy_shad/wr.png" alt="black pawn" height="60"/></div>
</div>
</body>
</html>

    /*
<p>Hello, {user.getNickname}! Now you can <a href={userService.createLogoutURL(request.getRequestURI)}>sign out</a> again.</p>
       <table><tr><td><pre>New board is created with {new Board().reset.pieces.size} pieces</pre></td></tr></table>
    */
    }
  }
}
