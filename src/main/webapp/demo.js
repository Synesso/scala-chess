var darkSquares = ['h1','h3','h5','h7','g2','g4','g6','g8','f1','f3','f5','f7','e2','e4','e6','e8','d1','d3','d5','d7','c2','c4','c6','c8','b1','b3','b5','b7','a2','a4','a6','a8'];
window.addEvent('domready', function() {
    var piecesPerSquare = new Hash();
    var squareClasses = new Hash();
    var viableMoves = new Hash();

    $('board').position();

    document.ondragstart = function () {
        return false;
    }; //IE drag hack

    new Request.JSON({url: "board.json.jsp", onSuccess: function(board) {
        var pieceCount = {};
        for (var square in board.pieces) {
            var piece = board.pieces[square];
            pieceCount[piece] = ($chk(pieceCount[piece])) ? pieceCount[piece] + 1 : 1;
            var pieceId = piece + '_' + pieceCount[piece];
            $(pieceId).position({relativeTo: square});
            $(pieceId).fade('in'); // todo - why doesn't this work?
            piecesPerSquare.set(square, pieceId);
        }
    }}).get({'game': 1});
    new Request.JSON({url: "moves.json.jsp", onSuccess: function(movesRawHash) {
        viableMoves = new Hash(movesRawHash);
        viableMoves.each(function(to, from, hash) {
            console.log('adding events to ' + from)
            var toElements = []
            to.each(function(dest, i, ary) {
                toElements = toElements.include($(dest))
            });
            new Drag.Move($(piecesPerSquare.get(from)), {
                droppables: toElements,
                container: $('board'),
                precalculate: true,
                snap: 0,
                onDrop: function(el, droppable) {
                    if (!droppable) {
                        var mover = new Fx.Move(el, {relativeTo: from});
                        mover.chain(function() {el.setStyle('z-index', 'auto');});
                        mover.start();
                    } else {
                        el.position({relativeTo: droppable});
                        console.log('completed - todo: action next turn'); // todo
                    };
                }
            });
            $(piecesPerSquare.get(from)).addEvent('mousedown', function(square) {
                to.each(function(dest, i, ary) {
                    $(piecesPerSquare.get(from)).setStyle('z-index', 1);
                    var morph = new Fx.Morph(dest, {duration: 400, transition: Fx.Transitions.Cubic.easeOut});
                    morph.start('.highlight');
                });
            });
            $(piecesPerSquare.get(from)).addEvent('mouseup', function(square) {
                to.each(function(dest, i, ary) {
                    var morph = new Fx.Morph(dest, {duration: 400, transition: Fx.Transitions.Cubic.easeOut});
                    morph.start(darkSquares.contains(dest) ? '.dark' : '.light');
                });
            });
            $(piecesPerSquare.get(from)).addClass('movable');
        });
    }}).get({'game': 1});
});
