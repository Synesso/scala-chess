var darkSquares = ['h1','h3','h5','h7','g2','g4','g6','g8','f1','f3','f5','f7','e2','e4','e6','e8','d1','d3','d5','d7','c2','c4','c6','c8','b1','b3','b5','b7','a2','a4','a6','a8'];
var log = function(msg) {
//    console.log(msg);
};
window.addEvent('domready', function() {
    var piecesPerSquare = new Hash();
    var dragEffects = new Hash();
    $('board').position();
    document.ondragstart = function () {
        return false;
    }; //IE drag hack

    // Place the pieces at their correct square on the board
    var place = function(pieces) {
        var pieceCount = {};
        for (var square in pieces) {
            var piece = pieces[square];
            pieceCount[piece] = ($chk(pieceCount[piece])) ? pieceCount[piece] + 1 : 1;
            var pieceId = piece + '_' + pieceCount[piece];
            log(square + "=" + pieceId);
            $(pieceId).position({relativeTo: square}); // todo - stop pieces from flipping around due to random order of map
            piecesPerSquare.set(square, pieceId);
            $(pieceId).fade('in'); // todo - why doesn't this work?
        }
    };
    // Remove the draggable effect from any pieces which are marked as draggable
    var freezeAll = function() {
        dragEffects.each(function(effect, from, dragHash) {
            var fromPiece = $(piecesPerSquare.get(from));
            log("removing events from " + piecesPerSquare.get(from) + " at " + from);
            effect.detach();
            fromPiece.removeEvents();
            fromPiece.removeClass('movable');
        });
        dragEffects.empty();
    };
    // Enable dragging and draggable effects on movable pieces
    var unfreeze = function(moves) {
        new Hash(moves).each(function(to, from, hash) {
            log(from + "=>" + to);
            var fromPiece = $(piecesPerSquare.get(from));
            fromPiece.addEvent('mousedown', function(square) {
                to.each(function(dest, i, ary) {
                    $(piecesPerSquare.get(from)).setStyle('z-index', 1);
                    var morph = new Fx.Morph(dest, {duration: 400, transition: Fx.Transitions.Cubic.easeOut});
                    morph.start('.highlight');
                });
            });
            fromPiece.addEvent('mouseup', function(square) {
                to.each(function(dest, i, ary) {
                    var morph = new Fx.Morph(dest, {duration: 400, transition: Fx.Transitions.Cubic.easeOut});
                    morph.start(darkSquares.contains(dest) ? '.dark' : '.light');
                });
            });
            fromPiece.addClass('movable');
            var toElements = [];
            to.each(function(dest, i, ary) {
                toElements = toElements.include($(dest))
            });
            var dragMove = new Drag.Move(fromPiece, {
                droppables: toElements,
                container: $('board'),
                precalculate: true,
                snap: 5,
                onDrop: function(el, droppable) {
                    if (!droppable) {
                        var mover = new Fx.Move(el, {relativeTo: from});
                        mover.chain(function() {
                            el.setStyle('z-index', '0');
                        });
                        mover.start();
                    } else {
                        el.position({relativeTo: droppable});
                        freezeAll();
                        log("Setting " + droppable.id + " -> " + piecesPerSquare.get(from));
                        piecesPerSquare.set(droppable.id, piecesPerSquare.get(from));
                        log("Removing " + from);
                        piecesPerSquare.erase(from);
                        submit(from, droppable.id);
                    }
                }
            });
            dragEffects.set(from, dragMove);
        });
    };
    // Transition the entire game board to the given state
    var render = function(game) {
        place(game.pieces);
        unfreeze(game.moves);
    };
    // Transition only the recently affected pieces to the given state
    var renderChanged = function(game) {
        if (game.last.taken == 'undefined') { // todo - how to check if a JSON element is present?
            console.log("a piece was taken! " + game.last.taken);
        } else {
            console.log("no piece was taken" + game.last.taken);
        }
        // todo - castling
        // todo - promotion
        // todo - seems king in check doesn't work yet?
        unfreeze(game.moves);
    };
    // Submit the move from->to
    var submit = function(from, to) {
        log("Submitting move " + from + " to " + to);
        new Request.JSON({url: "move.json.jsp", onSuccess: renderChanged}).get({
            'game': 1, 'from': from, 'to': to
        });
    };
    // Let's get this party started!
    new Request.JSON({url: "game.json.jsp", onSuccess: render}).get({'game': 1});
});
