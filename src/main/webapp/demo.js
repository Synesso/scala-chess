
var darkSquares = ['h1','h3','h5','h7','g2','g4','g6','g8','f1','f3','f5','f7','e2','e4','e6','e8','d1','d3','d5','d7','c2','c4','c6','c8','b1','b3','b5','b7','a2','a4','a6','a8'];


window.addEvent('domready', function() {

    var piecesPerSquare = new Hash();
    var squareClasses = new Hash();
    var viableMoves = new Hash();


    document.ondragstart = function () {
        return false;
    }; //IE drag hack

    //for every draggable image...
/*
    $$('.piece').each(function(drag) {

        //make it draggable, and set the destination divs
        new Drag.Move(drag, {
            droppables: '.square',
            onDrop: function(el, droppable) {
                if ($chk(droppable) && droppable.get('rel') != 'filled') {
                    //inject into current parent
                    el.inject(droppable).addClass('locked');
                    el.setStyles({'left':0,'top':0,'position':'relative','margin':0});
                    droppable.set('rel', 'filled');
                    this.detach();
                }
            },
            onEnter: function(el, droppable) {
                //colors!
                droppable.addClass('mo');
            },
            onLeave: function(el, droppable) {
                droppable.removeClass('mo');
            }
        });
        drag.setStyles({ 'top':0, 'left':0, 'margin-right':'20px' });
    });
*/

    $('board').position();

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
            to.each(function(dest, i, ary) {toElements = toElements.include($(dest))});

            new Drag.Move($(piecesPerSquare.get(from)), {
                droppables: toElements,
                precalculate: true,
                onDrop: function(el, droppable) {
                    console.log('dropping ' + el.get('id') + ' on ' + droppable.get('id'));
                    el.position({relativeTo: droppable});
                },
                onComplete: function(el) {
                    console.log('completed - todo: action next turn');
                }
            });

            $(piecesPerSquare.get(from)).addEvent('mousedown', function(square) {
                console.log('mousedown ' + from)
                to.each(function(dest, i, ary) {
                    var morph = new Fx.Morph(dest, {duration: 400, transition: Fx.Transitions.Cubic.easeOut});
                    morph.start('.highlight');
                });
            });
            $(piecesPerSquare.get(from)).addEvent('mouseup', function(square) {
                console.log('mouseup ' + from)
                to.each(function(dest, i, ary) {
                    var morph = new Fx.Morph(dest, {duration: 400, transition: Fx.Transitions.Cubic.easeOut});
                    morph.start(darkSquares.contains(dest) ? '.dark' : '.light');
                });
            });

            $(piecesPerSquare.get(from)).addClass('movable');
        });

    }}).get({'game': 1});

    // todo - store moves hash & store currently highlighted squares.
    // todo - unlight old and relight new (where different) on new click
    // todo - click on the piece, not the square.
});
