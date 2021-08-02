# -*- coding: utf-8 -*-
"""
Show pawn and knight mate positions
"""

import chess
import chess.pgn
import chess.svg
import io
import berserk


def save_svg(svg, filepath):
    """
    Save svg content in filepath

    :param str  svg:        SVG content
    :param str  filepath:   Path of the SVG file to save
    :return:
    """
    try:
        file_handle = open(filepath, 'w')
    except IOError as e:
        print(str(e))
        exit(1)

    file_handle.write(svg)
    file_handle.close() 

# Now try it for a Lichess game
# We know that a knight gave checkmate in https://lichess.org/v3U4K2uM#81
# Let's show that!

session = berserk.TokenSession('JTBHpWFidFFcahpb') # my API token
client = berserk.Client(session)

# Test game IDs
# v3U4K2uM
# 1MWLzMll
# 89l6KEm5

# Using advice in https://svgutils.readthedocs.io/en/latest/tutorials/composing_multipanel_figures.html

interesting_mates = {'v3U4K2uM', '1MWLzMll', '89l6KEm5'}
for m in interesting_mates:
    pgn = client.games.export(m, as_pgn=True)
    with io.StringIO(pgn) as f:
      game = chess.pgn.read_game(f)
    board = game.board()
    for move in game.mainline_moves():
      board.push(move)
      ucimove = move.uci()
    # Show final position as SVG
    final_pos = chess.svg.board(board, size = 250, lastmove = chess.Move.from_uci(ucimove), coordinates = False, flipped = False)
    save_svg(final_pos, "C:/Users/rahul/Documents/Github/rahulan-c.github.io/lichess4545-stats/reports/images/mates/{}.svg".format(m))


# Combine SVGs into single image for report
path_1 = "C:/Users/rahul/Documents/Github/rahulan-c.github.io/lichess4545-stats/reports/images/mates/v3U4K2uM.svg"
path_2 = "C:/Users/rahul/Documents/Github/rahulan-c.github.io/lichess4545-stats/reports/images/mates/1MWLzMll.svg"
save_path = "C:/Users/rahul/Documents/Github/rahulan-c.github.io/lichess4545-stats/reports/images/mates/combined.svg"

from svgutils.compose import *

Figure("500", "500",
       SVG(path_1).scale(0.5),
       SVG(path_2).scale(0.5)
       ).tile(2, 1).save(save_path)