# -*- coding: utf-8 -*-
"""
Save FENs for all moves in a set of games
"""

import chess
import chess.pgn

def GetFENs(pgn_path):
    
    allfens = []
    offsets = []
    pgn = open(pgn_path)
    
    while True:
        offset = pgn.tell()
        headers = chess.pgn.read_headers(pgn)
        if headers is None:
            break
        else:
            offsets.append(offset)
            
    for offset in offsets:
        gamefens = []
        pgn.seek(offset)
        game = chess.pgn.read_game(pgn)
        board = game.board()
        for move in game.mainline_moves():
            board.push(move)
            gamefens.append(board.epd())
        allfens.append(gamefens)
    
    return(allfens)
        
