# -*- coding: utf-8 -*-
"""
Obtain data on each ply (half-move) played in one or more chess games from a 
source PGN file. 
Returns a data frame with variables: 
    (1) FEN
    (2) Preceding move in UCI notation
"""

import chess
import chess.pgn

def GetFENs(pgn_path):
    
    all_fens = []
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
        
        game_fens = []
        pgn.seek(offset)
        game = chess.pgn.read_game(pgn)
        board = game.board()
        
        for move in game.mainline_moves():
            
            board.push(move)
            game_fens.append(board.epd())
            
        all_fens.append(game_fens)
    
    return(all_fens)
        
