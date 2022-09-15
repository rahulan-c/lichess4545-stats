# -*- coding: utf-8 -*-
"""
GET FENS FROM PGN

Obtain data on each ply (half-move) played in one or more chess games from a 
source PGN file. Returns a data frame with all FENs and preceding moves (in UCI 
format).
    
Last updated: 2022-06-30
"""

import chess
import chess.pgn
from tqdm import tqdm

def GetFENs(pgn_path):
    
    all_fens = []
    offsets = []
    game_ids = []
    
    pgn = open(pgn_path)
    
    while True:
        offset = pgn.tell()
        headers = chess.pgn.read_headers(pgn)
        
        if headers is None:
            break
        else:
            offsets.append(offset)
            
    for offset in tqdm(offsets):
        
        game_fens = []
        pgn.seek(offset)
        game = chess.pgn.read_game(pgn)
        game_ids.append(game.headers['Site'][-8:])
        board = game.board()
        
        for move in game.mainline_moves():
            
            board.push(move)
            game_fens.append(board.epd())
            
        all_fens.append(game_fens)
    
    return([all_fens, game_ids])
        
