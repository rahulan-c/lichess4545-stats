# -*- coding: utf-8 -*-
"""
Identify games with gambits and for each game, the side that played the gambit

Filters all games in a PGN file to find games played in openings associated with
gambits (eg Evans, Ruy Marshall...), then checks the moves played by both sides in the game.
If one side gambits material for more than 5 consecutive plies before ply 30, the game is 
classified as a gambit and the side that sacrificed material is also recorded.

"""

from tqdm import tqdm
from chess import Color, Board
from chess import KING, QUEEN, ROOK, BISHOP, KNIGHT, PAWN
import chess.pgn
from chess.pgn import ChildNode
import pandas as pd

def IdentifyGambits(pgn_file, gambits_lookup_path):

    values = { PAWN: 1, KNIGHT: 3, BISHOP: 3, ROOK: 5, QUEEN: 9 }
    
    def material_count(board: Board, side: Color) -> int:
        return sum(len(board.pieces(piece_type, side)) * value for piece_type, value in values.items())
    
    def material_diff(board: Board, side: Color) -> int:
        return material_count(board, side) - material_count(board, not side)
    
    pgn = open(pgn_file)
    
    gambits = pd.read_csv(gambits_lookup_path)
    
    gambit_lines = gambits['name']
    gambit_set = set(gambit_lines)
    
    offsets = []
    
    while True:
        
        offset = pgn.tell()
        headers = chess.pgn.read_headers(pgn)
        
        if headers is None:
            break
        else:
            offsets.append(offset)
    
    total_games = len(offsets)
    
    gambit_games = []
    gambit_cols = []
    
    for offset in tqdm(offsets):
    
        pgn.seek(offset)
        game = chess.pgn.read_game(pgn)
        board = game.board()
    
        # Skip games with non-gambit opening names in PGN data
        if not game.headers['Opening'] in gambit_set:
            continue
    
        gambit_seq = []
    
        # Loop through each move played
        for node in game.mainline():
    
            # Stop after ply 30
            if node.ply() == 30:
                break
    
            if material_diff(board, board.turn) < 0:
                gambit_col = board.turn
            elif material_diff(board, board.turn) > 0:
                gambit_col = not board.turn
            elif material_diff(board, board.turn) == 0:
                gambit_col = None
    
            # Define a gambit as indicated by a nonzero material difference for 5 consecutive plies
            if gambit_col != None:
                gambit_seq.append(node.ply())
            else:
                gambit_seq = []
    
            if len(gambit_seq) >= 5:
                gambit_games.append(game.headers['Site'][-8:])
                gambit_cols.append('w' if gambit_col else 'b')
                break
    
            board.push(node.move) 
    
    df = pd.DataFrame(data = {'id': gambit_games, 'col': gambit_cols})
    
    return df







