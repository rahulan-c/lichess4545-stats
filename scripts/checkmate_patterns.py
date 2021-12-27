# -*- coding: utf-8 -*-
"""
IDENTIFY CHECKMATE PATTERNS

Identify notable checkmate patterns in games

- Input:   PGN file
- Returns: list containing identified pattern ("Arabian"), game ID, and checkmate ply (integer)

Used for compiling images of notable checkmates reached in league games

Last updated: 2021-11-20
"""

from typing import List, Optional, Tuple
import chess
from chess import square_rank, Color, Board, Square, Piece, square_distance, WHITE, BLACK, SquareSet
from chess import KING, QUEEN, ROOK, BISHOP, KNIGHT, PAWN
from chess.pgn import ChildNode
from typing import Type, TypeVar

from chess import Outcome
from chess.pgn import Game

from tqdm import tqdm
from chess import square_rank, square_file, Board, SquareSet, Piece, PieceType, square_distance
from chess import KING, QUEEN, ROOK, BISHOP, KNIGHT, PAWN
from chess import WHITE, BLACK, Outcome, Termination
from chess.pgn import ChildNode

import chess.pgn
import chess.svg
import io
import glob
import os


# Back rank mate
def back_rank_mate(fen: str) -> bool:
    
    board = chess.Board(fen)
    pov = not board.turn
    king = board.king(not pov)

    assert king is not None

    back_rank = 7 if pov else 0

    if board.is_checkmate() and square_rank(king) == back_rank:
        squares = SquareSet.from_square(king + (-8 if pov else 8))
        if pov:
            if chess.square_file(king) < 7:
                squares.add(king - 7)
            if chess.square_file(king) > 0:
                squares.add(king - 9)
        else:
            if chess.square_file(king) < 7:
                squares.add(king + 9)
            if chess.square_file(king) > 0:
                squares.add(king + 7)
        for square in squares:
            piece = board.piece_at(square)
            if piece is None or piece.color == pov or board.attackers(pov, square):
                return False
        return any(square_rank(checker) == back_rank for checker in board.checkers())
    return False


# Hook mate
def hook_mate(fen: str) -> bool:
    board = chess.Board(fen)
    pov = not board.turn
    king = board.king(not pov)
    if board.is_checkmate():
        assert king is not None
        mate_square = list(board.checkers())[0]
        square = chess.Square(mate_square)
        if board.piece_at(square).symbol() in ['R', 'r'] and square_distance(square, king) == 1:
            for rook_defender_square in board.attackers(pov, square):
                defender = board.piece_at(rook_defender_square)
                if defender and defender.piece_type == KNIGHT and square_distance(rook_defender_square, king) == 1:
                    for knight_defender_square in board.attackers(pov, rook_defender_square):
                        pawn = board.piece_at(knight_defender_square)
                        if pawn and pawn.piece_type == PAWN:
                            return True
        return False
    return False

# Anastasia's mate
# Note: typically 
def anastasia_mate(fen: str) -> bool:
    board = chess.Board(fen)
    pov = not board.turn
    king = board.king(not pov)
    assert king is not None
    if board.is_checkmate():
        if square_file(king) in [0, 7] and square_rank(king) not in [0, 7]:
            mate_square = list(board.checkers())[0]
            square = chess.Square(mate_square)
            if square_file(square) == square_file(king) and board.piece_at(square).symbol() in ['Q', 'q', 'R', 'r']:
                if square_file(king) != 0:
                    board.apply_transform(chess.flip_horizontal)
                king = board.king(not pov)
                assert king is not None
                blocker = board.piece_at(king + 1)
                if blocker is not None and blocker.color != pov:
                    knight = board.piece_at(king + 3)
                    if knight is not None and knight.color == pov and knight.piece_type == KNIGHT:
                        return True
        return False
    return False

# Arabian mate
def arabian_mate(fen: str) -> bool:
    board = chess.Board(fen)
    pov = not board.turn
    king = board.king(not pov)
    assert king is not None
    if board.is_checkmate():
        mate_square = list(board.checkers())[0]
        square = chess.Square(mate_square)
        if square_file(king) in [0, 7] and square_rank(king) in [0, 7] and board.piece_at(square).symbol() in ['R', 'r'] and square_distance(square, king) == 1:
            for knight_square in board.attackers(pov, square):
                knight = board.piece_at(knight_square)
                if knight and knight.piece_type == KNIGHT and (
                    abs(square_rank(knight_square) - square_rank(king)) == 2 and
                    abs(square_file(knight_square) - square_file(king)) == 2
                ):
                    return True
        return False
    return False

# Smothered mate
def smothered_mate(fen: str) -> bool:
    board = chess.Board(fen)
    pov = not board.turn
    king = board.king(not pov)
    assert king is not None
    for checker_square in board.checkers():
        piece = board.piece_at(checker_square)
        assert piece
        if piece.piece_type == KNIGHT:
            for escape_square in [s for s in chess.SQUARES if square_distance(s, king) == 1]:
                blocker = board.piece_at(escape_square)
                if not blocker or blocker.color == pov:
                    return False
            return True
    return False


def IdentifyCheckmatePatterns(pgn_file):
    
    # Read PGN
    pgn = open(pgn_file)
    offsets = []
    mate_patterns = []
    
    while True:
        
        # Scan PGN headers for faster parsing
        offset = pgn.tell()
        headers = chess.pgn.read_headers(pgn)
        
        if headers is None:
            break
        else:
            offsets.append(offset)
    
    total_games = len(offsets)
    
    print('#####################################')
    print("  IDENTIFY INTERESTING CHECKMATES  ")
    print('#####################################')
    print('')
    print(f"Reading {pgn_file} ({len(offsets)} games)")
    print('')
    
    
    # Loop through each selected game
    for offset in tqdm(offsets):
    
        pgn.seek(offset)
        game = chess.pgn.read_game(pgn)
    
        # Show all checkmates
        final = game.end().board()
        outcome = final.outcome()
        if outcome == None or not outcome.termination == Termination(1):
            continue
    
        # Get final position
        position = final.fen()
    
        # Back-rank mate
        if back_rank_mate(position):
            mate_patterns.append(('back_rank',
                         game.headers['Site'][-8:],
                         game.end().ply()
                ))
    
        # Anastasia's mate
        if anastasia_mate(position):
            mate_patterns.append(('anastasia',
                         game.headers['Site'][-8:],
                         game.end().ply()
                ))
        
        # Hook mate
        if hook_mate(position):
            mate_patterns.append(('hook',
                         game.headers['Site'][-8:],
                         game.end().ply()
                ))
    
        # Arabian mate
        if arabian_mate(position):
            mate_patterns.append(('arabian',
                         game.headers['Site'][-8:],
                         game.end().ply()
                ))
    
        # Smothered mates
        if smothered_mate(position):
            mate_patterns.append(('smothered',
                         game.headers['Site'][-8:],
                         game.end().ply()
                ))
    
    print('mate patterns checked')
    
    return(mate_patterns)
