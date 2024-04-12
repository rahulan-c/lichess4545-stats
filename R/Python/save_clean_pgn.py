# -*- coding: utf-8 -*-
"""Save a PGN without eval or clock tags."""

import chess.pgn

def save_clean_pgn(read_path, write_path, new_name):
  """Saves PGN without comments or variations."""
  pgn = open(read_path)
  new_pgn = open(f"{new_name}.pgn", "w", encoding="utf-8")
  exporter = chess.pgn.FileExporter(new_pgn, comments=False, variations=False)
  
  while (game := chess.pgn.read_game(pgn)):
    game.accept(exporter)
    
  # print("Saved clean PGN (no evals or clock times)")

def save_evals_pgn(read_path, write_path, new_name):
  """Saves PGN with comments (evals, clock times)."""
  pgn = open(read_path)
  new_pgn = open(f"{new_name}.pgn", "w", encoding="utf-8")
  exporter = chess.pgn.FileExporter(new_pgn, comments=True, variations=False)
  
  while (game := chess.pgn.read_game(pgn)):
    game.accept(exporter)
    
  # print("Saved PGN with evals")
