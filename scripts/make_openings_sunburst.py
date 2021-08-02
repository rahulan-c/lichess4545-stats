# -*- coding: utf-8 -*-
"""
MAKE OPENINGS SUNBURST PLOT
"""

# !pip install --upgrade plotly
# !pip install --upgrade pgnparser
# !pip install --upgrade chess_graph

# import plotly.io as pio

# pio.renderers.default='browser'

import chess_graph

# Define path to season games PGN
pgn_path = "C:/Users/rahul/Documents/Github/rahulan-c.github.io/lichess4545-stats/data/games.pgn"

# Produce openings sunburst plot
chess_graph.graph(pgn_path, depth=16, shade = False, 
                  fragmentation_percentage=0.004, 
                  should_defragment = True, custom_branching=False, 
                  should_download = True, download_format = 'html', 
                  download_name = 'sunburst')

# Re-do just to save snapshot PNG (for stats report)                  
chess_graph.graph(pgn_path, depth=16, shade = False, 
                  fragmentation_percentage=0.004, 
                  should_defragment = True, custom_branching=False, 
                  should_download = True, download_format = 'png', 
                  download_name = 'sunburst')

# print("Produced openings sunburst plot!")
