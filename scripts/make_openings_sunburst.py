# -*- coding: utf-8 -*-
"""
MAKE OPENINGS SUNBURST PLOT
"""

# !pip install --upgrade plotly
# !pip install --upgrade pgnparser
# !pip install --upgrade chess_graph

import plotly.io as pio

pio.renderers.default='browser'

import chess_graph

# Define path to season games PGN
pgn_path = "C:/Users/rahul/Documents/Github/rahulan-c.github.io/lichess4545-stats/data/games.pgn"

# Produce openings sunburst plot
chess_graph.graph(pgn_path, depth=6, shade = False, 
                  # color = 'black', name = 'izzie26',
                  fragmentation_percentage=0.002, 
                  should_defragment = True, custom_branching=False, 
                  should_download = True, download_format = 'html', 
                  download_name = 'sunburst')

# Save sunburst HTML file
# fig.write_html("C:/Users/rahul/Documents/Github/rahulan-c.github.io/lichess4545-stats/reports/sunburst_4545_s13.html")



# print("All done")