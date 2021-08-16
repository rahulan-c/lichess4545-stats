# -*- coding: utf-8 -*-
"""
Make openings sunburst plots for Lichess4545 seasons
Source: https://github.com/Destaq/chess_graph
"""

# import chess_graph

def make_sunburst(pgn):
  
  # import chess_graph
  
  # Make HTML sunburst
  chess_graph.graph(pgn, depth=16, shade = False, 
                    fragmentation_percentage=0.004, 
                    should_defragment = True, custom_branching=False, 
                    should_download = True, download_format = 'html', 
                    download_name = 'sunburst')

  # Make PNG sunburst - to show in final report, linked to the (separate) HTML version                  
  chess_graph.graph(pgn, depth=16, shade = False, 
                    fragmentation_percentage=0.004, 
                    should_defragment = True, custom_branching=False, 
                    should_download = True, download_format = 'png', 
                    download_name = 'sunburst')
