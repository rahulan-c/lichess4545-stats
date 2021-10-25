
# Generate lookup datasets for opening names and gambit names
# Last updated: 2021-10-25

# Source helper functions
library(here)
source(paste0(here::here(), "/scripts/all_functions.R"))

# Read detailed opening lookup info
# Source: https://github.com/niklasf/chess-openings
a <- readr::read_tsv("https://raw.githubusercontent.com/niklasf/chess-openings/master/dist/a.tsv")
b <- readr::read_tsv("https://raw.githubusercontent.com/niklasf/chess-openings/master/dist/b.tsv")
c <- readr::read_tsv("https://raw.githubusercontent.com/niklasf/chess-openings/master/dist/c.tsv")
d <- readr::read_tsv("https://raw.githubusercontent.com/niklasf/chess-openings/master/dist/d.tsv")
e <- readr::read_tsv("https://raw.githubusercontent.com/niklasf/chess-openings/master/dist/e.tsv")

# Combine into one openings data tibble
openings_lookup <- rbindlist(list(a, b, c, d, e))
rm(a, b, c, d, e)

# Make a separate gambits subset
gambits_lookup <- openings_lookup %>% 
  filter(str_detect(name, paste(c("(?i)gambit", # include all gambits (obviously)
                                  "(?i)countergambit", # include all countergambits too
                                  
                                  # Include various gambit openings that don't have "gambit" in their name
                                  "Ruy Lopez: Marshall Attack",
                                  "Van Weersel Attack",
                                  "Italian Game: Two Knights Defense, Fried Liver Attack",
                                  "Italian Game: Two Knights Defense, Traxler Counterattack",
                                  "Italian Game: Two Knights Defense, Polerio Defense",
                                  "Italian Game: Two Knights Defense, Ulvestad Variation",
                                  "Frankenstein-Dracula Variation",
                                  "Indian Defense: Budapest Defense"
                                  ), 
                                collapse = "|"))) %>% 
  as_tibble() %>% 
  filter(!(str_detect(name, "Queen's Gambit Declined"))) %>%  # we don't want any QGD games
  filter(!(name == "Queen's Gambit")) %>%
  filter(!(str_detect(name, "Declined")))

# Identify a shortened "gambit name" for each full opening line 
# Eg "Four Knights Game: Halloween Gambit" becomes "Halloween Gambit"
gambits <- gambits_lookup
gambits$gambit_name <- stri_extract_last_regex(gambits$name,
                                               pattern = c("((:|,)\\s|(\\w+.\\w+.\\w+|\\w+.\\w+))\\s(Gambit|Countergambit)"))

# Also shorten other lines that aren't named "gambit"
gambits$gambit_name[str_detect(gambits$name, "Italian Game: Two Knights Defense, Fried Liver Attack")] <- "Fried Liver Attack"
gambits$gambit_name[str_detect(gambits$name, "Italian Game: Two Knights Defense, Traxler Counterattack")] <- "Traxler Counterattack"
gambits$gambit_name[str_detect(gambits$name, "Italian Game: Two Knights Defense, Ulvestad Variation")] <- "Ulvestad Variation (Two Knights)"
gambits$gambit_name[str_detect(gambits$name, "Italian Game: Two Knights Defense, Polerio Defense")] <- "Polerio Defense (Two Knights)"
gambits$gambit_name[str_detect(gambits$name, "Indian Defense: Budapest Defense")] <- "Budapest Defense"
gambits$gambit_name[str_detect(gambits$name, "Ruy Lopez: Marshall Attack")] <- "Ruy Lopez: Marshall Attack"
gambits$gambit_name[str_detect(gambits$name, "Caro-Kann Defense: Accelerated Panov Attack, Van Weersel Attack")] <- "Caro-Kann: Van Weersel Attack"
gambits$gambit_name[str_detect(gambits$name, "Frankenstein-Dracula Variation")] <- "Frankenstein-Dracula Variation"

# Save opening/gambit datasets
readr::write_csv(openings_lookup, paste0(here::here(), "/data/lookup/openings.csv"))
readr::write_csv(gambits, paste0(here::here(), "/data/lookup/gambits.csv"))

