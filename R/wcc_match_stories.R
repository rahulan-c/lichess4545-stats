
# =============================================================================
#                  WORLD CHAMPIONSHIP MATCH 'STORY' PLOTS
# =============================================================================

# RC | Dec 2021

# ---- DESCRIPTION ------------------------------------------------------------ 

# My attempt to plot the story of every chess World Championship match by 
# charting the evolution of each game, using Stockfish 14 move-by-move 
# evaluations, to indicate how the overall match reached its outcome. Will try
# to incorporate additional relevant match/player/game details too.

# Made possible by Lichess's recent project collating all WC match games into 
# PGN files. See https://lichess.org/page/world-championships.
# I'm using https://github.com/michael1241/wcc_analysis/tree/master/analysed_pgns

# Originally based on my script for making similar plots for Lichess4545 Team 
# League matches, for example:
# https://rahulan-c.github.io/lichess4545-stats/reports/stories/s28_r4_allmatches.pdf.

# But for these, I'll probably aim for something more like the match summary 
# graphics published by FiveThirtyEight:
# https://fivethirtyeight.com/wp-content/uploads/2021/11/roeder.chess-game2.png?w=700


# ---- OPTIONS ----------------------------------------------------------------

# Plot colours
p1_col <- "seagreen2"           
p2_col <- "coral1"            
eval_col <- "black"  
equality_col <- "#555F61"


# ---- PACKAGES ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, rvest, cowplot, cli, ggtext)


# ---- DATA -------------------------------------------------------------------

url <- "https://github.com/michael1241/wcc_analysis/tree/master/analysed_pgns"
prefix <- "https://raw.githubusercontent.com/michael1241/wcc_analysis/master/analysed_pgns/"

pgn_names <- read_html(url) %>%
  rvest::html_elements(".js-navigation-open.Link--primary") %>%
  rvest::html_text()

# Exclude tournaments (1948, 2007)
pgn_names <- pgn_names[!(str_detect(pgn_names,
                                    "lichess_study_1948-world-championship-tournament_by_Lichess_2021.11.29.pgn"))]
pgn_names <- pgn_names[!(str_detect(pgn_names,
                                    "lichess_study_2007-world-championship-tournament_by_Lichess_2021.11.29.pgn"))]

pgn_links <- paste0(prefix, pgn_names)

lst_matchdata <- list()
lst_matchsummary <- list()

# For testing, only use a small sample of games
sample_size <- 1
pgn_links <- sample(pgn_links, sample_size)
# pgn_links <- pgn_links[length(pgn_links)-1] # to manually test Carlsen-Caruana

# Get match data by parsing PGNs
for (m in seq(1:length(pgn_links))) {
  
  match <- readr::read_file(pgn_links[m])
  games <- str_split(match, "\\[Event")[[1]]
  
  match_games <- tibble::tibble(event = character(),
                                site = character(),
                                round = integer(),
                                white = character(),
                                black = character(),
                                result = character(),
                                evals = numeric(),
                                ply = integer(),
                                eco = character(),
                                opening = character())
  
  for (g in c(2:length(games))) {
    
    game <- games[[g]]
    
    event <- str_extract(game, "^.+\\]") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    site <- str_extract(game, "\\[Site .+\\]") %>% 
      str_remove("\\[Site ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    round <- str_extract(game, "\\[Round .+\\]") %>% 
      str_remove("\\[Round ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    round <- as.integer(round)
    
    white <- str_extract(game, "\\[White .+\\]") %>% 
      str_remove("\\[White ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    black <- str_extract(game, "\\[Black .+\\]") %>% 
      str_remove("\\[Black ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    result <- str_extract(game, "\\[Result .+\\]") %>% 
      str_remove("\\[Result ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    eco <- str_extract(game, "\\[ECO .+\\]") %>% 
      str_remove("\\[ECO ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    opening <- str_extract(game, "\\[Opening .+\\]") %>% 
      str_remove("\\[Opening ") %>% 
      str_remove("\\]") %>% 
      str_remove_all("\"")
    
    # Skip forfeits
    if(
      ((white == "Fischer, Robert James") & (black == "Spassky, Boris V") & (round == 2)) |
      ((white == "Kramnik,V") & (black == "Topalov,V") & (round == 5))
       ){
     next 
    }
    
    evals <- str_extract_all(game, "\\{ \\[%eval [:graph:]{1,}\\] \\}")[[1]] %>% 
      str_remove_all("\\{ \\[%eval ") %>% 
      str_remove_all("\\] \\}")
    
    mate_evals <- evals[str_detect(evals, "#")]
    mate_evals <- str_remove_all(mate_evals, "#")
    mate_evals <- as.numeric(mate_evals)
    mate_evals <- (mate_evals / abs(mate_evals)) * ((21 - pmin(abs(mate_evals), 10)))
    evals[str_detect(evals, "#")] <- mate_evals
    evals <- as.numeric(evals)
    
    ply <- seq(1:length(evals))
    
    match_games <- match_games %>% 
      add_row(
        event = event,
        site = site,
        round = round,
        white = white,
        black = black,
        result = result,
        evals = evals,
        ply = ply,
        eco = eco,
        opening = opening
      )
  }
  
  # Get player names (sorted A-Z)
  players <- sort(c(match_games$white[1], match_games$black[1]))
  
  # Get match score data
  game_data <- match_games %>% 
    distinct(round, .keep_all = TRUE) %>% 
    arrange(round) %>% 
    mutate(p1 = rep(players[1], nrow(.)),
           p2 = rep(players[2], nrow(.))) %>% 
    mutate(pts1 = case_when(
      result == "1-0" & white == players[1] ~ 1,
      result == "0-1" & white == players[1] ~ 0,
      result == "1/2-1/2" & white == players[1] ~ 0.5,
      result == "1-0" & white == players[2] ~ 0,
      result == "0-1" & white == players[2] ~ 1,
      result == "1/2-1/2" & white == players[2] ~ 0.5,
      TRUE ~ NA_real_
    )) %>% 
    mutate(pts2 = case_when(
      result == "1-0" & white == players[2] ~ 1,
      result == "0-1" & white == players[2] ~ 0,
      result == "1/2-1/2" & white == players[2] ~ 0.5,
      result == "1-0" & white == players[1] ~ 0,
      result == "0-1" & white == players[1] ~ 1,
      result == "1/2-1/2" & white == players[1] ~ 0.5,
      TRUE ~ NA_real_
    )) %>% 
    mutate(post1 = cumsum(pts1),
           post2 = cumsum(pts2),
           margin = post1 - post2,
           pre1 = post1 - pts1,
           pre2 = post2 - pts2,
           premargin = pre1 - pre2) %>% 
    select(-c(evals, ply))
  
  lst_matchdata[[m]] <- match_games
  lst_matchsummary[[m]] <- game_data
  cli::cli_inform("{m}/{length(pgn_links)}")
}

cli::cli_inform("")
cli::cli_inform("all done!")



# ----  PLOT  -----------------------------------------------------------------

# Test method on a single match
selection <- sample(1:sample_size, 1)

matchdata <- lst_matchdata[[selection]]
matchsummary <- lst_matchsummary[[selection]]

matchsummary_sub <- matchsummary %>% 
  select(round, p1, p2, pts1, pts2, post1, post2, margin, pre1, pre2,
         premargin)

matchdata <- dplyr::left_join(matchdata, matchsummary_sub, by = c("round")) %>%
  mutate(player = ifelse(ply %% 2 == 1, white, black)) %>% 
  rename("eval" = evals)

# Calculate scaled evals using Lichess's eval charts scale
matchdata <- matchdata %>%
  arrange(round, ply) %>% 
  mutate(evalsc = 2 / (1 + exp(-0.004 * eval * 100)) - 1) %>% 
  mutate(evalsc = ifelse(white == p1, evalsc,
                                   (-1 * evalsc)
  )) %>%
  # Compute a scaled match eval measure - this is what gets plotted as the eval line
  mutate(mevalsc = premargin + evalsc)

players <- sort(c(matchdata$white[1], matchdata$black[1]))


# Make plot
plt_story <- ggplot(data = matchdata) +
  
  # Area under player 2's match eval line
  geom_ribbon(data = matchdata, 
              aes(ymin = pmin(evalsc, 0), 
                  ymax = 0, 
                  x = ply / 2, 
                  y = evalsc), 
              fill = p2_col, 
              col = "#ffffff", 
              alpha = 0.5, 
              size = 0.1) +
  
  # Area under player 1's match eval line
  geom_ribbon(data = matchdata, 
              aes(ymin = 0, 
                  ymax = pmax(evalsc, 0), 
                  x = ply / 2, 
                  y = evalsc), 
              fill = p1_col, 
              col = "#ffffff", 
              alpha = 0.5, 
              size = 0.1) +
  
  # Match eval line
  geom_line(data = matchdata, aes(x = ply / 2, y = evalsc), 
            col = eval_col, 
            size = 0.9) +
  
  # Colour indicators
  # annotate("point",
  #          x = 0,
  #          y = ifelse(matchdata$white[1] == matchdata$p1[1], 1, -1),
  #          colour = "white",
  #          size = 2) +
  
  
  
  ylim(c(-1, 1)) +
  xlim(c(0, 100)) +

  theme_minimal() +
  # theme_cowplot() +
  
  facet_wrap(~round, ncol = 4, scales = "fixed") +
  
  
  
  # Axis labels, title etc - for now at least
  labs(title = paste0(players[1], " vs ", players[2]),
       subtitle = paste0(str_trim(matchsummary$event[1]), " ", matchsummary$site[1]),
       caption = "DRAFT - WORK IN PROGRESS") +

  # Remove gridlines
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # axis.line = element_blank(),
    axis.title = element_blank()
    # axis.ticks = element_blank(),
    # axis.text = element_blank(),
    # plot.title = element_blank(),
    # plot.subtitle = element_blank(),
    # plot.caption = element_blank()
    # text = element_text(family = plotinfo_font, colour = "grey25")
  )


# Show plot
plt_story


# ---- TODO / ISSUES ----------------------------------------------------------

# - Go for sthg more like 538's graphics: 