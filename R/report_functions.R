
# HELPER FUNCTIONS FOR SEASON REPORTS

# Last updated: 2021-12-17

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, data.table, reactable, httr, jsonlite, xml2, 
               rvest, ndjson, reshape2, utf8, lubridate, tictoc, reticulate,
               rmarkdown, fs, stringi, git2r, glue, here, distill, htmltools,
               tidyjson, magick)


with_tooltip <- function(value, tooltip, ...) {
  # Custom tooltips for reactable tables
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy(value, tooltip, ...))
}

LoadLookups <- function(){
  # Load supporting lookup data
  # 1. Gambit openings - to enable analysis of gambits and gambit-lovers
  gambits_lookup <- read.csv(glue::glue("{here::here()}/data/lookup/gambits.csv"))
  
  # 2. FIDE performance rating lookup values
  # Source: https://handbook.fide.com/chapter/B022017
  # Required for calculating player performance ratings using FIDE's method 
  fide_tpr_lookup <- read.csv(glue::glue("{here::here()}/data/lookup/fide_tpr_lookup.csv"))
  return(list(gambits_lookup, fide_tpr_lookup))
}

LeagueColours <- function(league = league){
  # Set league-specific colours to use in plots
  if(league == "team4545"){
    league_col <- "#557f97"
    league_col_dark <- "#245164"
  }
  if(league == "lonewolf"){
    league_col <- "#699486"
    league_col_dark <- "#225622"
  }
  if(league == "chess960"){
    league_col <- "#c69501"
    league_col_dark <- "#c69501"
  }
  league_col <- league_col_dark
  return(list(league_col, league_col_dark))
}

BoardsPerTeam <- function(league = league, season = season){
  team_boards <- 99
  # Get correct team_boards parameter
  if(league != "team4545"){team_boards <- 99} else
    if(season == 1){team_boards <- 4} else
      if(season == 2){team_boards <- 5} else
        if(season <= 15){team_boards <-  6} else
          if(season <= 24){team_boards <-  8} else
            if (season <= 29){team_boards <- 10} else
              if(season <= 99){team_boards <-  8}
  
  return(team_boards)
}

SeasonReportData <- function(data_path, league, lw_u1800, league_load_label, season,
                             update_alltime_data){
  
  
  games <- readRDS(paste0(data_path, "games_", league_load_label, "_s", season, ".rds")) %>% as_tibble()
  website_pairings <- readRDS(paste0(data_path, "website_pairings_", league_load_label, "_s", season, ".rds")) %>% as_tibble()
  pairings <- rio::import(paste0(data_path, "pairings_", league_load_label, "_s", season, ".csv")) %>% as_tibble()
  positions <- rio::import(paste0(data_path, "positions_", league_load_label, "_s", season, ".csv")) %>% as_tibble()
  
  # Update all-time league games data if necessary before loading it
  if(update_alltime_data){
    UpdateAllTimeGames(league, season)
  }
  
  # Read all-time games data
  if(league == "team4545"){alltime_label <- "team4545"}
  if(league == "lonewolf"){
    if(lw_u1800){
      alltime_label <- "lwu1800"
    } else {
      alltime_label <- "lwopen"
    }
  }
  if(league == "chess960"){alltime_label <- "chess960"}
  
  all_games <- readRDS(paste0(data_path, "allgames_", alltime_label, ".rds"))
  all_games <- tibble::as_tibble(all_games)
  return(list(games, website_pairings, pairings, positions, all_games))
}

SeasonDates <- function(games = games){
  season_dates <- paste0(lubridate::day(min(games$started)), " ",
                         lubridate::month(min(games$started), label = T, abbr = F), " ", 
                         lubridate::year(max(games$started)),
                         " and ",
                         lubridate::day(max(games$started)), " ",
                         lubridate::month(max(games$started), label = T, abbr = F), " ",
                         lubridate::year(max(games$started)))
  return(season_dates)
}

GetWDLStats <- function(website_pairings, games){
  # White wins, draws and Black wins
  # Computed from website (played) pairings data
  wdl <- website_pairings %>% 
    group_by(result) %>% 
    tally() %>%
    filter(result %in% c("1-0", "0-1", "1/2-1/2")) %>% 
    mutate(perc = n/nrow(games))
  return(wdl)
}

GamesByEnding <- function(games){
  # How games ended
  ended <- games %>% group_by(status) %>% tally() %>% 
    mutate(status = replace(status, status == "outoftime", "out of time")) %>%
    mutate(status = replace(status, status == "stalemate", "stalemate")) %>%
    arrange(desc(n))
  return(ended)
}

NewPlayers <- function(games, all_games){
  season_players <- games %>% 
    select(white, black) %>% 
    as_tibble()
  prev_players <- all_games %>% 
    filter(started < min(games$started)) %>% 
    select(white, black) %>% 
    as_tibble()
  prev_players <- rbind(tibble("player" = prev_players$white),
                        tibble("player" = prev_players$black)) %>% 
    distinct(player) %>% 
    dplyr::pull() %>% 
    sort()
  season_players <- rbind(tibble("player" = season_players$white),
                          tibble("player" = season_players$black)) %>% 
    distinct(player) %>%
    dplyr::pull() %>% 
    sort()
  new_players <- unique(season_players[! season_players %in% prev_players])
  return(length(new_players))
}



ResultByColourByBoard <- function(games = games){
  wins_by_col <- games %>% 
    select(board, result) %>% 
    add_count(board, name = "n_board") %>% 
    add_count(board, result, name = "n_board_result") %>% 
    mutate(perc = n_board_result / n_board) %>% 
    distinct() %>% 
    mutate(cat = case_when(
      result == "1-0" ~ "cat1",
      result == "1/2-1/2" ~ "cat2",
      result == "0-1" ~ "cat3",
      TRUE ~ NA_character_
    )) %>%
    group_by(board) %>% 
    arrange(desc(cat)) %>% 
    # Get the % labels positioned right
    mutate(pos_label = ifelse(cat == "cat3",
                              # White wins
                              perc / 2, 
                              ifelse(cat == "cat2",
                                     # Draws
                                     lag(perc) + (perc/2), 
                                     ifelse(cat == "cat1",
                                            # Black wins
                                            lag(perc, 2) + lag(perc) + (perc/2), 
                                            0)))) %>% 
    ungroup()
  
  plt_wins_by_col <- ggplot(wins_by_col, aes(x = board, y = n_board_result, fill = cat)) +
    geom_bar(stat = "identity", position = "fill", colour = "#ffffff") +
    theme_cowplot() +
    scale_x_continuous(trans = "reverse", 
                       labels = paste0("B",as.character(wins_by_col$board)), 
                       breaks = wins_by_col$board) +
    scale_y_continuous(trans = "reverse", labels = scales::percent, breaks = waiver()) +
    geom_text(aes(y = pos_label, label = paste0(round(perc*100, 0), "%")), colour = "white") +
    labs(x = "", y = "") +
    theme(legend.justification = "centre",
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank()) +
    scale_fill_manual(values = c("#b5b5b5", "#6c6c6c", "#383838"), 
                      labels = c("1-0", "1/2-1/2", "0-1")) +
    coord_flip()
  return(plt_wins_by_col)
}


ResultByColourByRatingBand <- function(games = games){
  # LW plot
  # Make average ratings bins
  games$rating_group <- cut(games$mean_rating, 
                            breaks=c(800, 1000, 1200, 1400, 1600, 1800, 2000,
                                     2200, 2400, 2600),
                            labels = c("800-1000", "1000-1200",
                                       "1200-1400", "1400-1600", "1600-1800",
                                       "1800-2000", "2000-2200",
                                       "2200-2400", "2400-2600"),
                            include.lowest=TRUE, dig.lab = 4)
  
  wins_by_col <- games %>% 
    select(rating_group, result) %>% 
    add_count(rating_group, name = "n_group") %>% 
    add_count(rating_group, result, name = "n_group_result") %>% 
    mutate(perc = n_group_result / n_group) %>% 
    distinct() %>% 
    mutate(cat = case_when(
      result == "1-0" ~ "cat1",
      result == "1/2-1/2" ~ "cat2",
      result == "0-1" ~ "cat3",
      TRUE ~ NA_character_
    )) %>%
    group_by(rating_group) %>% 
    arrange(desc(cat)) %>% 
    # Get the % labels positioned right
    mutate(pos_label = ifelse(cat == "cat3",
                              # White wins
                              perc / 2, 
                              ifelse(cat == "cat2",
                                     # Draws
                                     lag(perc) + (perc/2), 
                                     ifelse(cat == "cat1",
                                            # Black wins
                                            lag(perc, 2) + lag(perc) + (perc/2), 
                                            0)))) %>% 
    ungroup()
  
  plt_wins_by_col <- ggplot(wins_by_col, aes(x = rating_group, y = n_group_result, fill = cat)) +
    geom_bar(stat = "identity", position = "fill", colour = "#ffffff") +
    theme_cowplot() +
    # scale_x_continuous(trans = "reverse", 
    #                    labels = paste0("B",as.character(wins_by_col$rating_group)), 
    #                    breaks = wins_by_col$rating_group) +
    scale_y_continuous(trans = "reverse", labels = scales::percent, breaks = waiver()) +
    geom_text(aes(y = pos_label, label = paste0(round(perc*100, 0), "%")), colour = "white") +
    labs(x = "", y = "") +
    theme(legend.justification = "centre",
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank()) +
    scale_fill_manual(values = c("#b5b5b5", "#6c6c6c", "#383838"), 
                      labels = c("1-0", "1/2-1/2", "0-1")) +
    coord_flip()
  return(plt_wins_by_col)
}


PodiumTeams <- function(summary_url){
  # For 4545...
  
  # 1st place team name
  first <- read_html(summary_url) %>% 
    html_element(".first-place .team-link") %>% 
    html_text()
  # 1st place team link
  first_link <- read_html(summary_url) %>% 
    html_element(".first-place .team-link") %>%
    html_attr("href")
  first_link <- paste0("https://www.lichess4545.com", first_link)
  
  # 2nd place team name
  second <- read_html(summary_url) %>% 
    html_element(".second-place .team-link") %>% 
    html_text()
  # 2nd place team link
  second_link <- read_html(summary_url) %>% 
    html_element(".second-place .team-link") %>%
    html_attr("href")
  second_link <- paste0("https://www.lichess4545.com", second_link)
  
  # 3rd place team name
  third <- read_html(summary_url) %>% 
    html_element(".third-place .team-link") %>% 
    html_text()
  # 3rd place team link
  third_link <- read_html(summary_url) %>% 
    html_element(".third-place .team-link") %>%
    html_attr("href")
  third_link <- paste0("https://www.lichess4545.com", third_link)
  return(list(first, first_link, second, second_link, third, third_link))
}


PodiumPlayers <- function(summary_url){
  # Get details of players who finished 1st, 2nd and 3rd in a season
  
  # Function to return player's season score
  GetSeasonScore <- function(url){
    score <- rvest::read_html(url) %>%
      rvest::html_nodes(".player-profile-section+ .player-profile-section td:nth-child(1)") %>% 
      rvest::html_text() %>% 
      stringr::str_replace_all("[\r\n]" , "") %>% 
      stringr::str_squish() %>% 
      stringr::str_c()
    return(score)
  }
  
  # Function to return player's season perf rating
  GetSeasonPerf <- function(url){
    perf <- rvest::read_html(url) %>%
      rvest::html_nodes(".player-profile-section+ .player-profile-section span") %>% 
      rvest::html_text() %>% 
      stringr::str_replace_all("[\r\n]" , "") %>% 
      stringr::str_squish() %>% 
      stringr::str_c()
    return(perf)
  }
  
  # 1st place finisher
  first <- read_html(summary_url) %>% 
    html_element(".first-place .player-link") %>% 
    html_text()
  first_link <- read_html(summary_url) %>% 
    html_element(".first-place .player-link") %>%
    html_attr("href")
  first_link <- paste0("https://www.lichess4545.com", first_link)
  first_score <- GetSeasonScore(first_link)
  first_perf <- GetSeasonPerf(first_link)
  
  
  # 2nd place finisher
  second <- read_html(summary_url) %>% 
    html_element(".second-place .player-link") %>% 
    html_text()
  second_link <- read_html(summary_url) %>% 
    html_element(".second-place .player-link") %>%
    html_attr("href")
  second_link <- paste0("https://www.lichess4545.com", second_link)
  second_score <- GetSeasonScore(second_link)
  second_perf <- GetSeasonPerf(second_link)
  
  # 3rd place finisher
  third <- read_html(summary_url) %>% 
    html_element(".third-place .player-link") %>% 
    html_text()
  third_link <- read_html(summary_url) %>% 
    html_element(".third-place .player-link") %>%
    html_attr("href")
  third_link <- paste0("https://www.lichess4545.com", third_link)
  third_score <- GetSeasonScore(third_link)
  third_perf <- GetSeasonPerf(third_link)
  
  # LW U2000 (Open) or U1600 (U1800) prize winners
  prize <- read_html(summary_url) %>% 
    html_element(".u1600-winner .player-link") %>% 
    html_text()
  prize_link <- read_html(summary_url) %>% 
    html_element(".u1600-winner .player-link") %>%
    html_attr("href")
  prize_link <- paste0("https://www.lichess4545.com", prize_link)
  prize_score <- GetSeasonScore(prize_link)
  prize_perf <- GetSeasonPerf(prize_link)
  
  
  return(list(first, first_link, first_score, first_perf,
              second, second_link, second_score, second_perf,
              third, third_link, third_score, third_perf, prize, prize_link, 
              prize_score, prize_perf))
}


PodiumTeamPlayers <- function(positions, games, league, season){
  # Identify top 3 teams
  top_3 <- positions %>% 
    filter(round == max(positions$round)) %>% 
    filter(rank %in% c(1:3))
  
  # Identify everyone who played for the top 3 teams
  players_teams <- rbind(tibble("player" = games$white, "team" = games$team_w, "round" = games$round),
                         tibble("player" = games$black, "team" = games$team_b, "round" = games$round)) %>% 
    group_by(player, team) %>% 
    summarise(games = n()) %>% 
    arrange(desc(games))
  
  players_gold <- players_teams %>% 
    filter(team == top_3$team[top_3$rank == 1]) %>% 
    arrange(desc(games)) %>% 
    mutate(player_games = paste0(player, " (", games, ")")) %>% 
    dplyr::pull(player_games)
  
  
  players_silver <- players_teams %>% 
    filter(team == top_3$team[top_3$rank == 2]) %>% 
    arrange(desc(games)) %>% 
    mutate(player_games = paste0(player, " (", games, ")")) %>% 
    dplyr::pull(player_games)
  
  
  players_bronze <- players_teams %>% 
    filter(team == top_3$team[top_3$rank == 3]) %>% 
    arrange(desc(games)) %>% 
    mutate(player_games = paste0(player, " (", games, ")")) %>% 
    dplyr::pull(player_games)
  
  # Identify length of longest team list
  longest_team <- max(c(length(players_gold), 
                        length(players_silver), 
                        length(players_bronze)))
  
  # players_gold <- sort(players_gold)
  if(length(players_gold) < longest_team){
    players_gold <- c(players_gold, rep("", longest_team - length(players_gold)))
  }
  
  # players_silver <- sort(players_silver)
  if(length(players_silver) < longest_team){
    players_silver <- c(players_silver, rep("", longest_team - length(players_silver)))
  }
  
  # players_bronze <- sort(players_bronze)
  if(length(players_bronze) < longest_team){
    players_bronze <- c(players_bronze, rep("", longest_team - length(players_bronze)))
  }
  
  # Create final table of players
  winners <- tibble("Gold" = players_gold,
                    "Silver" = players_silver,
                    "Bronze" = players_bronze)
  
  # Swap teams in cases where podium placements required advanced tiebreaks 
  if(league == "team4545"){
    
    # S11: swap 1st and 2nd
    if(season == 11){
      winners <- tibble("Gold" = players_silver,
                        "Silver" = players_gold,
                        "Bronze" = players_bronze)
    }
    
    # S11: swap 2nd and 3rd
    if(season == 16){
      winners <- tibble("Gold" = players_gold,
                        "Silver" = players_bronze,
                        "Bronze" = players_silver)
    }
  }
  return(winners)
}


SeasonRankTracker <- function(league = league, positions = positions, league_col_dark = league_col_dark) {
  if(league == "team4545"){
    top_places <- positions %>% 
      filter(round == 8) %>% 
      arrange(rank) %>% 
      filter(rank <= 5) %>% 
      dplyr::pull(team)
    position_tracker <- positions %>% 
      mutate(round = as.factor(round)) %>% 
      ggplot(aes(x = round, y = rank, colour = team)) +
      geom_line(aes(group = team), size = 2.5) +
      # geom_point(aes(group = team), size = 2) +
      theme_minimal() +
      # scale_y_reverse(limits = c(20, 1), breaks = integer_breaks()) +
      scale_y_reverse(limits = c(max(positions$rank), 1), breaks = floor(seq(max(positions$rank), 1, length.out = 4))) +
      scale_x_discrete(expand = expansion(add = c(0.1, 0.1))) +
      scale_color_manual(values = c("#ffd700", "#7b7b7b", "#cd7f32", "#a6c7ff", "#d9e7ff"),
                         breaks = c(top_places),
                         labels = c(top_places)) +
      gghighlight::gghighlight(team %in% top_places, use_direct_label = F,
                               unhighlighted_params = list(colour = alpha("grey", 0.1)),
                               label_params = list(size = 4, 
                                                   nudge_x = -1, 
                                                   nudge_y = -15, 
                                                   direction = "y",
                                                   fill = "white",
                                                   colour = "black")) +
      labs(x = "Round",
           y = "Rank after each round") +
      theme(legend.position = "top") +
      theme(panel.grid.minor = element_blank()) +
      theme(
        legend.position = c(0.5, 0.3),
        legend.title = element_blank(),
        legend.box.background = element_rect(fill = "white", colour = league_col_dark)
      )
  }
  
  if(league == "lonewolf"){
    top_places <- positions %>% 
      filter(round == 12) %>% 
      arrange(pos) %>% 
      filter(pos <= 5) %>%  
      dplyr::pull(player)
    position_tracker <- positions %>% 
      filter(!(is.na(pos))) %>% 
      mutate(player = str_to_lower(player)) %>% 
      mutate(round = as.factor(round)) %>% 
      ggplot(aes(x = round, y = pos, colour = player)) +
      geom_line(aes(group = player), size = 2) +
      # geom_point(aes(group = player), size = 2) +
      theme_minimal() +
      scale_y_reverse(limits = c(50, 1), breaks = c(1, 10, 20, 30, 40, 50)) +
      scale_x_discrete(expand = expansion(add = c(0.1, 0.1))) +
      gghighlight::gghighlight(player %in% top_places, use_direct_label = F,
                               unhighlighted_params = list(colour = alpha("grey", 0.1)),
                               label_params = list(size = 4, nudge_x = 2, nudge_y = 0, direction = "y")) +
      labs(x = "Round",
           y = "Rank before round") +
      theme(legend.position = "right") +
      scale_color_manual(values = c("#ffd700", "#7b7b7b", "#cd7f32", "#69bfb3", "#9fd6ce"),
                         breaks = c(top_places),
                         labels = c(top_places)) +
      theme(panel.grid.minor = element_blank()) +
      theme(
        legend.position = c(0.5, 0.3),
        legend.title = element_blank(),
        legend.box.background = element_rect(fill = "white", colour = league_col_dark),
        legend.direction = "horizontal"
      )
  }
  
  if(league == "chess960"){
    top_places <- positions %>% 
      filter(round == 9) %>% 
      arrange(pos) %>% 
      filter(pos <= 5) %>%  
      dplyr::pull(player)
    position_tracker <- positions %>% 
      filter(!(is.na(pos))) %>% 
      mutate(player = str_to_lower(player)) %>% 
      mutate(round = as.factor(round)) %>% 
      ggplot(aes(x = round, y = pos, colour = player)) +
      geom_line(aes(group = player), size = 2.5) +
      # geom_point(aes(group = player), size = 2) +
      theme_minimal() +
      scale_y_reverse(limits = c(20, 1), breaks = c(1, 10, 20, 30, 40, 50)) +
      scale_x_discrete(expand = expansion(add = c(0.1, 0.1))) +
      gghighlight::gghighlight(player %in% top_places, use_direct_label = F,
                               unhighlighted_params = list(colour = alpha("grey", 0.1)),
                               label_params = list(size = 4, nudge_x = 2, nudge_y = 0, direction = "y")) +
      labs(x = "Round",
           y = "Rank before round") +
      theme(legend.position = "right") +
      scale_color_manual(values = c("#ffd700", "#7b7b7b", "#cd7f32", "#FFF2B1", "#CCC18D"),
                         breaks = c(top_places),
                         labels = c(top_places)) +
      theme(panel.grid.minor = element_blank()) +
      theme(
        legend.position = c(0.5, 0.3),
        legend.title = element_blank(),
        legend.box.background = element_rect(fill = "white", colour = league_col_dark),
        legend.direction = "horizontal"
      )
  }
  
  
  return(position_tracker)
}


BoardPerfRatings <- function(pairings, fide_tpr_lookup, tos_violators,
                             team_boards) {
  bperfs_w <- pairings %>% 
    filter(winner != "forfeit/unplayed") %>% 
    group_by(white, board) %>%
    summarise(sum_opp_rating = sum(rating_b),
              games = n(),
              points = ((sum(winner == "white") * 1) + (sum(winner == "draw") * 0.5)),
              wins = sum(winner == "white"),
              draws = sum(winner == "draw"),
              losses = sum(winner == "black"),
              initial_rating = rating_w[which.min(round)],
              initial_round = min(round)) %>% 
    rename("player" = white)
  
  bperfs_b <- pairings %>% 
    filter(winner != "forfeit/unplayed") %>% 
    group_by(black, board) %>%
    summarise(sum_opp_rating = sum(rating_w),
              games = n(),
              points = ((sum(winner == "black") * 1) + (sum(winner == "draw") * 0.5)),
              wins = sum(winner == "black"),
              draws = sum(winner == "draw"),
              losses = sum(winner == "white"),
              initial_rating = rating_b[which.min(round)],
              initial_round = min(round)) %>% 
    rename("player" = black)
  
  bperfs <- rbind(bperfs_w, bperfs_b)
  rm(bperfs_w, bperfs_b)
  
  # Compute board performance ratings using FIDE method
  # Create list with board-specific rankings
  bperfs <- bperfs %>% 
    group_by(player, board) %>% 
    summarise(sum_opp_rating = sum(sum_opp_rating),
              games = sum(games),
              avg_opp_rating = sum_opp_rating / games,
              points = sum(points),
              tp_score = points / games,
              wins = sum(wins),
              draws = sum(draws),
              losses = sum(losses),
              initial_rating = initial_rating[which.min(initial_round)]) %>%
    mutate(adj = fide_tpr_lookup[match(round(tp_score, 2), fide_tpr_lookup$p),2]) %>% 
    mutate(bperf_rating = avg_opp_rating + adj) %>% 
    mutate(bperf_diff = bperf_rating - initial_rating) %>% 
    filter(games >= 5) %>%
    ungroup() %>% 
    group_by(board) %>% 
    arrange(desc(bperf_rating)) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% # remove ToS violators
    slice_max(order_by = bperf_rating, n = 3) %>% 
    mutate(rank = paste0("brank_", rep(1:3, n = team_boards))) %>% 
    select(board, rank, player, games, points, bperf_rating)
  return(bperfs)
}



GamesByDay <- function(games, league_col){
  
  games <- games %>% 
    as_tibble() %>% 
    mutate(wday = lubridate::wday(started, label = TRUE, week_start = 1),
           hour = lubridate::hour(games$started))
  
  # Create summary table with info for heatmap
  games_by_day_time <- plyr::ddply(games, c("hour", "wday"),
                                   summarise,
                                   N = length(started))
  # Reverse order of months
  games_by_day_time$wday <- factor(games_by_day_time$wday, levels=rev(levels(games_by_day_time$wday)))
  
  # Make bar chart of games per day
  plt_gamesbyday <- games_by_day_time %>% 
    group_by(wday) %>% 
    tally(N) %>% 
    mutate(wday = fct_relevel(wday, 
                              "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>% 
    ggplot(aes(wday, n)) +
    geom_col(fill = league_col, colour = league_col) +
    geom_text(aes(label = n), colour = "white", size = 5, vjust = 1.5) +
    theme_minimal() +
    labs(x = "Day", y = "Games", 
         subtitle = paste0(round((sum(games_by_day_time[games_by_day_time$wday %in% c("Sat", "Sun"), ]$N) / nrow(games)) * 100, 0),
                           "% played on weekend")) +
    scale_y_continuous(breaks = integer_breaks()) +
    theme(axis.text.x = element_text(size = 12)) +
    theme(plot.title = element_text(size = 10, hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  return(plt_gamesbyday)
}


GamesByDayAndHour <- function(games, league_col){
  
  games <- games %>% 
    as_tibble() %>% 
    mutate(wday = lubridate::wday(started, label = TRUE, week_start = 1),
           hour = lubridate::hour(games$started))
  
  # Create summary table with info for heatmap
  games_by_day_time <- plyr::ddply(games, c("hour", "wday"),
                                   summarise,
                                   N = length(started))
  # Reverse order of months
  games_by_day_time$wday <- factor(games_by_day_time$wday, levels=rev(levels(games_by_day_time$wday)))
  
  heat_dayhour <- ggplot(games_by_day_time, aes(hour, wday)) + geom_tile(aes(fill = N),colour = "white", na.rm = TRUE) +
    scale_fill_gradient(low = "white", high = league_col) +
    guides(fill=guide_legend(title="# games")) +
    theme_bw() + theme_minimal() +
    labs(x = "Hour (UTC)", y = "Day") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  return(heat_dayhour)
}


MovesPerGameHist <- function(games, league_col){
  moves_hist <- games %>% 
    ggplot(aes(num_moves)) +
    geom_histogram(binwidth = 10, fill = league_col, col = "white") +
    theme_cowplot() +
    geom_vline(xintercept = mean(games$num_moves), size = 1, colour="#333333", linetype = "dashed") +
    scale_y_continuous(breaks = integer_breaks()) +
    labs(subtitle = paste0("Average moves/game: ", round(mean(games$num_moves), 1)),
         x = "# moves", y = "Games") +
    scale_x_continuous(breaks = seq(0, max(games$num_moves), by = 10)) +
    theme(plot.subtitle = element_text(hjust = 0.5),
          panel.grid.minor = element_blank())
  return(moves_hist)
}


GameDurationHist <- function(games, league_col){
  # Game duration histogram
  duration_hist <- games %>% 
    ggplot(aes(duration/60)) +
    geom_histogram(binwidth = 15, fill = league_col, col = "white") +
    theme_cowplot() +
    geom_vline(xintercept = mean(games$duration/60), size = 1, colour="#333333", linetype = "dashed") +
    labs(x = "Minutes", y = "Games",
         subtitle = paste0("Average time taken/game: ", round(mean(games$duration/60), 1), " minutes")) +
    scale_y_continuous(breaks = integer_breaks()) +
    scale_x_continuous(breaks = seq(0, max(games$duration/60), by = 30)) +
    theme(plot.subtitle = element_text(hjust = 0.5),
          panel.grid.minor = element_blank())
  return(duration_hist)
}


LongestGamesByMoves <- function(games, above_perc){
  longest_moves <- games %>% 
    arrange(desc(num_moves)) %>% 
    select("white" = players.white.user.name, 
           "black" = players.black.user.name, 
           id, 
           num_moves,
           started) %>% 
    mutate(players = paste0(white, " - ", black),
           url = paste0("https://lichess.org/", id),
           rank = dense_rank(desc(num_moves))) %>% 
    select(rank, players, started, url, num_moves) %>% 
    mutate(date = sprintf('%s %2d %2s %2d', lubridate::wday(started, label=T, abbr=T), lubridate::day(started), lubridate::month(started, label = T, abbr = T), lubridate::year(started))) %>% 
    mutate(prank = ntile(num_moves, 100)) %>% # calculate percentiles
    filter(prank >= above_perc) %>%  # only show games above a certain percentile
    select(-c(prank, started)) %>% 
    select(rank, players, date, url, num_moves)
  return(longest_moves)
}


LongestGamesByDuration <- function(games, above_perc){
  longest_duration <- games %>% 
    arrange(desc(duration)) %>% 
    select("white" = players.white.user.name, 
           "black" = players.black.user.name, 
           id, 
           duration,
           started) %>% 
    mutate(players = paste0(white, " - ", black),
           url = paste0("https://lichess.org/", id),
           rank = dense_rank(desc(duration))) %>% 
    select(rank, players, started, url, duration) %>% 
    mutate(duration = lubridate::seconds_to_period(duration)) %>%
    mutate(duration_print = sprintf('%2gh %2gm %.0fs', lubridate::hour(duration), lubridate::minute(duration), lubridate::second(duration))) %>% # more readable total times
    mutate(date = sprintf('%s %2d %2s %2d', lubridate::wday(started, label=T, abbr=T), lubridate::day(started), lubridate::month(started, label = T, abbr = T), lubridate::year(started))) %>% 
    mutate(prank = ntile(duration, 100)) %>% # calculate percentiles
    filter(prank >= above_perc) %>%  # only show games above certain percentile
    select(rank, players, date, url, duration_print)
  return(longest_duration)
}


ErrorRates <- function(league, games, league_col){
  if(league == "team4545"){
    # First, compute ACPL and blunder rates by board
    error_rates_w <- games %>% 
      select(white, board, num_moves, 
             players.white.analysis.blunder, players.white.analysis.mistake, players.white.analysis.inaccuracy, players.white.analysis.acpl,
             players.black.analysis.blunder, players.black.analysis.mistake, players.black.analysis.inaccuracy, players.black.analysis.acpl)
    
    error_rates_b <- games %>% 
      select(black, board, num_moves, 
             players.black.analysis.blunder, players.black.analysis.mistake, players.black.analysis.inaccuracy, players.black.analysis.acpl,
             players.white.analysis.blunder, players.white.analysis.mistake, players.white.analysis.inaccuracy, players.white.analysis.acpl)
    
    colnames(error_rates_w) <- c("player", "board", "moves", "blunders", "mistakes", "inaccuracies", "acpl",
                                 "opp_blunders", "opp_mistakes", "opp_inaccuracies", "opp_acpl")
    colnames(error_rates_b) <- c("player", "board", "moves", "blunders", "mistakes", "inaccuracies", "acpl",
                                 "opp_blunders", "opp_mistakes", "opp_inaccuracies", "opp_acpl")
    
    error_rates <- rbind(error_rates_w, error_rates_b)
    rm(error_rates_w, error_rates_b)
    
    error_rates <- error_rates %>% 
      mutate(cpl = acpl * moves,
             opp_cpl = opp_acpl * moves)
    
    # Combine to get final table
    error_rates_grouped <- error_rates %>% 
      group_by(board) %>% 
      summarise(games = n(),
                blunder_rate = sum(blunders, na.rm = T) / sum(moves, na.rm = T),
                mistake_rate = sum(mistakes, na.rm = T) / sum(moves, na.rm = T),
                inaccuracy_rate = sum(inaccuracies, na.rm = T) / sum(moves, na.rm = T),
                error_rate = blunder_rate + mistake_rate + inaccuracy_rate,
                acpl = sum(cpl, na.rm = T) / sum(moves, na.rm = T)) %>% 
      mutate(blunder_rate = round(blunder_rate * 100, 1),
             error_rate = round(error_rate * 100, 1),
             acpl = round(acpl, 1)) %>% 
      select(board, games, acpl, blunder_rate) %>% 
      arrange(board)
    
    # Plot ACPL by board
    plt_acpl <- ggplot(error_rates_grouped, aes(acpl, board, 
                                                label = round(acpl))) +
      geom_segment(aes(x = 0, y = board, xend = acpl, yend = board), colour = "grey50") +
      geom_point(colour = league_col, size = 10) +
      theme_cowplot() + 
      scale_y_continuous(trans = "reverse", 
                         labels = paste0("B",as.character(error_rates_grouped$board)), 
                         breaks = error_rates_grouped$board) +
      labs(y = "Board", x = "ACPL", title = "ACPL by board") +
      geom_text(colour = "white", size = 4) +
      theme(axis.title.y = element_blank()) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Plot blunder rates by board
    plt_blunder_rates <- ggplot(error_rates_grouped, aes(blunder_rate, board, 
                                                         label = scales::percent(blunder_rate/100, accuracy = 1))) +
      geom_segment(aes(x = 0, y = board, xend = blunder_rate, yend = board), colour = "grey50") +
      geom_point(colour = league_col, size = 10) +
      theme_cowplot() + 
      scale_y_continuous(trans = "reverse",
                         labels = paste0("B",as.character(error_rates_grouped$board)), 
                         breaks = error_rates_grouped$board) +
      labs(y = "Board", x = "Blunder rate (%)", title = "Blunder rate by board") +
      # scale_x_continuous(breaks = integer_breaks()) +
      geom_text(colour = "white", size = 4) +
      theme(axis.title.y = element_blank()) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else {
    
    # Plot errors by rating band
    error_rates_w <- games %>% 
      select(white, rating_w, num_moves, 
             players.white.analysis.blunder, players.white.analysis.mistake, players.white.analysis.inaccuracy, players.white.analysis.acpl,
             players.black.analysis.blunder, players.black.analysis.mistake, players.black.analysis.inaccuracy, players.black.analysis.acpl)
    
    error_rates_b <- games %>% 
      select(black, rating_b, num_moves, 
             players.black.analysis.blunder, players.black.analysis.mistake, players.black.analysis.inaccuracy, players.black.analysis.acpl,
             players.white.analysis.blunder, players.white.analysis.mistake, players.white.analysis.inaccuracy, players.white.analysis.acpl)
    
    colnames(error_rates_w) <- c("player", "rating", "moves", "blunders", "mistakes", "inaccuracies", "acpl",
                                 "opp_blunders", "opp_mistakes", "opp_inaccuracies", "opp_acpl")
    colnames(error_rates_b) <- c("player", "rating", "moves", "blunders", "mistakes", "inaccuracies", "acpl",
                                 "opp_blunders", "opp_mistakes", "opp_inaccuracies", "opp_acpl")
    
    error_rates <- rbind(error_rates_w, error_rates_b)
    rm(error_rates_w, error_rates_b)
    
    error_rates <- error_rates %>% 
      mutate(cpl = acpl * moves,
             opp_cpl = opp_acpl * moves)
    
    error_rates$rating_group <- cut(error_rates$rating, 
                                    breaks=c(800, 1000, 1200, 1400, 1600, 1800, 2000,
                                             2200, 2400, 2600),
                                    labels = c("800-1000", "1000-1200",
                                               "1200-1400", "1400-1600", "1600-1800",
                                               "1800-2000", "2000-2200",
                                               "2200-2400", "2400-2600"),
                                    include.lowest=TRUE, dig.lab = 4)
    
    error_rates_grouped <- error_rates %>% 
      group_by(rating_group) %>% 
      summarise(games = n(),
                blunder_rate = sum(blunders, na.rm = T) / sum(moves, na.rm = T),
                mistake_rate = sum(mistakes, na.rm = T) / sum(moves, na.rm = T),
                inaccuracy_rate = sum(inaccuracies, na.rm = T) / sum(moves, na.rm = T),
                error_rate = blunder_rate + mistake_rate + inaccuracy_rate,
                acpl = sum(cpl, na.rm = T) / sum(moves, na.rm = T)) %>% 
      mutate(blunder_rate = round(blunder_rate * 100, 1),
             error_rate = round(error_rate * 100, 1),
             acpl = round(acpl, 1)) %>% 
      select(rating_group, games, blunder_rate, error_rate, acpl) %>% 
      arrange(desc(rating_group)) %>% 
      mutate(rating_group2 = paste0(rating_group, " (", games, ")"))
    
    # Plot ACPL by rating band
    plt_acpl <- ggplot(error_rates_grouped, aes(acpl, rating_group2, 
                                                label = round(acpl))) +
      geom_segment(aes(x = 0, y = rating_group2, xend = acpl, yend = rating_group2), colour = "grey50") +
      geom_point(colour = league_col, size = 10) +
      theme_cowplot() + 
      labs(y = "Rating band", x = "ACPL", title = "ACPL by rating band") +
      geom_text(colour = "white", size = 4) +
      theme(axis.title.y = element_blank()) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Plot blunder rates by board
    plt_blunder_rates <- ggplot(error_rates_grouped, aes(blunder_rate, rating_group2, 
                                                         label = scales::percent(blunder_rate/100, accuracy = 1))) +
      geom_segment(aes(x = 0, y = rating_group2, xend = blunder_rate, yend = rating_group2), colour = "grey50") +
      geom_point(colour = league_col, size = 10) +
      theme_cowplot() + 
      labs(y = "Rating band", x = "Blunder rate (%)", title = "Blunder rate by rating band") +
      # scale_x_continuous(breaks = integer_breaks()) +
      geom_text(colour = "white", size = 4) +
      theme(axis.title.y = element_blank()) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  return(list(plt_acpl, plt_blunder_rates, error_rates))
  
}


LowestACPLGames <- function(league, games, tos_violators){
  if(league == "team4545"){
    # Show games with lowest combined ACPL by board
    lowest_acpl_games_all <- games %>% 
      filter(!(is.na(board))) %>% 
      select(id, board, white, black, players.white.analysis.acpl, players.black.analysis.acpl, result, started) %>% 
      filter(!(str_to_lower(white) %in% str_to_lower(tos_violators))) %>% # remove ToS violators (White)
      filter(!(str_to_lower(black) %in% str_to_lower(tos_violators))) %>% # remove ToS violators (Black)
      mutate(sum_acpl = players.white.analysis.acpl + players.black.analysis.acpl,
             url = paste0("https://lichess.org/", id),
             players = paste0(white, " - ", black)) %>%
      mutate(date = sprintf('%s %2d %2s %2d', lubridate::wday(started, label=T, abbr=T), lubridate::day(started), lubridate::month(started, label = T, abbr = T), lubridate::year(started))) %>% 
      select(board, players, url, result, sum_acpl, date) %>% 
      group_by(board) %>% 
      slice_min(sum_acpl) 
  } else {
    # Order games by lowest combined ACPL
    lowest_acpl_games_all <- games %>% 
      select(id, white, black, players.white.analysis.acpl, players.black.analysis.acpl, result, started) %>% 
      filter(!(str_to_lower(white) %in% str_to_lower(tos_violators))) %>% # remove ToS violators (White)
      filter(!(str_to_lower(black) %in% str_to_lower(tos_violators))) %>% # remove ToS violators (Black)
      mutate(sum_acpl = players.white.analysis.acpl + players.black.analysis.acpl,
             url = paste0("https://lichess.org/", id),
             players = paste0(white, " - ", black)) %>%
      mutate(date = sprintf('%s %2d %2s %2d', lubridate::wday(started, label=T, abbr=T), lubridate::day(started), lubridate::month(started, label = T, abbr = T), lubridate::year(started))) %>% 
      select(players, url, result, sum_acpl, date) %>% 
      slice_min(sum_acpl, n = 10) %>% 
      arrange(sum_acpl)
  }
  
  return(lowest_acpl_games_all)
  
}


LowestACPLGamesDecisive <- function(league, games, tos_violators) {
  if(league == "team4545"){
    lowest_acpl_games_decisive <- games %>% 
      filter(!(is.na(board))) %>% 
      filter(winner %in% c("white", "black")) %>% 
      select(id, board, white, black, players.white.analysis.acpl, players.black.analysis.acpl, result, started) %>% 
      filter(!(str_to_lower(white) %in% str_to_lower(tos_violators))) %>% # remove ToS violators (White)
      filter(!(str_to_lower(black) %in% str_to_lower(tos_violators))) %>% # remove ToS violators (Black)
      mutate(sum_acpl = players.white.analysis.acpl + players.black.analysis.acpl,
             url = paste0("https://lichess.org/", id),
             players = paste0(white, " - ", black)) %>%
      mutate(date = sprintf('%s %2d %2s %2d', lubridate::wday(started, label=T, abbr=T), lubridate::day(started), lubridate::month(started, label = T, abbr = T), lubridate::year(started))) %>% 
      select(board, players, url, result, sum_acpl, date) %>% 
      group_by(board) %>% 
      slice_min(sum_acpl) 
  } else {
    lowest_acpl_games_decisive <- games %>% 
      filter(winner %in% c("white", "black")) %>% 
      select(id, white, black, players.white.analysis.acpl, players.black.analysis.acpl, result, started) %>% 
      filter(!(str_to_lower(white) %in% str_to_lower(tos_violators))) %>% # remove ToS violators (White)
      filter(!(str_to_lower(black) %in% str_to_lower(tos_violators))) %>% # remove ToS violators (Black)
      mutate(sum_acpl = players.white.analysis.acpl + players.black.analysis.acpl,
             url = paste0("https://lichess.org/", id),
             players = paste0(white, " - ", black)) %>%
      mutate(date = sprintf('%s %2d %2s %2d', lubridate::wday(started, label=T, abbr=T), lubridate::day(started), lubridate::month(started, label = T, abbr = T), lubridate::year(started))) %>% 
      select(players, url, result, sum_acpl, date) %>% 
      slice_min(sum_acpl, n = 10) %>% 
      arrange(sum_acpl)
  }
  return(lowest_acpl_games_decisive)
}


PopularOpenings <- function(games = games){
  # Show openings by broad opening name (excl. variation)
  openings_exclvar <- games %>% 
    group_by(opening.broad) %>% 
    summarise(games = n(),
              mean_rating = mean(mean_rating),
              score_w = sum(score_w),
              white_wins = (sum(result == "1-0") / games) * 100,
              draws = (sum(result == "1/2-1/2") / games) * 100,
              black_wins = (sum(result == "0-1") / games) * 100,
              mean_eval_after_15 = mean(eval_after_15, na.rm = T) / 100) %>% 
    arrange(desc(games)) %>% 
    mutate(rank = dense_rank(desc(games))) %>% 
    mutate(perc_w = (score_w / games) * 100) %>%
    select(rank, opening.broad, games, perc_w, white_wins, draws, black_wins, 
           mean_eval_after_15, mean_rating)
  return(openings_exclvar)
}


PopularVariations <- function(games = games){
  # Show openings by Lichess-determined opening name including variation
  openings_inclvar <- games %>% 
    group_by(opening.name) %>% 
    summarise(games = n(),
              mean_rating = mean(mean_rating),
              score_w = sum(score_w),
              white_wins = (sum(result == "1-0") / games) * 100,
              draws = (sum(result == "1/2-1/2") / games) * 100,
              black_wins = (sum(result == "0-1") / games) * 100,
              mean_eval_after_15 = mean(eval_after_15, na.rm = T) / 100) %>% 
    arrange(desc(games)) %>% 
    mutate(rank = dense_rank(desc(games))) %>% 
    mutate(perc_w = (score_w / games)*100) %>%
    select(rank, opening.name, games, perc_w, white_wins, draws, black_wins, 
           mean_eval_after_15, mean_rating)
  return(openings_inclvar)
}


BestOpeningsForWhite <- function(games = games){
  # Same as (3) but ordered by White score instead
  openings_bestwhite <- games %>% 
    group_by(opening.broad) %>% 
    summarise(games = n(),
              mean_rating = mean(mean_rating),
              score_w = sum(score_w),
              white_wins = (sum(result == "1-0") / games) * 100,
              draws = (sum(result == "1/2-1/2") / games) * 100,
              black_wins = (sum(result == "0-1") / games) * 100,
              mean_eval_after_15 = mean(eval_after_15, na.rm = T) / 100) %>% 
    mutate(perc_w = (score_w / games) * 100) %>%
    arrange(desc(perc_w)) %>% 
    filter(games > 10) %>% 
    mutate(rank = dense_rank(desc(perc_w))) %>% 
    select(rank, opening.broad, games, perc_w, white_wins, draws, black_wins,
           mean_eval_after_15, mean_rating)
  return(openings_bestwhite)
}


BestOpeningsForBlack <- function(games = games){
  # Same ordered by Black score
  openings_bestblack <- games %>% 
    group_by(opening.broad) %>% 
    summarise(games = n(),
              mean_rating = mean(mean_rating),
              score_b = sum(score_b),
              white_wins = (sum(result == "1-0") / games) * 100,
              draws = (sum(result == "1/2-1/2") / games) * 100,
              black_wins = (sum(result == "0-1") / games) * 100,
              mean_eval_after_15 = mean(eval_after_15, na.rm = T) / 100) %>% 
    mutate(perc_b = (score_b / games)*100) %>% 
    arrange(desc(perc_b)) %>% 
    filter(games > 10) %>% 
    mutate(rank = dense_rank(desc(perc_b))) %>% 
    select(rank, opening.broad, games, perc_b, black_wins, draws, white_wins, 
           mean_eval_after_15, mean_rating)
  return(openings_bestblack)
}

Perfs <- function(pairings = pairings, fide_tpr_lookup = fide_tpr_lookup){
  
  perfs_w <- pairings %>% 
    filter(winner != "forfeit/unplayed") %>% 
    group_by(white) %>%
    summarise(sum_opp_rating = sum(rating_b),
              games = n(),
              points = ((sum(winner == "white") * 1) + (sum(winner == "draw") * 0.5)),
              wins = sum(winner == "white"),
              draws = sum(winner == "draw"),
              losses = sum(winner == "black"),
              initial_rating = rating_w[which.min(round)],
              initial_round = min(round)) %>% 
    rename("player" = white)
  
  perfs_b <- pairings %>% 
    filter(winner != "forfeit/unplayed") %>% 
    group_by(black) %>%
    summarise(sum_opp_rating = sum(rating_w),
              games = n(),
              points = ((sum(winner == "black") * 1) + (sum(winner == "draw") * 0.5)),
              wins = sum(winner == "black"),
              draws = sum(winner == "draw"),
              losses = sum(winner == "white"),
              initial_rating = rating_b[which.min(round)],
              initial_round = min(round)) %>% 
    rename("player" = black)
  
  perfs <- rbind(perfs_w, perfs_b)
  
  # Compute performance ratings using FIDE method
  perfs_fide <- perfs %>% 
    group_by(player) %>% 
    summarise(sum_opp_rating = sum(sum_opp_rating),
              games = sum(games),
              avg_opp_rating = sum_opp_rating / games,
              points = sum(points),
              tp_score = points / games,
              wins = sum(wins),
              draws = sum(draws),
              losses = sum(losses),
              initial_rating = initial_rating[which.min(initial_round)]) %>%
    mutate(adj = fide_tpr_lookup[match(round(tp_score, 2), fide_tpr_lookup$p),2]) %>% 
    mutate(perf_rating = avg_opp_rating + adj) %>% 
    mutate(perf_diff = perf_rating - initial_rating)
  
  # Save as all perfs df
  perfs <- perfs_fide %>% as_tibble()
  return(perfs)
}


TopScorers <- function(pairings, fide_tpr_lookup, tos_violators){
  perfs <- Perfs(pairings, fide_tpr_lookup)
  top_scorers <- perfs %>% 
    mutate(plus_score = wins - losses) %>% 
    filter(plus_score >= 5) %>% 
    arrange(desc(plus_score)) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>%  # remove ToS violators
    tibble::as_tibble()
  
  top_scorers_final <- top_scorers %>% 
    mutate(text = paste0("**", player, "**", " (", points, "/", games, ")")) %>% 
    select(player, text) %>% 
    tibble::as_tibble()
  
  aces <- top_scorers %>%
    filter(plus_score >= 6) %>% 
    arrange(desc(plus_score)) %>% 
    select(player) %>% 
    tibble::as_tibble()
  
  # if(nrow(aces) < 1){
  #   aces <- ""
  # }
  
  return(list(top_scorers_final$player, top_scorers_final$text, aces))
}

RelativePerfs <- function(pairings = pairings, min_games = 5, fide_tpr_lookup = fide_tpr_lookup, tos_violators = tos_violators){
  perfs <- Perfs(pairings, fide_tpr_lookup) %>% tibble::as_tibble()
  
  # Produce relative perfs for report
  relative_perfs <- perfs %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% # remove ToS violators
    filter(games >= min_games) %>% 
    filter(perf_diff >= 0) %>% 
    arrange(desc(perf_diff)) %>% 
    mutate(perf_rank = dense_rank(desc(perf_diff))) %>% 
    select(perf_rank, player, games, wins, draws, losses, initial_rating, perf_rating, perf_diff)
  return(list(perfs, relative_perfs))
}


LowestACPLs <- function(error_rates = error_rates, 
                        all_moves = all_moves, 
                        min_games, 
                        top_percentile_to_show, 
                        tos_violators = tos_violators){
  
  # Identify players with lowest season ACPLs
  # Based on CPLs in moves after ply 20 in undecided positions
  
  season_acpls_lichess <- error_rates %>%
    group_by(player) %>%
    summarise(season_acpl = sum(cpl, na.rm=T) / sum(moves, na.rm=T)) %>% 
    arrange(season_acpl)
  
  # Recalculated using all_moves
  
  lowest_acpls <- all_moves %>% 
    # exclude first 10 moves
    filter(ply > 19) %>%
    # # exclude 0 cpl moves made in 0 eval positions 
    filter(!((capped_eval_prev == 0) & (cpl == 0))) %>% 
    # # exclude non-errors played in decided positions
    filter(!(!(judgment %in% c("Mistake", "Blunder")) & (abs(capped_eval_prev) > 300))) %>% 
    group_by(player) %>% 
    summarise(acpl = mean(cpl),
              games = n_distinct(game_id)) %>% 
    filter(games >= min_games) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% # remove ToS violators
    mutate(rank = dense_rank(acpl)) %>% 
    arrange(acpl) %>% 
    mutate(prank = ntile(acpl, 100)) %>%
    filter(prank <= top_percentile_to_show) %>% 
    select(rank, player, games, acpl) %>%
    tibble::as_tibble()
  
  # Add Lichess ACPL-derived figures to the data - just for comparing the two calcs
  lowest_acpls <- dplyr::left_join(lowest_acpls, season_acpls_lichess,
                                   by = c("player"))
  
  return(lowest_acpls)
}


LowestACPLs960 <- function(error_rates = error_rates,
                           min_games,
                           top_percentile_to_show,
                           tos_violators = tos_violators){
  
  season_acpls_lichess <- error_rates %>%
    group_by(player) %>%
    summarise(games = n(),
              acpl = sum(cpl, na.rm=T) / sum(moves, na.rm=T)) %>% 
    arrange(acpl) %>% 
    filter(games >= min_games) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% # remove ToS violators
    mutate(rank = dense_rank(acpl)) %>% 
    mutate(prank = ntile(acpl, 100)) %>%
    filter(prank <= top_percentile_to_show) %>% 
    select(rank, player, games, acpl) %>%
    tibble::as_tibble()
  return(season_acpls_lichess)
}


SingleFigureACPLs <- function(games = games, tos_violators = tos_violators){
  minacpl_players_w <- games %>% 
    arrange(players.white.analysis.acpl) %>% 
    select(white, players.white.analysis.acpl,
           id, result) %>% 
    mutate(colour = "w")
  colnames(minacpl_players_w)[1:2] <- c("player", "acpl")
  
  minacpl_players_b <- games %>% 
    arrange(players.black.analysis.acpl) %>% 
    select(black, players.black.analysis.acpl, id, result) %>% 
    mutate(colour = "b")
  colnames(minacpl_players_b)[1:2] <- c("player", "acpl")
  
  minacpl_players <- rbind(minacpl_players_w, minacpl_players_b) %>% 
    arrange(acpl) %>% 
    select(player, acpl, id)
  
  rm(minacpl_players_w, minacpl_players_b)
  
  # Show players with multiple single-figure ACPL games
  minacpl_players <- minacpl_players %>%  
    filter(acpl < 10) %>% 
    group_by(player) %>% 
    tally() %>% 
    filter(n>1) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    tibble::as_tibble() %>% 
    arrange(desc(n)) %>% 
    slice_max(n)
  
  # if(nrow(minacpl_players > 0)){
  #   minacpl_players <- minacpl_players %>% 
  #     arrange(desc(n)) %>% 
  #     slice_max(n)
  # }
  return(minacpl_players)
}


MostTimeSpentPerMove <- function(games = games, min_games = 3, tos_violators = tos_violators){
  avg_think_times <- rbind(tibble("player" = games$white, "duration" = games$clock_used_after_move10_w, "moves" = games$num_moves - 10),
                           tibble("player" = games$black, "duration" = games$clock_used_after_move10_b, "moves" = games$num_moves - 10)) %>% 
    filter(moves > 0) %>% 
    group_by(player) %>% 
    summarise(time_spent_after_move10 = sum(duration, na.rm = T),
              moves = sum(moves, na.rm = T),
              games = n(),
              avg_time_spent_after_move10 = time_spent_after_move10 / moves) %>% 
    arrange(desc(avg_time_spent_after_move10)) %>% 
    select(player, games, moves, avg_time_spent_after_move10, time_spent_after_move10) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    filter(games >= min_games) %>% 
    mutate(prank = ntile(avg_time_spent_after_move10, 100)) %>%
    mutate(rank = dense_rank(desc(avg_time_spent_after_move10))) %>% 
    mutate(duration_print = sprintf('%2dm %2.1fs', lubridate::minute(seconds_to_period(avg_time_spent_after_move10)), lubridate::second(seconds_to_period(avg_time_spent_after_move10)))) %>% 
    select(rank, player, games, moves, duration_print, prank)
  return(avg_think_times)
} 


MostTimeSpentAcrossSeason <- function(games = games, players_to_show = 50, tos_violators = tos_violators){
  season_think <- rbind(tibble("player" = games$white, "duration" = games$duration_w, "moves" = games$num_moves),
                        tibble("player" = games$black, "duration" = games$duration_b, "moves" = games$num_moves)) %>% 
    group_by(player) %>% 
    summarise(total_duration = lubridate::seconds_to_period(sum(duration, na.rm = T)),
              total_moves = sum(moves, na.rm = T),
              games = n()) %>% 
    arrange(desc(total_duration)) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    mutate(rank = dense_rank(desc(total_duration))) %>% 
    mutate(duration_print = sprintf('%2dh %2dm %2ds',
                                    lubridate::hour(total_duration), 
                                    lubridate::minute(total_duration), 
                                    lubridate::second(total_duration))) %>% 
    select(rank, player, games, duration_print) %>% 
    head(players_to_show)
  return(season_think)
}


MostDraws <- function(pairings = pairings, tos_violators = tos_violators){
  drawers <- pairings %>% 
    filter(winner == "draw") %>% 
    select(white, black)
  
  drawers <- c(drawers$white, drawers$black)
  
  drawers <- tibble("player" = drawers) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    group_by(player) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n > 1) %>% 
    slice_max(n)
  return(drawers)
} 


Comebacks <- function(all_moves = all_moves, tos_violators){
  
  comebacks <- all_moves %>% 
    select(player, colour, game_id, winner, 
           "eval" = capped_eval) %>% 
    mutate(col = if_else(colour == "white", "w", "b"),
           eval = if_else(colour == "black", eval * -1, eval),
           player_score = case_when(
             colour == "white" & winner == "white" ~ 1,
             colour == "white" & winner == "black" ~ 0,
             colour == "white" & is.na(winner) ~ 0.5,
             colour == "black" & winner == "white" ~ 0,
             colour == "black" & winner == "black" ~ 1,
             colour == "black" & is.na(winner) ~ 0.5,
             TRUE ~ NA_real_
           )) %>% 
    group_by(player, game_id) %>% 
    summarise(pts = max(player_score),
              col = max(col),
              worst = min(eval)) %>% 
    select(player, "id" = game_id, col, pts, worst) %>% 
    filter(pts >= 0.5,
           worst < -300) %>% 
    mutate(
      cb_pts = case_when(
        pts == 0.5 & worst >= -600 ~ 0.3,
        pts == 0.5 & worst >= -1000 ~ 0.5,
        pts == 1 & worst >= -600 ~ 0.7,
        pts == 1 & worst >= -1000 ~ 1,
        TRUE ~ NA_real_
      )
    ) %>% 
    filter(cb_pts > 0) %>% 
    group_by(player) %>% 
    summarise(cb_total = sum(cb_pts, na.rm = T),
              cb_games = n(),
              wins_cat1 = sum(cb_pts == 1),
              wins_cat2 = sum(cb_pts == 0.7),
              draws_cat1 = sum(cb_pts == 0.5),
              draws_cat2 = sum(cb_pts == 0.3)
    ) %>% 
    arrange(desc(cb_total)) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    mutate(rank = dense_rank(desc(cb_total))) %>% 
    select(rank, player, cb_total, cb_games, wins_cat1, wins_cat2, draws_cat1, draws_cat2)
  return(comebacks)
}


Upsets <- function(games = games, min_rating_gap = 100, tos_violators){
  
  upsets <- games %>% 
    select(id, white, black, rating_w, rating_b, score_w, score_b) %>% 
    filter((score_w == 1 & score_b == 0 & rating_w <= (rating_b - min_rating_gap)) |
             (score_w == 0 & score_b == 1 & rating_b <= (rating_w - min_rating_gap))) %>% 
    mutate(player = ifelse(score_w == 1, white, black),
           rating_gap = abs(rating_w - rating_b),
           link = paste0("https://lichess.org/", id)) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    arrange(desc(rating_gap)) %>% 
    mutate(rank = dense_rank(desc(rating_gap))) %>% 
    select(rank, player, rating_gap, link)
              
  
  # Repeat upset specialists
  upset_specialists <- upsets %>% 
    group_by(player) %>% 
    summarise(upsets = n(),
              mean_rating_gap = mean(rating_gap),
              max_rating_gap = max(rating_gap),
              upset_pts = sum(rating_gap)) %>%
    filter(upsets > 1) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    arrange(desc(upset_pts)) %>% 
    mutate(rank = dense_rank(desc(upset_pts))) %>% 
    select(rank, player, upset_pts, upsets, mean_rating_gap, max_rating_gap)
  return(list(upsets, upset_specialists))
}


AllMoves <- function(games = games){
  # Get dataset of all moves in games data
  all_moves <- data.table::rbindlist(games$evals, fill = T) %>% 
    mutate(colour = case_when(
      ply %% 2 == 0 ~ "white",
      ply %% 2 == 1 ~ "black",
      TRUE ~ NA_character_
    )) %>% 
    as_tibble()
  
  # Redo the eval capping cos that seems to have failed somehow
  all_moves <- all_moves %>% 
    mutate(
      capped_eval = case_when(
        eval >= 1000 ~ 1000,
        eval <= -1000 ~ -1000,
        TRUE ~ eval
      )
    ) %>% 
    mutate(
      capped_eval_prev = case_when(
        eval_prev >= 1000 ~ 1000,
        eval_prev <= -1000 ~ -1000,
        TRUE ~ eval_prev
      )
    ) %>% mutate(
      cpl = case_when(
        ply %% 2 == 0 ~ pmax(capped_eval_prev - capped_eval, 0),
        ply %% 2 == 1 ~ pmax((capped_eval_prev - capped_eval) * -1, 0),
        TRUE ~ NA_real_
      )
    )
  
  # Add id, White, Black, winner (white, black, NA)
  games_basics <- games %>% select(id, white, black, winner, result) %>% 
    as_tibble()
  
  all_moves <- left_join(all_moves, games_basics, by = c("game_id" = "id")) %>% 
    mutate(player = ifelse(colour == "white", white, black)) %>% 
    group_by(player) %>% 
    add_tally() %>% 
    rename("player_total_moves" = n) %>% 
    mutate(player_outcome = case_when(
      colour == "white" & winner == "white" ~ "win",
      colour == "black" & winner == "black" ~ "win",
      TRUE ~ NA_character_
    )) %>% 
    # Get preceding position eval from the mover's perspective 
    mutate(player_eval_prev = case_when(
      colour == "white" ~ capped_eval_prev,
      colour == "black" ~ capped_eval_prev * -1,
      TRUE ~ NA_real_
    )) %>% 
    as_tibble()
  return(all_moves)
}


Instamovers <- function(all_moves = all_moves, min_instamoves = 6, tos_violators = tos_violators){
  instamovers <- all_moves %>% 
    filter(time_spent < 1)
  
  if(nrow(instamovers) > 0){
    instamovers <- instamovers %>% 
      select(game_id, ply, time_spent, white, black, result) %>% 
      mutate(player = if_else(ply %% 2 == 1, black, white)) %>% 
      group_by(player) %>% 
      summarise(instamoves = n()) %>% 
      arrange(desc(instamoves)) %>%
      filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
      filter(instamoves >= min_instamoves) %>% 
      mutate(rank = dense_rank(desc(instamoves))) %>% 
      select(rank, player, instamoves)
  } else {
    instamovers <- tibble("rank" = 1, "player" = "n/a", "instamoves" = "n/a")
  }
  return(instamovers)
}


LongestThinks <- function(all_moves = all_moves, rows_to_show = 100, tos_violators = tos_violators){
  # Longest movetimes (top 100)
  movetimes <- all_moves %>% 
    arrange(desc(time_spent)) %>% 
    mutate(url = paste0("https://lichess.org/", game_id, "#", ply+1)) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    mutate(rank = dense_rank(desc(time_spent))) %>% 
    select(rank, player, time_spent, colour, ply, url) %>%
    mutate(time_spent = lubridate::seconds_to_period(time_spent),
           move = ifelse(ply %% 2 == 0, ply/2+1, (ply/2)+0.5)) %>% 
    mutate(time_spent = sprintf('%2dm %2ds', lubridate::minute(time_spent), lubridate::second(time_spent))) %>% 
    mutate(colour = str_to_title(colour)) %>% 
    select(rank, player, time_spent, colour, move, url) %>% 
    head(rows_to_show) %>% 
    as_tibble()
  return(movetimes)
}


SavedByTheBell <- function(all_moves = all_moves, max_secs_left = 5, tos_violators = tos_violators){
  # Saved by the bell
  savedbythebell <- all_moves %>% 
    filter(time_left <= max_secs_left,
           player_outcome == "win") %>%
    select(player, game_id, white, black, time_left, "eval" = player_eval_prev) %>% 
    group_by(player, game_id) %>% 
    summarise(panic_moves = n(), min_time_left = min(time_left), worst_eval = min(eval)) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    filter(worst_eval <= 300) %>% # excludes players who played last-second moves in winning positions
    arrange(desc(panic_moves), min_time_left, worst_eval) %>% 
    mutate(url = paste0("https://lichess.org/", game_id)) %>% 
    select(player, panic_moves, min_time_left, url)
  return(savedbythebell)
}


IntimateWithIncrement <- function(all_moves = all_moves, timetrouble_threshold = 60, min_moves_played = 100, tos_violators = tos_violators){
  # Highest % of moves in time trouble (<60s)
  timetrouble <- all_moves %>% 
    filter(time_left < timetrouble_threshold) %>% 
    group_by(player) %>% 
    summarise(tt_moves = n(),
              moves = max(player_total_moves),
              tt_pc = tt_moves/moves) %>% 
    arrange(desc(tt_pc)) %>%
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    filter(moves >= min_moves_played) %>% # min threshold 100 moves
    mutate(rank = dense_rank(desc(tt_pc))) %>% 
    select(rank, player, moves, tt_moves, tt_pc)
  return(timetrouble)
}


DavidAward <- function(games = games, perfs = perfs, 
                       min_comb_opp_games = 30, 
                       min_comb_opp_score = 0.5, 
                       tos_violators = tos_violators){
  # David Award (suggested by Tranzoo)
  # For the player who faced the opponents with the highest collective score across
  # the season (excluding games against the player)
  # Criteria: opponents must have played over 20 games (against others)
  
  david <- games %>% 
    select(white, black, result, score_w, score_b) %>%
    left_join(perfs, by = c("white" = "player")) %>% 
    select(white, black, score_w, score_b, "games_w" = games, "points_w" = points) %>% 
    mutate(games_aw = games_w - 1, points_aw = points_w - score_w) %>% 
    left_join(perfs, by = c("black" = "player")) %>% 
    select(white, black, score_w, score_b, games_aw, points_aw, "games_b" = games, "points_b" = points) %>%
    mutate(games_ab = games_b - 1, points_ab = points_b - score_b) %>% 
    select(white, black, games_aw, games_ab, points_aw, points_ab)
  
  # Get list of players and games played over season, to apply a minimum games
  # played filter to the final list
  players_games <- rbind(tibble("player" = games$white, "id" = games$id),
                         tibble("player" = games$black, "id" = games$id))
  eligible_players <- players_games %>% 
    group_by(player) %>% 
    summarise(games = n()) %>% 
    filter(games >= 5) %>% 
    select(player) %>% 
    dplyr::pull()
  rm(players_games)
  
  
  david <- rbind(tibble("player" = david$white,
                        "opp_adjgames" = david$games_ab,
                        "opp_adjpoints" = david$points_ab),
                 tibble("player" = david$black,
                        "opp_adjgames" = david$games_aw,
                        "opp_adjpoints" = david$points_aw)) %>% 
    group_by(player) %>% 
    summarise(opp_games = sum(opp_adjgames),
              opp_points = sum(opp_adjpoints),
              opp_perc = opp_points / opp_games) %>% 
    filter(opp_games >= min_comb_opp_games) %>% 
    filter(opp_perc >= min_comb_opp_score) %>% 
    mutate(opp_perc = opp_perc * 100) %>% 
    arrange(desc(opp_perc)) %>%
    filter(player %in% eligible_players) %>%
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    mutate(rank = dense_rank(desc(opp_perc))) %>% 
    select(rank, player, opp_games, opp_points, opp_perc)
  return(david)
}


TeamStats <- function(games = games, positions = positions){
  team_data <- games %>% 
    select(season, round, board, id, white, black, result, num_moves, score_w, 
           score_b, team_w, team_b, gp_w, gp_b,
           "acpl_w" = players.white.analysis.acpl,
           "acpl_b" = players.white.analysis.acpl,
           "blunders_w" = players.white.analysis.blunder,
           "blunders_b" = players.black.analysis.blunder,
           duration, duration_w, duration_b, eval_after_15, evals,
           match
    ) %>% 
    # Games played with the wrong colours can't be assigned teams (currently)
    # Excluding these prevents the appearance of any NA rows in the final team data
    filter(!(is.na(team_w))) %>% 
    filter(!(is.na(team_b)))
  
  team_data_w <- tibble("team" = team_data$team_w, "player" = team_data$white,
                        "score" = team_data$score_w, "board" = team_data$board,
                        "acpl" = team_data$acpl_w, "blunders" = team_data$blunders_w,
                        "moves" = team_data$num_moves, "game_duration" = team_data$duration,
                        "player_duration" = team_data$duration_w,
                        "cpl" = team_data$acpl_w * team_data$num_moves)
  team_data_b <- tibble("team" = team_data$team_b, "player" = team_data$black,
                        "score" = team_data$score_b, "board" = team_data$board,
                        "acpl" = team_data$acpl_b, "blunders" = team_data$blunders_b,
                        "moves" = team_data$num_moves, "game_duration" = team_data$duration,
                        "player_duration" = team_data$duration_b,
                        "cpl" = team_data$acpl_w * team_data$num_moves)
  
  team_data <- rbind(team_data_w, team_data_b) %>% 
    group_by(team) %>% 
    summarise(players = n_distinct(player),
              games = n(),
              pts_nonforf = sum(score, na.rm = T),
              wins = sum(score == 1, na.rm = T),
              draws = sum(score == 0.5, na.rm = T),
              losses = sum(score == 0, na.rm = T),
              acpl = sum(cpl, na.rm = T) / sum(moves, na.rm = T),
              avg_moves = mean(moves, na.rm = T),
              blunder_pc = sum(blunders, na.rm = T) / sum(moves, na.rm = T)
    )
  
  rm(team_data_w, team_data_b)
  
  # Add teams' final game points total (incl. forfeits/sched. draws etc) to team data
  team_points <- positions %>%
    filter(round == 8) %>% 
    select(rank, team, "points" = gp)
  
  team_data <- left_join(team_data, team_points, by = c("team"))
  
  team_data <- team_data %>% 
    mutate(blunder_pc = blunder_pc * 100) %>%
    arrange(rank) %>% 
    select(rank, team, games, points, pts_nonforf, wins, draws, losses, players,
           avg_moves, acpl, blunder_pc)
  
  # Get lowest team ACPL for awards
  team_accuracy_award <- team_data %>% 
    arrange(acpl) %>% 
    filter(!(is.na(team))) %>%
    slice_min(acpl) %>% 
    select(team, acpl)
  
  return(list(team_data, team_accuracy_award))
}


PlayersTeams <- function(games = games){
  # Gather data on all players and their teams
  players_teams <- rbind(tibble("player" = games$white, "team" = games$team_w, "round" = games$round),
                         tibble("player" = games$black, "team" = games$team_b, "round" = games$round)) %>% 
    group_by(player, team) %>% 
    summarise(games = n()) 
  return(players_teams)
}


EgalitarianAward <- function(players_teams = players_teams, perfs = perfs){
  # Silkthewander's suggestion
  # Award for the team with the lowest standard deviation of players' relative performance ratings over the season, 
  # where one's rel. perf rating = their actual perf. rating minus their initial rating
  
  # Construct variant dataset for obtaining relative perf ratings 
  players_teams_2 <- players_teams %>% 
    mutate(player = str_to_lower(player))
  
  # Compute team egalitarianism score
  egalitarian <- players_teams %>% 
    left_join(perfs, by = c("player")) %>% 
    group_by(team) %>% 
    summarise(egalite = sd(perf_diff, na.rm = T)) %>% 
    arrange(egalite) %>% 
    filter(!(is.na(team)))
  return(egalitarian)
}


AltAwards <- function(players_teams = players_teams, tos_violators = tos_violators){
  # Highlight alternates
  # Criteria: played for more than 1 team, or played only 1 game
  alts <- players_teams %>% 
    group_by(player) %>% 
    summarise(games = sum(games, na.rm = T),
              teams = n_distinct(team)) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>%
    filter(teams > 1 | games == 1) %>% 
    arrange(desc(teams)) %>% 
    mutate(rank = min_rank(-teams))
  
  # Identify player who played for the most teams
  alt_award <- alts %>% filter(rank == 1) 
  
  # Identify player(s) who played for the second-highest number of teams
  alt_runnerup <- alts %>% 
    filter(rank > 1) %>% 
    slice_max(teams)
  
  return(list(alt_award, alt_runnerup))
}


RookieAward <- function(all_games = all_games, games = games, perfs = perfs, min_games = 4, tos_violators = tos_violators){

  # Select rookie of the season
  
  assertthat::assert_that(is_tibble(all_games))
  assertthat::assert_that(is_tibble(games))
  assertthat::assert_that(is_tibble(perfs))
  assertthat::assert_that(is.character(tos_violators))
  
  # Identify players who previously played in the league
  veterans <- all_games %>% 
    filter(started < min(games$started)) %>% 
    select(white, black)
  
  veterans <- rbind(tibble("player" = veterans$white),
                    tibble("player" = veterans$black)) %>% 
    distinct(player) %>% 
    dplyr::pull() %>% 
    sort()
  
  rookie_perfs <- perfs %>% 
    filter(!(player %in% veterans)) %>%# only select rookies
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% # remove ToS violators
    filter(games >= min_games) %>% # apply lower games played threshold for rookie award
    filter(perf_diff >= 0) %>% 
    arrange(desc(perf_diff)) %>% 
    mutate(perf_rank = dense_rank(desc(perf_diff))) %>% 
    select(perf_rank, player, games, wins, draws, losses, initial_rating, perf_rating, perf_diff)
  
  return(rookie_perfs)
}


SeasonAwards <- function(league = NULL,
                         league_col_dark = NULL,
                         movetimes_exist = NULL,
                         minacpl_players = NULL, 
                         drawers = NULL, 
                         alt_award_list = NULL,
                         gambiteers = NULL,
                         relative_perfs = NULL,
                         lowest_acpls = NULL,
                         season_think = NULL,
                         comebacks = NULL,
                         upsets = NULL,
                         upset_specialists = NULL,
                         dawdlers = NULL,
                         instamovers = NULL,
                         movetimes = NULL,
                         timetrouble = NULL,
                         savedbythebell = NULL,
                         david = NULL,
                         rookie_perfs = NULL,
                         gambiteer_award = FALSE,
                         team_accuracy_award = NULL,
                         egalitarian = NULL,
                         aces = NULL,
                         marathonmovers = NULL,
                         needforspeed = NULL
                         ){
  
  # Show all eligible Primates of Precision (most games with sub-10 ACPL)
  if(nrow(minacpl_players) > 0){
    single_fig_acpl_players <- minacpl_players %>% select(player) %>% dplyr::pull()
    single_fig_acpl_games <- minacpl_players %>% select(n) %>% dplyr::pull()
    single_fig_acpl_games <- as.integer(single_fig_acpl_games[1])
    assertthat::is.number(single_fig_acpl_games)
  } else {
    single_fig_acpl_players <- "" # leave blank if no one's eligible
  }
  
  # Giri Award winners (and their number of draws)
  if(nrow(drawers) > 0){
    giri_winners <- tibble::tibble(drawers) %>% select(player) %>% dplyr::pull()
    giri_games <- tibble::tibble(drawers) %>% select(n) %>% dplyr::pull()
    giri_games <- as.integer(giri_games[1])
    assertthat::is.number(giri_games)
    giri_details <- paste0(giri_games, " draws")
  } else {
    giri_winners <- ""
    giri_details <- "No eligible players"
  }
  
  # Show all eligible Aces award winners 
  if(nrow(aces) > 0){
    aces <- str_c(sort(unlist(aces)), collapse = ", ")
  } else {
    aces <- "" # leave blank if no one's eligible
  }
  
  # Show Alt award winners for 4545 reports
  if(league == "team4545"){
    alt_winners <- alt_award_list[[1]] %>% select(player) %>% dplyr::pull() %>% str_c(collapse = ", ")
    alt_value <- alt_award_list[[1]] %>% select(teams) %>% dplyr::pull() %>% unique()
    alt_runnerup_names <- alt_award_list[[2]] %>% select(player) %>% dplyr::pull()
    alt_runnerup_value <- alt_award_list[[2]] %>% select(teams) %>% distinct() %>% dplyr::pull() 
  }
  
  # Identify correct Gambit Guru winner(s)
  # Properly account for ties
  if(gambiteer_award){
    # Winners
    gambiteer_winners <- gambiteers %>% 
      filter(rank == 1) %>% 
      arrange(player) %>% 
      select(player) %>% 
      dplyr::pull() %>% 
      str_c(collapse = ", ")
    # Hon. mentions
    gambiteer_mentions <- gambiteers %>% 
      filter(rank %in% c(2, 3)) %>% 
      arrange(player) %>% 
      select(player) %>% 
      dplyr::pull() %>% 
      str_c(collapse = ", ")
      
  } else {
    gambiteer_winners <- ""
    gambiteer_mentions <- ""
  }
  
  marathonmovers_winners <- marathonmovers %>% 
    filter(rank == 1) %>% 
    arrange(player)
  
  marathonmovers_mentions <- marathonmovers %>% 
    arrange(rank, player) %>% 
    filter(rank %in% c(2:3))
  
  if(movetimes_exist){
    needforspeed_winners <- needforspeed %>% 
      filter(rank == 1) %>% 
      arrange(player) %>% 
      select(player) %>% 
      dplyr::pull() %>% 
      str_c(collapse = ", ")
    
    needforspeed_mentions <- needforspeed %>% 
      filter(rank %in% c(2:3)) %>% 
      arrange(player) %>% 
      select(player) %>% 
      dplyr::pull() %>% 
      str_c(collapse = ", ")
    
  } else {
    needforspeed_winners <- ""
    needforspeed_mentions <- ""
  }
    
  
  
  # All award names
  award_names_lw <- c("Gambit Guru", "MVP Award", "Archbishop of Accuracy",
                      "Primates of Precision", "Tetrarch of Time", "Giri Award",
                      "Houdini Award", "Tarjan Award", "Slingshot Specialist",
                      "Grischuk's Cousin", "Bullet Boss", "Musing or Snoozing",
                      "Intimate with Increment", "Saved by the Bell", 
                      "David Award", "Rookie Award", "Aces", "Marathon Mover", "Need for Speed")
  award_names_team <- c(award_names_lw, "Awesome Alt", "Team Accuracy", 
                        "Egalitarian Award")
  award_names_960 <- award_names_lw[c(2:8,10,12:13,16:19)]

  
  # All award descriptions
  award_definitions_lw <- c("Best score playing gambits", 
                            "Best performance rating relative to initial rating",
                            "Lowest ACPL in undecided positions after move 10", 
                            "Most games < 10 ACPL",
                            "Spent the most clock time across the season", 
                            "Most draws (> 1)",
                            "Biggest comebacks", 
                            "Achieved the biggest upset",
                            "Achieved the most upsets across the season",
                            "Least time remaining after 10th move, and still winning",
                            "Most moves made in under 0.5s",
                            "Longest time spent on a move",
                            "Highest percentage of moves played with < 1 min left",
                            "Most moves made with < 5s left before winning",
                            "Faced the strongest opposition",
                            "Best relative perf. by a new league player",
                            "Scored +6 or better",
                            "Most moves played",
                            "Most wins/draws while gaining clock time")
  award_definitions_team <- c(award_definitions_lw, "Played for the most teams during the season", 
                              "Lowest team ACPL", "Team with the lowest SD across players' relative perfs")
  award_definitions_960 <- award_definitions_lw[c(2:8,10,12:13,16:19)]

  
  # Identify all award winners to show in reports
  winners_lw <- c(gambiteer_winners,
                  relative_perfs$player[1],
                  lowest_acpls$player[1],
                  str_c(single_fig_acpl_players, collapse = ", "),
                  ifelse(movetimes_exist, season_think$player[1], ""),
                  ifelse(nrow(drawers) > 0, str_c(giri_winners, collapse = ", "), giri_winners),
                  comebacks$player[1],
                  upsets$player[1],
                  upset_specialists$player[1],
                  ifelse(movetimes_exist, dawdlers$player[1], ""),
                  ifelse(movetimes_exist, instamovers$player[1], ""),
                  ifelse(movetimes_exist, movetimes$player[1], ""),
                  ifelse(movetimes_exist, timetrouble$player[1], ""),
                  ifelse(movetimes_exist, savedbythebell$player[1], ""),
                  david$player[1],
                  ifelse(nrow(rookie_perfs) > 0, rookie_perfs$player[1], ""),
                  aces,
                  str_c(marathonmovers_winners$player, collapse = ", "),
                  needforspeed_winners 
  )
  winners_960 <- winners_lw[c(2:8,10,12:13,16:19)]
  
  
  # All award details
  details_lw <- c(ifelse(gambiteer_award, paste0("Scored ", round(gambiteers$success[1]), "% from ", gambiteers$games[1], " gambit games"), "Can't be awarded"),
                  ifelse(nrow(relative_perfs) > 0, paste0("+", relative_perfs$wins[1], "-", relative_perfs$losses[1], "=", relative_perfs$draws[1], " perf ", round(relative_perfs$perf_rating[1]), ", initially ", round(relative_perfs$initial_rating[1])), ""),
                  paste0("Achieved a season ACPL of ", round(lowest_acpls$acpl[1], 1)),
                  ifelse(nrow(minacpl_players) > 0, paste0(single_fig_acpl_games[1], " games"), "No eligible players"),
                  ifelse(movetimes_exist, paste0(season_think$duration_print[1]), "Can't be awarded"),
                  paste0(giri_details),
                  paste0("Achieved ", comebacks$cb_games[1], " notable comeback wins/draws"),
                  paste0(upsets$link[1], " (rating gap: ", upsets$rating_gap[1], ")"),
                  paste0(upset_specialists$upsets[1], " upsets (", "biggest rating gap: ", upset_specialists$max_rating_gap[1], ")"),
                  ifelse(movetimes_exist, paste0("Had ", dawdlers$time_print[1], " left after 10 moves"), "Can't be awarded"),
                  ifelse(movetimes_exist, paste0(instamovers$instamoves[1], " insta-moves"), "Can't be awarded"),
                  ifelse(movetimes_exist, paste0("Spent ", movetimes$time_spent[1]), "Can't be awarded"),
                  ifelse(movetimes_exist, paste0(round(timetrouble$tt_pc[1]*100, 1), "%"), "Can't be awarded"),
                  ifelse(movetimes_exist, paste0(savedbythebell$panic_moves[1], " move(s) in ", savedbythebell$url[1]), "Can't be awarded"),
                  paste0("Opponents' record: ", david$opp_points[1], "/", david$opp_games[1]),
                  ifelse(nrow(rookie_perfs) > 0, paste0("+", rookie_perfs$wins[1], "-", rookie_perfs$losses[1], "=", rookie_perfs$draws[1], " perf ", round(rookie_perfs$perf_rating[1]), ", initially ", round(rookie_perfs$initial_rating[1])), ""),
                  ifelse(aces != "", "", "No eligible players"),
                  paste0(marathonmovers_winners$moves[1], " moves"),
                  paste0(needforspeed$games[1], " time-gaining games")
                  )
  
  details_960 <- details_lw[c(2:8,10,12:13,16:19)]
  
  
  # Honourable mentions (2nd and 3rd ranked players)
  mentions_lw <- c(gambiteer_mentions,
                   str_c(relative_perfs$player[2:3], collapse = ", "),
                   str_c(lowest_acpls$player[2:3], collapse = ", "),
                   "",
                   ifelse(movetimes_exist, str_c(season_think$player[2:3], collapse = ", "), ""),
                   "",
                   str_c(comebacks$player[2:3], collapse = ", "),
                   str_c(upsets$player[2:3], collapse = ", "),
                   str_c(upset_specialists$player[2:3], collapse = ", "),
                   ifelse(movetimes_exist, str_c(dawdlers$player[2:3], collapse = ", "), ""),
                   ifelse(movetimes_exist, str_c(instamovers$player[2:3], collapse = ", "), ""),
                   ifelse(movetimes_exist, str_c(movetimes$player[2:3], collapse = ", "), ""),
                   ifelse(movetimes_exist, str_c(timetrouble$player[2:3], collapse = ", "), ""),
                   ifelse(movetimes_exist, str_c(savedbythebell$player[2:3], collapse = ", "), ""),
                   str_c(david$player[2:3], collapse = ", "),
                   ifelse(nrow(rookie_perfs) >= 3, str_c(rookie_perfs$player[2:3], collapse = ", "), ""),
                   "",
                   str_c(marathonmovers_mentions$player, collapse = ", "),
                   needforspeed_mentions
                   )
  mentions_960 <- mentions_lw[c(2:8,10,12:13,16:19)]
  
  # Construct awards table
  if(league == "team4545"){
    
    # 4545 awards  
    winners_team <- c(winners_lw, str_c(alt_winners, collapse = ", "),
                      team_accuracy_award$team[1], egalitarian$team[1])
    
    details_team <- c(details_lw, paste0("Played for ", alt_value, " different teams"),
                      paste0("Team ACPL: ", round(team_accuracy_award$acpl[1], 1)),
                      paste0("SD: ", round(egalitarian$egalite[1], 1)))
    
    mentions_team <- c(mentions_lw, 
                       paste0(str_c(alt_runnerup_names, collapse = ", "), " (", alt_runnerup_value, ")"),
                       "", "")
    
    awards <- tibble(
      "Image" = c(rep("x", length(award_names_team))),
      "Award" = award_names_team,
      "Definition" = award_definitions_team, 
      "Winner" =winners_team,
      "Details" = details_team,
      "Mentions" = mentions_team
    )
    
  } else if (league == "lonewolf"){
    
    # LW awards
    awards <- tibble(
      "Image" = c(rep("x", length(award_names_lw))),
      "Award" = award_names_lw,
      "Definition" = award_definitions_lw, 
      "Winner" = winners_lw,
      "Details" = details_lw,
      "Mentions" = mentions_lw
    )
    
  } else if(league == "chess960"){
    
    # 960 awards
    awards <- tibble(
      "Image" = c(rep("x", length(award_names_960))),
      "Award" = award_names_960,
      "Definition" = award_definitions_960, 
      "Winner" = winners_960,
      "Details" = details_960,
      "Mentions" = mentions_960
    )
  }
  
  # Re-arrange columns for final presentation
  awards <- awards %>% 
    select(Image, Award, Definition, Winner, Details, Mentions) %>% 
    arrange(Award) %>% 
    filter(!(is.na(Award)))
  
  return(awards)
}


PieceCheckmates <- function(games = games){
  # Identify checkmates: all mates, pawn mates, and knight mates
  mates <- tibble("id" = games$id,
                  "mates" = rep(NA, nrow(games)))
  
  for(i in seq(1:nrow(games))){
    mates$mates[i] <- str_extract(games$pgn[i], "\\s[:alnum:]+(?<!\\s)#") %>% str_squish()
  }
  
  mates <- mates %>% 
    filter(!(is.na(mates))) %>% 
    mutate(mating_piece = str_sub(mates, 1, 1))
  
  all_mates <- mates %>% 
    mutate(status = replace(mating_piece, mating_piece %in% letters[1:8], "P")) %>%
    rename("piece" = status) %>%
    group_by(piece) %>% 
    tally() %>% 
    arrange(desc(n))
  
  # List all games with pawn mates
  pawn_mate_games <- mates %>% 
    filter(!(mating_piece %in% c("Q", "R", "B", "N"))) %>% 
    mutate(url = paste0("https://lichess.org/", id)) %>% 
    select(url) %>% 
    dplyr::pull()
  
  knight_mate_games <- mates %>% 
    filter(mating_piece == "N") %>% 
    mutate(url = paste0("https://lichess.org/", id)) %>% 
    select(url) %>% 
    dplyr::pull()
  
  return(list(all_mates, pawn_mate_games, knight_mate_games))
}


Promotions <- function(games = games){
  # Get stats about promotions, most promotions, under-promotions...
  promotions <- tibble("url" = paste0("https://lichess.org/", games$id),
                       "promotions" = rep(0, nrow(games)),
                       "promotion_movetext" = rep(NA, nrow(games)))
  
  promotion_moves <- str_extract_all(games$pgn, "[:alnum:]+(?<!\\s)=[:alnum:]")
  
  for(i in seq(1:nrow(games))){
    promotions$promotions[i] <- length(promotion_moves[[i]])
    if(promotions$promotions[i] == 1){
      promotions$promotion_movetext[i] <- promotion_moves[[i]]
    }
    if(promotions$promotions[i] > 1){
      promotions$promotion_movetext[i] <- str_c(promotion_moves[[i]], collapse = " ")
    }
  }
  
  promotions <- promotions %>% 
    filter(promotions > 0) %>% 
    arrange(desc(promotions))
  
  underpromotions <- promotions %>% 
    filter(str_detect(promotion_movetext, "N|B|R"))
  
  most_promotions <- promotions %>% 
    slice_max(promotions) %>% 
    select(url) %>% 
    dplyr::pull()
  
  return(list(promotions, underpromotions, most_promotions))
}


RollercoasterGames <- function(all_moves = all_moves, yoyo_threshold = 0.75){
  # Identify how many games were "rollercoaster" games
  # Where both sides had winning chances > yoyo_threshold (eg 75%), based on Lichess's charting scale
  # So not as robust as using SF 12/14 WDL percentages (but IMO those scales are too extreme to be informative)
  yoyos <- all_moves %>% 
    group_by(game_id) %>% 
    summarise(yoyo = ifelse(max(eval_scaled) >= yoyo_threshold, ifelse(min(eval_scaled) <= -yoyo_threshold, TRUE, FALSE), FALSE)) %>% 
    filter(yoyo == TRUE) %>% 
    nrow()
  return(yoyos)
}


ConsecutiveChecksAndCaptures <- function(games){
  # Find games with most consecutive checks and captures
  lst_checks <- list()
  lst_captures <- list()
  
  for(i in seq(1:nrow(games))){
    
    checks <- tibble(url = rep(paste0("https://lichess.org/", games$id[i]), games$num_moves[i]),
                     move_num = seq(1:games$num_moves[i]),
                     white_text = rep("", games$num_moves[i]),
                     black_text = rep("", games$num_moves[i]))
    
    movetext <- str_extract_all(games$moves[i], "[:alpha:]+[:graph:]+")
    
    captures <- tibble(url = rep(paste0("https://lichess.org/", games$id[i]), length(movetext[[1]])),
                       ply_num = seq(1:length(movetext[[1]])),
                       text = movetext[[1]])
    
    if(length(movetext[[1]]) %% 2 == 1){movetext[[1]] <- c(movetext[[1]], NA)}
    
    checks$white_text <- movetext[[1]][c(T,F)]
    checks$black_text <- movetext[[1]][c(F,T)]
    
    lst_checks[[i]] <- checks
    lst_captures[[i]] <- captures
  }
  
  checks <- rbindlist(lst_checks) %>% as_tibble()
  captures <- rbindlist(lst_captures) %>% as_tibble()
  
  checks$is_check_w <- ifelse(str_detect(checks$white_text, "\\+"), 1, 0)
  checks$is_check_b <- ifelse(str_detect(checks$black_text, "\\+"), 1, 0)
  
  captures$is_capture <- ifelse(str_detect(captures$text, "x"), 1, 0)
  
  # Isolate checks by colour
  checks_w <- checks %>% 
    select(-c("black_text", "is_check_b")) %>% 
    rename("is_check" = "is_check_w")
  
  checks_b <- checks %>% 
    select(-c("white_text", "is_check_w")) %>% 
    rename("is_check" = "is_check_b")
  
  # For each colour set, find the games with the most consecutive checks
  
  # White
  check_streaks_w <- checks_w %>% 
    group_by(url) %>%
    do({tmp <- with(rle(.$is_check == T), lengths[values])
    data.frame(url = .$url, Max=if(length(tmp)==0) 0
               else max(tmp)) }) %>%
    slice(1L) %>%
    arrange(desc(Max)) %>%
    rename("streak" = Max) %>%
    as_tibble() %>%
    slice_max(streak)
  
  # Black
  check_streaks_b <- checks_b %>%
    group_by(url) %>%
    do({tmp <- with(rle(.$is_check == T), lengths[values])
    data.frame(url = .$url, Max=if(length(tmp)==0) 0
               else max(tmp)) }) %>%
    slice(1L) %>%
    arrange(desc(Max)) %>%
    rename("streak" = Max) %>%
    as_tibble() %>%
    slice_max(streak)
  
  # Captures
  capture_streaks <- captures %>%
    group_by(url) %>%
    do({tmp <- with(rle(.$is_capture == T), lengths[values])
    data.frame(url = .$url, Max=if(length(tmp)==0) 0
               else max(tmp)) }) %>%
    slice(1L) %>%
    arrange(desc(Max)) %>%
    rename("streak" = Max) %>%
    as_tibble() %>%
    slice_max(streak)
  
  # Combine to get games with most consecutive checks by White and Black 
  check_streaks <- bind_rows(check_streaks_w, check_streaks_b) %>% 
    arrange(desc(streak)) %>%
    slice_max(streak)
  
  capture_streaks <- capture_streaks %>% 
    arrange(desc(streak)) %>%
    slice_max(streak)
  
  check_streaks_games <- check_streaks %>% 
    select(url) %>% 
    dplyr::pull()
  
  capture_streaks_games <- capture_streaks %>% 
    select(url) %>% 
    dplyr::pull()
  
  # Find game with latest first capture
  latest_first_capture <- captures %>%
    filter(is_capture == 1) %>% 
    group_by(url) %>% 
    summarise(first_capture = min(ply_num)) %>% 
    slice_max(first_capture)
  
  game_latest_capture <- latest_first_capture %>% 
    select(url) %>% dplyr::pull()
  
  return(list(check_streaks, check_streaks_games, capture_streaks, 
              capture_streaks_games, latest_first_capture, game_latest_capture))
}

Dawdlers <- function(all_moves = all_moves, games = games, tos_violators = tos_violators,
                     games_to_show = 10){
  dawdlers <- all_moves %>% 
    filter(ply %in% c(18, 19)) %>%
    arrange(time_left) %>% 
    mutate(url = paste0("https://lichess.org/", game_id, "#", ply+1)) %>% 
    select(player, url, time_left, "outcome" = player_outcome) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    arrange(time_left) %>% 
    mutate(time_left_2 = lubridate::seconds_to_period(time_left)) %>% 
    mutate(time_print = sprintf('%2dm %2ds', lubridate::minute(time_left_2), lubridate::second(time_left_2))) %>% 
    filter(outcome == "win") %>% 
    head(games_to_show) %>% 
    mutate("rank" = dense_rank(time_left)) %>% 
    select(rank, player, time_print, outcome, url)
  return(dawdlers)
}


GetCheckmatePatterns <- function(path_scripts, data_path, league_load_label, season, games){
  # Load and run the Python script that identifies checkmate patterns 
  reticulate::source_python(paste0(path_scripts, "Python/checkmate_patterns.py"))
  mate_patterns <- IdentifyCheckmatePatterns(pgn_file =  paste0(data_path, 
                                                                "/games_noevals_", 
                                                                league_load_label, 
                                                                "_",
                                                                "s",
                                                                season,
                                                                ".pgn"))
  # Return identified data as tibble
  if(length(mate_patterns) > 0){
    mate_tibble <- data.table::rbindlist(mate_patterns) %>% 
      select("pattern" = "V1", "id" = "V2", "ply" = "V3")
  } else {
    cli::cli_inform("No games with notable checkmate pattterns were identified.")
    return("")
  }
  
  # Add other game details
  games_sub <- games %>% select(id, white, black, date)
  mate_tibble <- dplyr::left_join(mate_tibble,
                                  games_sub, 
                                  by = c("id"))
  mate_tibble <- mate_tibble %>% 
    mutate(link = paste0("https://lichess.org/", id, "#", ply)) %>% 
    mutate(winner = ifelse(as.numeric(ply) %% 2 == 1, white, black)) %>% 
    mutate(opp = ifelse(winner == white, black, white)) %>% 
    select(pattern, id, ply, winner, opp, date) %>% 
    arrange(pattern, date) %>% 
    tibble::as_tibble()
  mate_tibble$pattern[mate_tibble$pattern == "back_rank"] <- "back rank"
  
  # Obtain, annotate and save position images for each identified mate
  for (i in seq(1:nrow(mate_tibble))) {
    selected_id <- mate_tibble$id[i]
    selected_ply <- mate_tibble$ply[i]
    img <- magick::image_read( paste0("https://lichess1.org/game/export/gif/", selected_id, ".gif")) %>%
      image_scale("400x")
    img <- img[selected_ply + 1] # isolate final position
    img <- magick::image_crop(img, "+0+35") # crop to remove usernames/ratings
    img <- magick::image_crop(img, "+0-35")
    img <- magick::image_border(img, "white", "20x20") # add border for annotations
    # Add text with checkmate type 
    img <- image_annotate(img, 
                          text = stringr::str_to_title(mate_tibble$pattern[i]), 
                               location = "+20+0",
                               size = 15,
                               font = "sans",
                               weight = 700)
    # Add player details
    img <- image_annotate(img, 
                               text = paste0(mate_tibble$winner[i], " against ",
                                             mate_tibble$opp[i]),
                               location = "+20+420",
                               size = 15,
                               font = "sans",
                               weight = 400)
    # Save PNG
    image_write(img, path = paste0(here::here(), "/reports/images/mates/",
                                   league_load_label,
                                   "_s", 
                                   sprintf("%02d", season), 
                                   "_mate_", sprintf("%02d", i), ".png"),
                format = "png")
    cli::cli_alert_success("Saved {i}/{nrow(mate_tibble)} checkmate images")
  }
  
  # Once all individual images saved
  # Combine into single image
  final <- fs::dir_info(paste0(here::here(), "/reports/images/mates/")) %>% 
    filter(type == "file") %>% 
    filter(str_detect(path, "png")) %>% 
    filter(!(str_detect(path, "^mates_"))) %>% 
    filter(str_detect(path, paste0(league_load_label, 
                                   "_s", 
                                   sprintf("%02d", season),
                                   "_mate"
    )
    )) %>% 
    select(path) %>% 
    dplyr::pull() %>% 
    magick::image_read() %>% 
    magick::image_montage(geometry = geometry_area(width = 500, height = 500, x_off = 10, y_off = 10), 
                          tile = "5")
  # Save combined image
  image_write(final, path = paste0(here::here(), 
                                   "/reports/images/mates/",
                                   "mates_", 
                                   league_load_label, 
                                   "_s", 
                                   sprintf("%02d", season), 
                                   ".png"),
              format = "png")
  # return(mate_tibble)
}



TimeTurners <- function(moves = all_moves,
                        games = games,
                        league = league,
                        season = season,
                        tos_violators = tos_violators){
  
  # Time Turner award
  # For whoever wins or draws the most games longer than 15 moves where they
  # end up having gained time on the clock 
  
  if(league == "team4545"){timeleft_threshold <- 45 * 60}
  if(league == "lonewolf"){timeleft_threshold <- 30 * 60}
  if(league == "chess960"){
    timeleft_threshold <- ifelse(season <= 13, 15 * 60, 20 * 60)
    }
  
  games <- games %>% tibble() %>% 
    distinct(id, .keep_all = T)
  
  games_sub <- games %>% select(num_moves, id, result, moves) %>% 
    mutate(plies = stringr::str_count(moves, "[:graph:]+"))
  
  last_plies <- moves %>% 
    select(ply, time_left, game_id, player) %>% 
    group_by(game_id, player) %>% 
    summarise(last_ply = pmax(ply)) %>% 
    slice_max(last_ply) %>% 
    distinct(game_id, player, last_ply)
  
  moves_sub <- moves %>% 
    select(ply, game_id, player, time_left)
  
  timeturner <- left_join(last_plies, moves_sub, 
                          by = c("last_ply" = "ply", "game_id", "player")) %>% 
    mutate(last_ply = last_ply + 1) %>% 
    mutate(colour = ifelse(last_ply %% 2 == 0, "black", "white")) %>% 
    filter(time_left >= timeleft_threshold)
  
  # Only include wins and draws
  timeturners <- left_join(timeturner, games_sub, by = c("game_id" = "id")) %>% 
    select(-c(moves)) %>% 
    mutate(outcome = case_when(
      result == "1-0" & colour == "white" ~ "win",
      result == "0-1" & colour == "black" ~ "win",
      result == "1-0" & colour == "black" ~ "loss",
      result == "0-1" & colour == "white" ~ "loss",
      result == "1/2-1/2" ~ "draw",
      TRUE ~ NA_character_
    )) %>% 
    filter(outcome %in% c("win", "draw")) %>% # exclude losses
    filter(num_moves >= 15) %>% # exclude short wins/draws
    group_by(player) %>%
    summarise(games = n(),
              ids = str_c(game_id, collapse = ", "),
              max_left = max(time_left),
              avg_left = mean(time_left)) %>%
    arrange(desc(games), desc(max_left)) %>% 
    filter(games > 1) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    mutate(rank = data.table::frank(., -games, -max_left, ties.method = "min")) %>%
    arrange(rank) %>% 
    mutate(max_left = lubridate::seconds_to_period(max_left),
           avg_left = lubridate::seconds_to_period(avg_left)) %>% 
    select(rank, player, ids, games, max_left, avg_left)
  
  return(timeturners)
}



MostMovesPlayed <- function(games = games, 
                            rows_to_show = 100, 
                            tos_violators = tos_violators){
  # Marathon Moves award
  # For whoever plays the most moves in a season
  # Longstanding award. Suggested for new awards by TimothyHa
  
  msub <- games %>%
    select(id, white, black, moves) %>% 
    mutate(plies = stringr::str_count(moves, "[:graph:]+"),
           moves_w = ifelse(plies %% 2 == 0, plies/2, plies/2 + 0.5),
           moves_b = ifelse(plies %% 2 == 0, plies/2, plies/2 - 0.5))
  
  mostmoves <- rbind(
    tibble(id = msub$id, player = msub$white, moves = msub$moves_w),
    tibble(id = msub$id, player = msub$black, moves = msub$moves_b)
  ) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    summarise(moves = sum(moves)) %>% 
    filter(!(str_to_lower(player) %in% str_to_lower(tos_violators))) %>% 
    slice_max(order_by = moves, n = rows_to_show) %>% 
    mutate(rank = dense_rank(desc(moves))) %>%
    filter(rank <= 10) %>% 
    arrange(rank) %>% 
    select(rank, player, moves)
  
  return(mostmoves)
}

FirstMoves <- function(games = games, league = league){
  
  # Make chart showing first moves played in games split by board number or 
  # average rating band
  
  if(league == "team4545"){
    
    # For 4545, show first moves played by board
    firstmoves <- games %>%
      select(board, first_moves) %>% 
      group_by(board, first_moves) %>% 
      summarise(games = n()) %>% 
      arrange(desc(games)) %>% 
      tibble::as_tibble() %>% 
      ggplot(aes(fill = factor(first_moves, levels = c("e4 e5", "e4 c5", "e4 c6", "e4 e6",
                                                       "e4 d5", "e4 Nf6",
                                                       "d4 d5", "d4 Nf6", "d4 e6", "d4 d6",
                                                       "d4 f5", "d4 b6",
                                                       "Nf3 d5", "Nf3 Nf6", "Nf3 c5",
                                                       "c4 c5", "c4 Nf6", "c4 e5", "c4 c6")), 
                 y = games, 
                 x = board)) +
      geom_bar(position = "fill", stat = "identity") +
      theme_cowplot() +
      labs(x = "Board",
           y = "Proportion of games") +
      theme(legend.position = "right") +
      scale_x_continuous(breaks = seq(1:max(games$board))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = c("#002d9c", "#0043ce", "#0f62fe", "#4589ff", 
                                   "#78a9ff", "#a6c8ff",
                                   "#491d8b", "#6929c4", "#8a3ffc", "#a56eff", 
                                   "#be95ff", "#d4bbff",
                                   "#00539a", "#0072c3", "#1192e8",
                                   "#005d5d", "#007d79", "#009d9a", "#08bdba"), 
                        na.value = "#8C979A",
                        name = "First moves")
    
  } else if(league == "lonewolf"){
    
    # For LW, show first moves played by average rating band
    games$rating_group <- cut(games$mean_rating, 
                              breaks=c(800, 1000, 1200, 1400, 1600, 1800, 2000,
                                       2200, 2400, 2600),
                              labels = c("800-1000", "1000-1200",
                                         "1200-1400", "1400-1600", "1600-1800",
                                         "1800-2000", "2000-2200",
                                         "2200-2400", "2400-2600"),
                              include.lowest=TRUE, dig.lab = 4)
    
    firstmoves <- games %>%
      select(rating_group, first_moves) %>% 
      group_by(rating_group, first_moves) %>% 
      summarise(games = n()) %>% 
      arrange(desc(games)) %>% 
      tibble::as_tibble() %>% 
      ggplot(aes(fill = factor(first_moves, levels = c("e4 e5", "e4 c5", "e4 c6", "e4 e6",
                                                       "e4 d5", "e4 Nf6",
                                                       "d4 d5", "d4 Nf6", "d4 e6", "d4 d6",
                                                       "d4 f5", "d4 b6",
                                                       "Nf3 d5", "Nf3 Nf6", "Nf3 c5",
                                                       "c4 c5", "c4 Nf6", "c4 e5", "c4 c6")), 
                 y = games, 
                 x = rating_group)) +
      geom_bar(position = "fill", stat = "identity") +
      theme_cowplot() +
      labs(x = "Avg rating band",
           y = "Proportion of games") +
      theme(legend.position = "right") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = c("#002d9c", "#0043ce", "#0f62fe", "#4589ff", 
                                   "#78a9ff", "#a6c8ff",
                                   "#491d8b", "#6929c4", "#8a3ffc", "#a56eff", 
                                   "#be95ff", "#d4bbff",
                                   "#00539a", "#0072c3", "#1192e8",
                                   "#005d5d", "#007d79", "#009d9a", "#08bdba"), 
                        na.value = "#8C979A",
                        name = "First moves")
    
  }
  
  return(firstmoves)
}




# "I'm not trapped in here with you" award
# For whoever wins/plays the most games where they don't castle
# TODO