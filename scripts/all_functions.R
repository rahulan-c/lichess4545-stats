
# FUNCTIONS FOR WORKING WITH LICHESS/LICHESS4545 DATA

# Last updated: 2021-08-09

# All functions
# -------------

# get_league_games()
# get_user_games()
# tidy_lichess_games()
# get_league_data()
# integer_breaks()
# get_games_from_urls()
# save_season_data()
# report_season_stats()
# save_and_report_stats()
# prep_games_for_report() # INCOMPLETE
# make_sunburst_wrapper()
# move_sunburst()
# instareport_season()
# update_repo()
# wipe_all_stats()
# build_season_reports()
# build_alltime_stats()


# ---- User-defined parameters ------------------------------------------------

# Path to root folder
path_root <- "C:/Users/rahul/Documents/Github/rahulan-c.github.io/lichess4545-stats/"

# Path to Python environment
path_python <- "C:/Users/rahul/anaconda3/python.exe"


# Lichess API token
token <- Sys.getenv("LICHESS_TOKEN")
# token <- "my_token"


# ---- Directory paths (do not change) ----------------------------------------

# Directory where this functions script is saved
path_scripts <- paste0(path_root, "scripts/")

# Directory where 4545/LW season data is saved
path_savedata <- paste0(path_root, "data/")

# Directory where season stats R Markdown file is saved
path_loadrmd <- paste0(path_root, "reports/")

# R Markdown filenames
# RMD file that produces season stats
stats_rmd_filename <- "produce_season_stats"
# RMD file that produces all-time stats
alltime_stats_rmd_filename <- "alltime_records"

# Directory where season stats HTML reports will be saved
path_savereport <- paste0(path_root, "reports/")

# Directory where openings sunburst plots will be saved initially
path_sunburst_original <- path_scripts

# Directory where openings sunburst plots will be copied to
path_sunburst_new <- path_loadrmd


# ---- Required packages ------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, data.table, reactable, httr, jsonlite, xml2, 
               rvest, ndjson, reshape2, utf8, lubridate, tictoc, reticulate,
               rmarkdown, fs, stringi, git2r)


# ---- Functions --------------------------------------------------------------

#' Extract Lichess game data for Lichess4545 seasons
#' 
#' TBC
#' 
#' @param league_choice Lichess4545 league of choice - "team4545" for 4545 games, "lonewolf" for LoneWolf Open or U1800 games
#' @param seasons_choice Season(s) of choice
#' @param lw_u1800_choice TRUE if you want LW U1800 games, FALSE otherwise
#'
#' @return Data frame of season game data
#' @export
#'
#' @examples 
#' get_league_games("team4545", 25, FALSE)
#' get_league_games("lonewolf", 21, TRUE)
get_league_games <- function(league_choice, seasons_choice,
                             lw_u1800_choice){
  
  tic("Obtained game data")
  
  # Max number of game IDs allowed per Lichess API query
  max_ids_per_request <- 300
  
  all_data <- list()
  
  for(i in seq(1:length(seasons_choice))){
    
    # Lists to store season pairings data
    season_data <- list()
    
      
      # Extracting pairing data for 4545
      if(league_choice == "team4545"){
        
        r <- httr::GET(
          url = "https://www.lichess4545.com/api/get_season_games/",
          query = list(league = league_choice,
                       season = seasons_choice[[i]])
        )
        
        # Stop if there's an error
        if(r$status_code != 200){
          print("Error!")
          print(http_status(r)$message)}
        
        # Extract season data as tibble
        res <- r %>% 
          httr::content("text", encoding = stringi::stri_enc_detect(httr::content(r, "raw"))[[1]][1,1]) %>% 
          jsonlite::fromJSON() %>% 
          purrr::pluck("games")
        
        # Add returned game data to season game IDs list
        season_data[[i]] <- res
        
      }
      
      
      # Extracting pairings and game IDs for LoneWolf
      if(league_choice == "lonewolf"){
        
        # Amend season parameter for U1800 requests
        if(lw_u1800_choice){
          seasons_choice_u1800 <- paste0(seasons_choice[[i]], "u1800")
          
          r <- httr::GET(
            url = "https://www.lichess4545.com/api/get_season_games/",
            query = list(league = league_choice,
                         season = seasons_choice_u1800)
          )
          
        } else {
          
          r <- httr::GET(
            url = "https://www.lichess4545.com/api/get_season_games/",
            query = list(league = league_choice,
                         season = seasons_choice[[i]])
          )
          
        }
          
        # Stop if there's an error
        if(r$status_code != 200){
          print("Error!")
          print(http_status(r)$message)}
        
        # Extract season data as tibble
        res <- r %>% 
          httr::content("text", encoding = stringi::stri_enc_detect(httr::content(r, "raw"))[[1]][1,1]) %>% 
          jsonlite::fromJSON() %>% 
          purrr::pluck("games")
        
        # Add returned game data to season game IDs list
        season_data[[i]] <- res
        
        # # Add round game IDs to season game IDs list
        # season_ids[[j]] <- tibble("id" = round_ids)
        
        # print(paste0("Obtained game IDs for ", 
        #              ifelse(league_choice == "team4545", "4545 S",
        #                     ifelse(lw_u1800_choice, "LW U1800 S", "LW Open S")),
        #              seasons_choice[[i]]))
        
      }
    
    # print(paste0("Obtained all game IDs for ", 
    #              ifelse(league_choice == "team4545", "4545 S",
    #                     ifelse(lw_u1800_choice, "LW U1800 S", "LW Open S")),
    #              seasons_choice[[i]]))

    all_data[[i]] <- bind_rows(season_data)
    
  } # end season loop
  
  # Once all seasons have been checked...
  
  # Combine all game IDs, and split into list elements of size 300
  all_data <- bind_rows(all_data)
  all_ids <- all_data %>% 
    select(game_id) %>% 
    dplyr::pull()
  all_ids <- split(all_ids, ceiling(seq_along(all_ids)/max_ids_per_request))
  
  # Get data on requested games
  all_games <- list(rep(NA, length(all_ids)))
  
  for(l in seq(1:length(all_ids))){
    
    # Pause between batches of IDs
    if(l > 1){Sys.sleep(5)}
    
    batch_ids <- all_ids[[l]] %>% str_c(collapse = ",")
    
    query <- httr::POST(
      url = "https://lichess.org",
      path = paste0("/games/export/_ids"),
      body = batch_ids,
      query = list(moves = "true",
                   tags = "true",
                   clocks = "true",
                   evals = "true",
                   opening = "true",
                   pgnInJson = "true"),
      httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
      accept("application/x-ndjson"))
    
    # Stop if there's an error
    if(query$status_code != 200){
      print("Error!")
      print(http_status(query)$message)
      Sys.sleep(20)
      break}
    
    # Then convert the response into NDJSON
    game_data <- query %>% 
      httr::content("text", encoding = "UTF-8") %>% 
      read_lines() %>% 
      ndjson::flatten()
    
    all_games[[l]] <- game_data
    print(paste0("Obtained game data batch ", l, "/", length(all_ids)))
    
  }
  
  # Combine game data across batches into single df
  all_games <- bind_rows(all_games)
  
  print(paste0("Obtained all game data (", nrow(all_games), " games)"))
  
  toc(log = TRUE) # how long to get games data from Lichess
  
  # Inform user about unanalysed games for which the user should request Lichess analysis
  # Note: returned game IDs includes games < 3 moves (for which analysis isn't possible)
  need_analysis <- all_games %>% 
    filter(is.na(players.white.analysis.acpl))
  if(nrow(need_analysis) > 0){
    print("Some games haven't been analysed by Lichess...")
    print(need_analysis$id)
  }
  
  # Return Lichess game data on all game IDs in the Lichess4545 pairings data
  # and also return the Lichess4545 pairing data
  return(list(all_games, all_data))
  
}


# Get a user's Lichess games data
# Eg games <- get_user_games("izzie26", "2020-01-01", "Now", "blitz,rapid,classical")
get_user_games <- function(username, since, until, perfs){

  # username <- "izzie26"
  # since <- "2020-11-01"
  # until <- "Now"
  # perfs <- "bullet,blitz,rapid,classical"
  
  # Request games
  get_games <- httr::GET(
    url = "https://lichess.org",
    path = paste0("/api/games/user/", username),
    query = list(since = as.character(as.numeric(parse_date_time(since, "ymd")) * 1000),
                 until = until,
                 rated = "true",
                 perfType = perfs,
                 clocks = "true",
                 evals = "true",
                 opening = "true",
                 moves = "true",
                 pgnInJson = "true", 
                 tags = "true"),
    httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
    accept("application/x-ndjson"))
  
  # Convert response to data frame
  all_games <- get_games %>% 
    httr::content("text") %>% 
    read_lines() %>% 
    ndjson::flatten()
  
  return(all_games)
  
}


# Tidy Lichess games data
# Eg tidied_games <- tidy_lichess_games(games)
tidy_lichess_games <- function(games){

  tic("Tidied game data")
    
  # Exclude games that ended due to a) someone claiming a result due to their opponent 
  # disconnecting, b) cheat detection, or c) someone aborting the game
  # Also exclude games that started from custom positions
  games <- games %>% 
    filter(!(status %in% c("timeout", "cheat", "aborted"))) %>% 
    filter(perf != "fromPosition")
    
  # Only include analysed games
  games <- games %>% 
    filter(!(is.na(players.white.analysis.acpl))) %>% 
    filter(!(is.na(players.black.analysis.acpl)))
  
  # Add number of moves per game to data
  games$num_moves <- ifelse(str_count(games$moves, "\\s") %% 2 == 1,
                            (str_count(games$moves, "\\s") / 2) + 0.5,
                            (str_count(games$moves, "\\s") / 2) + 1)
  
  # Only include games with at least 3 moves
  games <- games %>% 
    filter(num_moves >= 3)
  
  # Add 'white' and 'black' fields to make it easier to refer to the players
  games <- games %>% 
    mutate(white = players.white.user.name,
           black = players.black.user.name)
  
  # Revise results field
  games <- games %>% 
    mutate(result = case_when(
      winner == "white" ~ "1-0",
      winner == "black" ~ "0-1",
      status == "draw" ~ "1/2-1/2",
      status == "stalemate" ~ "1/2-1/2",
      TRUE ~ NA_character_
    ))
  
  # Add game scores for both colours
  games <- games %>% 
    mutate(
      score_w = case_when(
        result == "1-0" ~ 1,
        result == "0-1" ~ 0,
        result == "1/2-1/2" ~ 0.5,
        TRUE ~ NA_real_
      )) %>% 
    mutate(
      score_b = case_when(
        result == "1-0" ~ 0,
        result == "0-1" ~ 1,
        result == "1/2-1/2" ~ 0.5,
        TRUE ~ NA_real_
      ))
  
  # Add average rating for each game
  games <- games %>% 
    mutate(rating_w = players.white.rating,
           rating_b = players.black.rating,
           mean_rating = (rating_w + rating_b) / 2)
  
  # Fix foreign characters in openings names
  games$opening.name <- games$opening.name %>% 
    str_replace_all("Ã¼", "ü") %>% 
    str_replace_all("Ã¶", "ö") %>% 
    str_replace_all("Ã³", "ó") %>% 
    str_replace_all("Ã©", "é")
  
  # Add broad opening names to data
  # Creates another column that takes the Lichess opening name but: 
  #   - removes variation names, ie any text after ":"
  #   - reduces any remaining names with "," to the stem opening
  #   - fixes remaining discrepancies, eg Guioco Piano amended to Italian Game
  games <- games %>% 
    mutate(opening.broad = str_replace_all(opening.name, "(?<=:).*$", "")) %>% 
    mutate(opening.broad = if_else(str_detect(opening.broad, ":"), str_replace(opening.broad, ":", ""), opening.broad)) %>% 
    mutate(opening.broad = str_replace_all(opening.broad, "(?<=,).*$", "")) %>% 
    mutate(opening.broad = if_else(str_detect(opening.broad, ","), str_replace(opening.broad, ",", ""), opening.broad)) %>% 
    mutate(opening.broad = if_else(opening.broad == "Guioco Piano", "Italian Game", opening.broad))
  
  # Make game creation times more readable
  games$started <- lubridate::as_datetime(games$createdAt / 1000)
  games$ended <- lubridate::as_datetime(games$lastMoveAt / 1000)
  
  # Add year, month, day of week and hour to data
  games$year <- lubridate::year(games$started)
  games$month <- lubridate::month(games$started, label = TRUE)
  games$day <- lubridate::day(games$started)
  games$wday <- lubridate::wday(games$started, label = TRUE, week_start = 1)
  games$hour <- lubridate::hour(games$started)
  games$week <- lubridate::week(games$started)
  # games$minute <- lubridate::minute(games$started)
  games$date <- lubridate::date(games$started)
  
  # Add first moves to games data
  games <- games %>% 
    mutate(first_moves = str_extract(moves, "^[:graph:]+\\s[:graph:]+")) %>% 
    mutate(first_move_w = str_extract(moves, "^[:graph:]+")) %>% 
    mutate(first_move_b = str_trim(str_extract(moves, "\\s[:graph:]+")))
  
  # Add move times to data
  
  # Compute move times
  times <- map(str_extract_all(games$pgn, "(?<=clk\\s)[0-1]\\:[0-5][0-9]\\:[0-5][0-9]"), lubridate::hms) %>% 
    map(lubridate::period_to_seconds) %>% 
    map2(games$clock.increment, ~ lag(.x, 2) - .x + .y)
  
  # Add id to times
  names(times) <- games$id
  
  # Compute time remaining after each move
  times_left <- map(str_extract_all(games$pgn, "(?<=clk\\s)[0-1]\\:[0-5][0-9]\\:[0-5][0-9]"), hms) %>% 
    map(period_to_seconds) %>% 
    map2(games$clock.increment, ~ .x - .y)
  
  # Add eval data
  # Isolate and melt eval data
  evals <- games %>% select(ends_with("eval"))
  names(evals) <- as.character(str_extract_all(names(evals), "[0-9]+"))
  evals$id <- games$id
  evals <- melt(evals, id.vars = c("id"))
  evals$variable <- as.numeric(as.character(evals$variable))
  
  # Isolate and melt mate data (ie moves until mate)
  mates <- games %>% select(ends_with("mate"))
  
  # Convert "mate in x moves" evals to cpls using the formula [x / abs(x)] * [300,000 - (abs(x) * 1000)]
  # mates <- mates %>% mutate_at(vars(ends_with("mate")), ~((./abs(.)) * (300000 - (abs(.)*1000))))
  # Alternative formula
  # Source: https://www.landonlehman.com/post/2021-01-25-how-to-reproduce-a-lichess-advantage-chart-in-python/)
  # Apparently this is used in python-chess
  mates <- mates %>% mutate_at(vars(ends_with("mate")), ~((./abs(.)) * (100 * (21 - pmin(abs(.), 10)))))
  
  names(mates) <- as.character(str_extract_all(names(mates), "[0-9]+"))
  mates$id <- games$id
  mates <- melt(mates)
  mates$variable <- as.numeric(as.character(mates$variable))
  
  # Combine eval and mates data and tidy
  evals <- bind_rows(evals, mates)
  rm(mates)
  evals <- na.omit(evals) # remove any rows with NA values
  colnames(evals)[2:3] <- c("ply", "eval")
  evals$ply <- as.numeric(as.character(evals$ply))
  evals$id <- as.character(evals$id)
  
  # Compute a scaled eval, for both plotting and statistical purposes
  # Maps evals onto a [-1, 1] scale
  # Based on scaling expression used in Lichess eval graphs
  # Source: https://github.com/ornicar/lila/blob/49705e68e2fc2e0c08c929dc96447d12c844108e/public/javascripts/chart/acpl.js
  evals <- evals %>% mutate(eval_scaled = 2 / (1 + exp(-0.004 * eval)) - 1) %>% 
    arrange(ply)
  
  
  # Extract info on individual inaccuracies, mistakes and blunders 
  judgments <- games %>% select(ends_with("judgment.name"))
  names(judgments) <- as.character(str_extract_all(names(judgments), "[0-9]+"))
  judgments$id <- games$id
  judgments <- melt(judgments,id.vars = c("id"), variable.name = "ply", value.name = "judgment")
  judgments$ply <- as.numeric(levels(judgments$ply))[judgments$ply]
  
  # Add judgments to evals data
  evals <- left_join(evals, judgments, by = c("id", "ply"))
  
  # Add initial times to times_left
  for(i in seq(1:nrow(games))){
    times_left[[i]][1] <- games$clock.initial[i]
    times_left[[i]][2] <- games$clock.initial[i]
  }
  
  # Add game duration (in seconds)
  # Calculated as the sum of both players' move times in the game
  # Not the same as time of last move minus time of creation
  games <- games %>% 
    mutate(duration_official = time_length(interval(games$started, games$ended), "second")) %>% 
    mutate(duration = map_dbl(times, ~ sum(.x,  na.rm = T))) %>%
    mutate(duration_w = map_dbl(times, ~ sum(.x[c(T,F)],  na.rm = T))) %>% 
    mutate(duration_b = map_dbl(times, ~ sum(.x[c(F,T)],  na.rm = T))) %>% 
    mutate(perc_total_clock_w = duration_w / (duration_w + duration_b),
           perc_total_clock_b = duration_b / (duration_w + duration_b)) %>% 
    mutate(clock_used_after_move10_w = map_dbl(times, ~ sum(.x[c(T,F)][-c(1:10)],  na.rm = T))) %>% 
    mutate(clock_used_after_move10_b = map_dbl(times, ~ sum(.x[c(F,T)][-c(1:10)],  na.rm = T)))
  
  # Add evals data
  
  # Nest evals data
  nested_evals <- evals %>% 
    mutate(game_id = id) %>% 
    group_by(id) %>% 
    nest()
  
  # Define function for adding movetimes and calculating CPLs
  add_movetimes_and_cpls <- function(nested_data, times, times_left){
    
    nested_data <- nested_data %>%
      # Add time spent per ply
      mutate(time_spent = times[1:nrow(.)]) %>% 
      # Add clock times left after each ply
      mutate(time_left = times_left[1:nrow(.)]) %>% 
      # Cap evals to +/- 1000 before calculating CPL
      mutate(capped_eval = ifelse(eval > 1000, 1000, eval)) %>% 
      mutate(capped_eval = ifelse(eval < -1000, -1000, eval)) %>% 
      # Then calculate CPLs per ply
      mutate(cpl = case_when(
        ply == 0 ~ 0,
        ply %% 2 == 0 ~ capped_eval - lag(capped_eval),
        ply %% 2 == 1 ~ (capped_eval - lag(capped_eval)) * -1,
        TRUE ~ NA_real_
    )) %>%
      mutate(cpl = cpl * -1) %>%
      # Add preceding position's eval
      mutate(eval_prev = lag(eval, 1)) %>%
      mutate(capped_eval_prev = lag(capped_eval, 1)) %>%
      # make any negative CPLs zero
      mutate(cpl = ifelse(cpl < 0, 0, cpl)) 
    
    return(nested_data)
  }
  
  # Apply this function to the evals data and add to games dataset 
  # using purrr::pmap() 
  games <- games %>% 
    mutate(evals = pmap(list(x = nested_evals$data, 
                              y = times, 
                              z = times_left),
                         ~ with(list(...),
                         add_movetimes_and_cpls(x, y, z)))) %>% 
    # Add eval after 15 moves to data
    mutate(eval_after_15 = unlist(map(evals, ~ .x$capped_eval[30])))
  
  # Remove unnecessary variables from data
  games <- games %>% 
    select(-starts_with("analysis")) %>% 
    select(-c(clock.totalTime, createdAt, lastMoveAt))
  
  toc(log = TRUE)
  
  return(games)

}


# Get 4545/LW season pairings and player/team positions data
# Eg 1 pairings <- get_league_data("team4545", 25, c(1:8), F, 10)[[1]]
# Eg 2 positions <- get_league_data("team4545", 25, c(1:8), F, 10)[[2]]
get_league_data <- function(league_choice, seasons_choice, rounds_choice, 
                            lw_u1800_choice, boards_per_team_choice){
  
  tic("Extracted pairing/rank data")
  
  # If only one round's games are required, repeat positions data extraction over the season to date
  all_pairings <- list()
  all_positions <- list()
  
  # For each selected season...
  for(i in seq(1:length(seasons_choice))){
    
    # For rounds 1:j+1 in each selected season...
    for(j in seq(1:(min(max(rounds_choice) + 1, ifelse(league_choice == "lonewolf", 11, 8))))){
      
      # Construct pairings URL
      url <- paste0("https://www.lichess4545.com/",
                    league_choice, 
                    "/season/", 
                    as.character(seasons_choice[[i]]), 
                    ifelse(lw_u1800_choice, "u1800", ""),
                    "/round/", 
                    as.character(j), "/pairings/")
      
      # 4545 code
      if(league_choice == "team4545"){
        
        
        # Get player data
        # Extract player pairings
        team_pairings <- read_html(url) %>%
          rvest::html_nodes("td") %>% 
          rvest::html_text() %>% 
          str_replace_all("[\r\n]" , "") %>% 
          str_squish() %>% 
          str_c()
        
        if(team_pairings[(length(team_pairings))] == "Unavailable"){
          team_pairings <- team_pairings[1:(length(team_pairings)- 2)]
        }
        
        # Identify players, result, and scheduled times; collate into tibble
        player1 = team_pairings[c(T, F, F, F)]
        result = team_pairings[c(F, T, F, F)]
        player2 = team_pairings[c(F, F, T, F)]
        sched_time = team_pairings[c(F, F, F, T)]
      
        team_pairings <- tibble(player1 = player1, player2 = player2, 
                                sched_time = sched_time,
                                result = result)
        # Add board numbers
        team_pairings$board <- rep(1:boards_per_team_choice, length.out = nrow(team_pairings))
        
        # Add season and round indicators
        team_pairings$season <- seasons_choice[[i]]
        team_pairings$round <- j
        
        # Add winners ("white", "black", "draw", "forfeit")
        team_pairings <- team_pairings %>%
          mutate(
            winner = case_when(
              board %% 2 == 1 & result == "10" ~ "white",
              board %% 2 == 1 & result == "01" ~ "black",
              board %% 2 == 0 & result == "10" ~ "black",
              board %% 2 == 0 & result == "01" ~ "white",
              board %% 2 == 1 & result == "½½" ~ "draw",
              str_detect(result, "F") | str_detect(result, "X") | str_detect(result, "Z") ~ "forfeit/unplayed",
              TRUE ~ "draw"
            ))
        
        
        # Extract player ratings
        team_pairings$rating1 <- team_pairings$player1 %>%
          str_extract_all("\\([:digit:]{3,4}\\)") %>% 
          str_replace_all("\\(", "") %>% 
          str_replace_all("\\)", "") %>% 
          as.integer()

        team_pairings$rating2 <- team_pairings$player2 %>%
          str_extract_all("\\([:digit:]{3,4}\\)") %>% 
          str_replace_all("\\(", "") %>% 
          str_replace_all("\\)", "") %>% 
          as.integer()
        
        # Clean up player names
        team_pairings$player1 <- team_pairings$player1 %>%
          str_replace_all("\\([:digit:]{3,4}\\)", "") %>% 
          str_squish() %>% 
          str_to_lower()
        
        team_pairings$player2 <- team_pairings$player2 %>% 
          str_replace_all("\\([:digit:]{3,4}\\)", "") %>% 
          str_squish() %>% 
          str_to_lower()
        
        # Identify each player's colour
        team_pairings <- team_pairings %>% 
          mutate(colour1 = case_when(
            board %% 2 == 1 ~ "white",
            board %% 2 == 0 ~ "black",
            TRUE ~ NA_character_
          )) %>% 
          mutate(colour2 = case_when(
            board %% 2 == 1 ~ "black",
            board %% 2 == 0 ~ "white",
            TRUE ~ NA_character_
          ))
        
        # Condense data to key variables
        team_pairings <- team_pairings %>% 
          mutate(white = if_else(colour1 == "white", player1, player2),
                 black = if_else(colour2 == "black", player2, player1),
                 rating_w = if_else(colour1 == "white", rating1, rating2),
                 rating_b = if_else(colour2 == "black", rating2, rating1)) %>% 
          select(season, round, board, white, rating_w, black, rating_b, result, winner, sched_time)
        
        
        # Get team position/points data
        # Extract team pairings
        teams <- read_html(url) %>%
          rvest::html_nodes("th") %>% 
          rvest::html_text() %>%
          str_squish() %>% 
          str_replace_all("Calendar" , "") %>% 
          str_replace_all("½", ".5")
        
        teams <- cbind.data.frame(split(teams, rep(1:5, times=length(teams)/5)), stringsAsFactors=F)
        names(teams) <- c("team_1", "score_1", "score_2", "team_2", "blank")
        teams <- teams[,1:4]
        
        # Add team scores
        teams$score_1 <- as.numeric(teams$score_1)
        teams$score_2 <- as.numeric(teams$score_2)
        
        # Now add players' team names and scores to pairings data
        team_pairings <- team_pairings %>% 
          mutate(team_w = rep(NA, nrow(.)),
                 team_b = rep(NA, nrow(.)),
                 gp_w = rep(NA, nrow(.)),
                 gp_b = rep(NA, nrow(.)))
        
        for(t in seq(1:(nrow(team_pairings)/boards_per_team_choice))){
          team_pairings$team_w[((t-1)*boards_per_team_choice+1):(boards_per_team_choice*t)] = rep(c(teams$team_1[t], teams$team_2[t]), times = boards_per_team_choice/2)
          team_pairings$team_b[((t-1)*boards_per_team_choice+1):(boards_per_team_choice*t)] = rep(c(teams$team_2[t], teams$team_1[t]), times = boards_per_team_choice/2)
          team_pairings$gp_w[((t-1)*boards_per_team_choice+1):(boards_per_team_choice*t)] = rep(c(teams$score_1[t], teams$score_2[t]), times = boards_per_team_choice/2)
          team_pairings$gp_b[((t-1)*boards_per_team_choice+1):(boards_per_team_choice*t)] = rep(c(teams$score_2[t], teams$score_1[t]), times = boards_per_team_choice/2)
          
        }
        
        # Now make sure the team names correspond correctly to each player's colour
        # Exploiting the fact that the left-named team is always White on Board 1
        team_pairings <- team_pairings %>% 
          mutate(team_res_w = case_when(
            gp_w > boards_per_team_choice / 2 ~ "win",
            gp_w  == boards_per_team_choice / 2 ~ "draw",
            gp_w < boards_per_team_choice / 2 ~ "loss",
            TRUE ~ NA_character_
          )) %>% 
          mutate(team_res_b = case_when(
            gp_b > boards_per_team_choice / 2 ~ "win",
            gp_b  == boards_per_team_choice / 2 ~ "draw",
            gp_b < boards_per_team_choice / 2 ~ "loss",
            TRUE ~ NA_character_
          )) %>% 
          # Add match identifier
          # NOT CORRECT ATM BECAUSE THE TEAM NAMES SWITCH AROUND SO IT'S NOT A 
          # UNIQUE MATCH IDENTIFIER - NEEDS TO BE FIXED
          # Update: this is corrected for in season_stats.Rmd but probably should
          # be fixed here instead.
          mutate(match = paste0(team_w, " vs ", team_b))
          
        
        # Now add game/match points to teams df
        teams <- teams %>% 
          mutate(mp_1 = case_when(
            score_1 > score_2 ~ 2,
            score_1 < score_2 ~ 0,
            score_1 == score_2 ~ 1,
            TRUE ~ NA_real_
          )) %>% 
          mutate(mp_2 = case_when(
            score_1 > score_2 ~ 0,
            score_1 < score_2 ~ 2,
            score_1 == score_2 ~ 1,
            TRUE ~ NA_real_
          ))
        
        teams_1 <- teams %>% 
          select("team" = team_1, 
                 "gp" = score_1, 
                 "mp" = mp_1)
        
        teams_2 <- teams %>% 
          select("team" = team_2, 
                 "gp" = score_2, 
                 "mp" = mp_2)
        
        teams_all <- rbind(teams_1, teams_2) %>% 
          mutate(round = j)
        
        
        # Add pairings data to the collated list
        all_pairings[[j]] <- team_pairings
        
        # Add team points data to the collated list
        all_positions[[j]] <- teams_all
        
        # print(paste0("Extracted pairings and positions for ", 
        #              ifelse(league_choice == "team4545", "4545 S",
        #                     ifelse(lw_u1800_choice, "LW U1800 S", "LW Open S")),
        #              seasons_choice[[i]], " R", j))
        
        
      }
      
      
      # Extract data on LoneWolf pairings
      if(league_choice == "lonewolf"){
        
        orig_results <- read_html(url) %>% 
          rvest::html_nodes("td") %>% 
          rvest::html_text() %>% 
          str_replace_all("[\r\n]" , "") %>% 
          str_squish() %>%
          str_c()
        
        results <- read_html(url) %>% 
          rvest::html_nodes("td") %>% 
          rvest::html_text() %>% 
          str_replace_all("[\r\n]" , "") %>% 
          str_squish() %>%
          str_c() 
        
        if(TRUE %in% str_detect(orig_results, "BYE")){
          
          # Locate first bye and delete rows after
          find_first_bye <- results %>% 
            str_detect("BYE") %>% 
            which()
          
          # Identify first player in pairings with a bye
          first_bye <- results[find_first_bye[1]-3] %>% 
            str_replace_all("\\([:digit:]{3,4}\\)", "")
          
          # Now remove all entries in original page after this first bye
          first_bye_loc <- results %>% str_which(first_bye)
          results <- results[1:(first_bye_loc-4)]
        
        }
        
        # Collect the data required to construct the pairings tibble                   
        board = results[c(T, F, F, F, F, F, F, F, F)] %>% as.integer()
        posw = results[c(F, T, F, F, F, F, F, F, F)] %>% as.integer()
        white = results[c(F, F, F, T, F, F, F, F, F)]
        posb = results[c(F, F, F, F, T, F, F, F, F)] %>% as.integer()
        black = results[c(F, F, F, F, F, F, T, F, F)]
        result = results[c(F, F, F, F, F, F, F, T, F)]
        date = results[c(F, F, F, F, F, F, F, F, T)]
        
        # Construct tibble
        results <- tibble(board = board, posw = posw, white = white, posb = posb,
                          black = black, result = result, date = date)
        
        # Extract ratings
        results$rating_w <- results$white %>% 
          str_extract_all("\\([:digit:]{3,4}\\)") %>% 
          str_replace_all("\\(", "") %>% 
          str_replace_all("\\)", "") %>% 
          as.integer()
        
        results$rating_b <- results$black %>% 
          str_extract_all("\\([:digit:]{3,4}\\)") %>% 
          str_replace_all("\\(", "") %>% 
          str_replace_all("\\)", "") %>% 
          as.integer()
        
        # Clean up player names
        results$white <- results$white %>% 
          str_replace_all("\\([:digit:]{3,4}\\)", "") %>% 
          str_squish() %>% 
          str_to_lower()
        
        results$black <- results$black %>% 
          str_replace_all("\\([:digit:]{3,4}\\)", "") %>% 
          str_squish() %>% 
          str_to_lower()
        
        # Add winners ("white", "black", "draw", "forfeit")
        results_without_byes <- results %>% 
          mutate(
            winner = case_when(
              result == "1-0" ~ "white",
              result == "0-1" ~ "black",
              str_detect(result, "F") | str_detect(result, "X") | str_detect(result, "Z") ~ "forfeit/unplayed",
              TRUE ~ "draw"
            ))
        
        # Add season and round numbers
        results_without_byes$season <- seasons_choice[[i]]
        results_without_byes$round <- j
        
        
        # Now extract players with byes
        if(TRUE %in% str_detect(orig_results, "BYE")){
        
          results <- read_html(url) %>% 
            rvest::html_nodes("td") %>% 
            rvest::html_text() %>% 
            str_replace_all("[\r\n]" , "") %>% 
            str_squish() %>%
            str_c() 
          
          # Locate first bye and delete rows after
          find_first_bye <- results %>% 
            str_detect("BYE") %>% 
            which()
          
          # Identify first player in pairings with a bye
          first_bye <- results[find_first_bye[1]-3] %>% 
            str_replace_all("\\([:digit:]{3,4}\\)", "")
          
          # Now remove all entries in original page after this first bye
          first_bye_loc <- results %>% str_which(first_bye)
          results <- results[(first_bye_loc-2):length(results)]
          
          # Collect the data required for players with byes                   
          pos = results[c(T, F, F, F, F, F, F, F, F)] %>% as.integer()
          player = results[c(F, F, T, F, F, F, F, F, F)]
          bye = results[c(F, F, F, F, F, T, F, F, F)]
          
          byes <- tibble(pos = pos, player = player, bye = bye)
          
          # Extract ratings
          byes$rating <- byes$player %>% 
            str_extract_all("\\([:digit:]{3,4}\\)") %>% 
            str_replace_all("\\(", "") %>% 
            str_replace_all("\\)", "") %>% 
            as.integer()
          
          # Clean up player names
          byes$player <- byes$player %>% 
            str_replace_all("\\([:digit:]{3,4}\\)", "") %>% 
            str_squish() %>% 
            str_to_lower()
        
        }
        
        # Construct df with all players and positions
        positions_w <- results_without_byes %>% 
          select("pos" = posw, "player" = white, "rating" = rating_w)
        
        positions_b <- results_without_byes %>% 
          select("pos" = posb, "player" = black, "rating" = rating_b)
        
        positions <- rbind(positions_w, positions_b)
        rm(positions_w, positions_b)
        
        # Now add the players with byes
        if(TRUE %in% str_detect(orig_results, "BYE")){
          positions <- rbind(positions, byes[,c(1:2, 4)])
        }
        
        # Add season and round numbers
        positions$season <- seasons_choice[[i]]
        positions$round <- j
        
        # Add round's pairings data to the list 
        all_pairings[[j]] <- results_without_byes
        
        # Add positions data to the collated list
        all_positions[[j]] <- positions
        
        # print(paste0("Extracted pairings and positions for ", 
        #              ifelse(league_choice == "team4545", "4545 S",
        #                     ifelse(lw_u1800_choice, "LW U1800 S", "LW Open S")),
        #              seasons_choice[[i]], " R", j))
        
      }
      
    } # finish round loop
    
    # For LW, extract final season standings
    if(league_choice == "lonewolf"){
      
      # URL for final LW standings
      lw_standings_url <- paste0("https://www.lichess4545.com/lonewolf/season/", 
                                    seasons_choice[[i]],
                                    ifelse(lw_u1800_choice, "u1800", ""),
                                    "/standings/")
      
      # Extract finishing positions
      lw_final_positions <- read_html(lw_standings_url) %>% 
        html_nodes("td:nth-child(2)") %>% 
        html_text() %>% 
        str_replace_all("[\r\n]" , "") %>% 
        str_squish() %>% 
        str_to_lower() # 
      
      lw_standings <- tibble("pos" = seq(1:length(lw_final_positions)),
                                "player" = lw_final_positions,
                                "round" = rep(12, length(lw_final_positions)))
    }
    
    
  } # finish season loop
  
  # Combine player pairings
  all_pairings <- bind_rows(all_pairings)
  
  # For LoneWolf, combine player positions data too
  if(league_choice == "lonewolf"){
    all_positions <- bind_rows(all_positions)
    
    # Add final positions to rest of positions data
    all_positions <- all_positions %>% 
      add_row(lw_standings)
  }
  
  # But for 4545, first team positions have to be computed
  if(league_choice == "team4545"){
    
    collated_positions <- bind_rows(all_positions)
    
    # Compute team positions for each round
    
    round_standings <- list() # to store team standings by round
    
    # Iterate through all rounds
    for(r in seq(1:max(collated_positions$round))){
      
      # Create subset of team positions data
      round_standings[[r]] <- collated_positions %>% 
        # Include team results up to including each round
        filter(round <= r) %>%
        # Compute each team's match and game points totals
        group_by(team) %>% 
        summarise(mp = sum(mp, na.rm = T),
                  gp = sum(gp, na.rm = T)) %>% 
        # Order by match points then game points
        arrange(desc(mp), desc(gp)) %>% 
        # Add rank
        mutate(rank = seq(1:nrow(.))) %>%
        # Add round indicator
        mutate(round = r)
    }
    
    # Once team standings computed for each round, combine into single data frame
    all_positions <- bind_rows(round_standings)
    
  }
  
  toc(log = TRUE)
  
  return(list(all_pairings, all_positions))
  # return(list(all_pairings, collated_positions))
  
}


# Ensure plot axes only show integer values
# From https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# Get data on all games listed in an input vector of game URLs
# Eg games <- get_games_from_urls(links)
get_games_from_urls <- function(links){
  
  # Max number of game IDs allowed per Lichess API query
  max_ids_per_request <- 300
  
  # Extract game IDs from user-supplied list of gamelinks or IDs
  all_ids <- links %>% 
    str_extract("[:alnum:]{8}|(?<=lichess.org/)[:alnum:]{8}") %>% 
    unique()

  # Split requested game IDs into list elements of size 300
  all_ids <- split(all_ids, ceiling(seq_along(all_ids)/max_ids_per_request))
  
  # Get data on requested games
  all_games <- list(rep(NA, length(all_ids)))
  
  for(l in seq(1:length(all_ids))){
    
    # Pause between batches of IDs
    if(l > 1){Sys.sleep(10)}
    
    batch_ids <- all_ids[[l]] %>% str_c(collapse = ",")
    
    query <- httr::POST(
      url = "https://lichess.org",
      path = paste0("/games/export/_ids"),
      body = batch_ids,
      query = list(moves = "true",
                   tags = "true",
                   clocks = "true",
                   evals = "true",
                   opening = "true",
                   pgnInJson = "true"),
      httr::add_headers(`Authorization` = sprintf("Bearer %s", token)),
      accept("application/x-ndjson"))
    
    # Stop if there's an error
    if(query$status_code != 200){
      print("Error!")
      print(http_status(query)$message)
      break}
    
    # Then convert the response into NDJSON
    game_data <- query %>% 
      httr::content("text", encoding = "UTF-8") %>% 
      read_lines() %>% 
      ndjson::flatten()
    
    all_games[[l]] <- game_data
    print(paste0("Obtained game data batch ", l, "/", length(all_ids)))
    
  }
  
  # Combine game data across batches into single df
  all_games <- bind_rows(all_games)
  
  print(paste0("Obtained all game data (", nrow(all_games), " games)"))
  
  # Inform user about unanalysed games
  no_analysis <- all_games %>% 
    filter(is.na(players.white.analysis.acpl))
  print(paste0(nrow(no_analysis), " games have no Lichess analysis..."))
  print(no_analysis$id)
  
  return(all_games)
  
}


# Save all required data (games, pairings, positions) for a range of seasons
# Eg save_season_data("team4545", c(12:15))
#    save_season_data("lw_open", c(12:15))
#    save_season_data("lw_u1800", c(12:15))
save_season_data <- function(league_choice, seasons){
  
  tic("Obtained all season data")
  
  # Get correct rounds parameter
  if(league_choice == "team4545"){rounds <- c(1:8)} else {
    rounds <- c(1:11)}
  
  # Make sure lw_u1800 is F if getting 4545 season data
  if(league_choice == "team4545"){
    league <- "team4545"
    lw_u1800 <- FALSE
  }
  
  if(league_choice == "lwopen"){
    league <- "lonewolf"
    lw_u1800 <- FALSE
  }
  
  if(league_choice == "lwu1800"){
    league <- "lonewolf"
    lw_u1800 <- TRUE
  }
    
  for(i in seasons){
    
    season <- i
    
    # Get correct team_boards parameter
    if(league == "lonewolf"){team_boards <- 99} else
      if(season == 1){team_boards <- 4} else
        if(season == 2){team_boards <- 5} else
          if(season <= 15){team_boards <-  6} else 
            if(season <= 24){team_boards <-  8} else
              if(season <= 26){team_boards <-  10}
    
    # Skip if user requests LW U1800 data for seasons 1-8
    # As Season 9 was the first LW season with an U1800 section
    if(league == "lonewolf"){
      if(lw_u1800){
        if(i < 9){
          print("U1800 data doesn't exist for LW seasons 1-8!")
          next
        }
      }
    }
    
    
    # Get league games and pairings data
    games_pairings <- get_league_games(league, season, lw_u1800)
    games <- games_pairings[[1]]
    website_pairings <- games_pairings[[2]]
    
    # Tidy game data
    tidied_games <- tidy_lichess_games(games)
    
    # Save game data with appropriate labels
    league_save_label <- league
    if(league == "lonewolf"){
      if(lw_u1800){league_save_label <- "lwu1800"} else {league_save_label <- "lwopen"}
    }
    
    
    
    # Get league pairings/positions data
    data <- get_league_data(league, season, rounds, lw_u1800, team_boards)
    
    pairings <- data[[1]]
    positions <- data[[2]]
    
    
    # Key differences between lichess4545_pairings and pairings (returned by get_league_data())
    
    # lichess4545_pairings:
    # - obtained through Lichess4545 website API - get_season_games endpoint
    # - only returns pairings that were played - ie associated with a Lichess game ID
    
    # pairings data obtained from get_league_data()
    # - obtained through scraping Lichess4545 pairing URLs
    # - returns all pairings, incl. forfeits, scheduling draws, and double forfeits.
    
    # Why have both?
    # Matching up Lichess game data with Lichess4545 API data allows for the Lichess game data
    # to accurately reflect the league results. Eg the game data can be filtered
    # to exclude forfeits. 
    
  
    
    # Add some fields in the pairings data to the games data before saving 
    # 4545 and LW: season, round, board
    # 4545 only:   match, tw (White team name), tb (ditto for Black),
    #              pw (White team game points in the match), pb
    #              outcomew (White team's match outcome - win/draw/loss), outcomeb
    
    if(league == "team4545"){
      # Extract team-related data from pairings data
      pairings_subset <- pairings %>% 
        select(season, round, board, "pairings_white" = white, 
               "pairings_black" = black, match, team_w, team_b, gp_w, gp_b, 
               team_res_w, team_res_b)
      
      # # Join with Lichess4545 pairings data
      # pairings_subset <- left_join(pairings_subset, lichess4545_pairings, 
      #                              by = c("round", 
      #                                     "pairings_white" = "white", 
      #                                     "pairings_black" = "black"))
      
      
      # Add team-related fields to games data
      tidied_games <- left_join(tidied_games, pairings_subset, 
                                by = c("players.white.user.id" = "pairings_white", 
                                       "players.black.user.id" = "pairings_black"))
    } else if(league == "lonewolf"){
      # For LoneWolf, just add season and round fields to the games data
      pairings_subset <- pairings %>% 
        select(season, round, "pairings_white" = white, "pairings_black" = black)
      tidied_games <- left_join(tidied_games, pairings_subset, 
                                by = c("players.white.user.id" = "pairings_white",
                                       "players.black.user.id" = "pairings_black"))
    }
    
    # Make pairings data usernames match games data usernames exactly (for played games at least)
    for(i in seq(1:nrow(pairings))){
      matches <- str_extract(tidied_games$white, fixed(pairings$white[i], ignore_case=TRUE)) 
      matches <- matches[!is.na(matches)] %>% unique()
      pairings$white[i] <- ifelse(length(matches) > 0, matches, pairings$white[i])
      matches <- str_extract(tidied_games$black, fixed(pairings$black[i], ignore_case=TRUE)) 
      matches <- matches[!is.na(matches)] %>% unique()
      pairings$black[i] <- ifelse(length(matches) > 0, matches, pairings$black[i])
    }
    
    
    # Save game data
    rio::export(tidied_games, paste0(path_savedata, "games_", league_save_label, "_s", season, ".rds"))
    
    # Save played pairings data (from Lichess4545 API)
    rio::export(website_pairings, paste0(path_savedata, "website_pairings_", league_save_label, "_s", season, ".rds"))
    
    # Save pairings/positions data
    rio::export(pairings, paste0(path_savedata, "pairings_", league_save_label, "_s", season, ".csv"))
    rio::export(positions, paste0(path_savedata, "positions_", league_save_label, "_s", season, ".csv"))
    
    print(paste0("Saved all data for ", ifelse(league == "team4545", "4545 S", ifelse(lw_u1800, "LW U1800 S", "LW Open S")), 
                 season))
    
  }
  
  toc(log = TRUE)
  
}

# Report season stats and player awards for a range of seasons
# Eg report_season_stats("team4545", c(12:15)), 
#    report_season_stats("lw_open", c(18:20))
#    report_season_stats("lw_u1800", c(18))
report_season_stats <- function(league_choice, seasons){
  
  tic("Produced season stats report")
  
  if(league_choice == "team4545"){
    league <- "team4545"
    lw_u1800_choice <- FALSE
    }
  if(league_choice == "lwopen"){
    league <- "lonewolf"
    lw_u1800_choice <- FALSE 
  }
  if(league_choice == "lwu1800"){
    league <- "lonewolf"
    lw_u1800_choice <- TRUE 
  }
  
  for (s in seasons) {
    
    league <- league
    season <- s
    lw_section <- NULL
    lw_section <- ifelse(lw_u1800_choice, "u1800", "")
    get_data <- F
    load_data <- T
    
    rmarkdown::render(paste0(path_loadrmd, paste0(stats_rmd_filename, '.Rmd')), 
                      output_format = "html_document",
                      output_file = paste0(path_savereport, "stats_",
                                           ifelse(league == "team4545", "4545", "lw"),
                                           ifelse(league == "lonewolf", 
                                                  ifelse(lw_u1800_choice, "u1800", "open"),
                                                  ""),
                                           "_",
                                           "s", 
                                           sprintf("%02d", s), 
                                           ".html"),
                      quiet = TRUE)
    print(paste0("Produced report for ", 
                 ifelse(league == "team4545", "4545 S", "LoneWolf S"),
                 s))
    
    toc(log = TRUE)
    
  }
}


# Save season data and report stats for range of seasons
# (combining prev two functions)
# Eg save_and_report_stats("team4545", c(12:15)), 
#    save_and_report_stats("lw_open", c(18:20))
#    save_and_report_stats("lw_u1800", c(18))
save_and_report_stats <- function(league_choice, season_range){
  for(x in season_range){
    save_season_data(league_choice, x)
    report_season_stats(league_choice, x)
  }
  print("Finished crunching stats!")
}


# Prep season data before processing stats for final reports
# Reads games, reads lookup data (gambits, FIDE perf ratings lookups, and banned
# 4545 players), adds gambit data to game data, excludes games and pairings 
# featuring banned players, and checks and fixes any incorrect character encoding in
# the data.
prep_games_for_report <- function(games){
  
}



# Make sunburst plot of all openings in games data
make_sunburst_wrapper <- function(league, season){
  
  # Load games data for requested league and season
  league_load_label <- league
  if(league == "lonewolf"){if(lw_u1800){league_load_label <- "lwu1800"} else {league_load_label <- "lwopen"}}
  games <- readRDS(paste0(path_savedata, "games_", league_load_label, "_s", season, ".rds"))
  
  # Construct single PGN with all games, but no evals or movetimes 
  games$pgn_noevals <- str_replace_all(games$pgn, "\\{ \\[%eval [:graph:]{1,}\\] \\[%clk [0-1]{1}:[0-5]{1}[0-9]{1}:[0-5]{1}[0-9]{1}\\] \\}", "") %>% 
    str_replace_all("\\s\\s\\d+\\.\\.\\.", "") %>% 
    str_replace_all("\\s(?=\\d+\\.)", "") %>% 
    str_replace_all("\\s(?=1-0)", "") %>% 
    str_replace_all("\\s(?=1/2)", "") %>% 
    str_replace_all("\\s(?=0-1)", "")
  pgn_noevals <- str_c(games$pgn_noevals, collapse = "")
  
  # Save PGN
  fileConn <- file(paste0(path_root, "data/games_noevals_", league, "_s", season, ".pgn"))
  writeLines(pgn_noevals, fileConn)
  close(fileConn)
  
  # Make sunburst
  pgn <- paste0(path_savedata, "games_noevals_", league, "_s", season, ".pgn")
  tic("Made openings sunburst plot")
  
  # Source make_openings_sunburst.py in R and call function to produce plot
  reticulate::import_from_path("chess_graph", path = "C:/Users/rahul/anaconda3/Lib/site-packages/chess_graph", convert = TRUE)
  reticulate::source_python(paste0(path_scripts, "make_openings_sunburst.py"))
  make_sunburst(pgn)
  
  # Move sunburst to reports folder
  move_sunburst(path_sunburst_original, path_sunburst_new, league, season)
  toc(log = TRUE)

}

# Renames and moves the openings sunburst HTML file created by 
# make_openings_sunburst.py to the reports folder, so it can be linked in
# the finals seasons stats reports
move_sunburst <- function(path_orig, path_new, league, season){
  
  if(league == "team4545"){league_var <- "4545"} else {league_var <- league}
  
  # Copy sunburst HTML to reports folder
  file.copy(from = paste0(path_root, "sunburst.html"),
            to   = paste0(path_new, "openings_", league_var, "_s", season, ".html"))
  
  # Do same for PNG file
  file.copy(from = paste0(path_root, "sunburst.png"),
            to   = paste0(path_new, "openings_", league_var, "_s", season, ".png"))
  
  # Remove original sunburst files
  file.remove(paste0(path_root, "sunburst.html"))
  file.remove(paste0(path_root, "sunburst.png"))
}

# Make a stats report from scratch
# Requests and saves season data, requests PGN for openings sunburst, makes 
# sunburst, then compiles and saves season stats HTML report.
instareport_season <- function(league, season, from_scratch = T){
  
  if(from_scratch){tic("Produce season report + sunburst from scratch")}
  if(from_scratch == F){tic("Produce season report + sunburst from existing data")}
  
  # 1. Save season data (if necessary)
  if(from_scratch){save_season_data(league, season)}
  
  # 2. Make sunburst
  make_sunburst_wrapper(league, season)
  
  # 3. Compile and produce season stats report
  report_season_stats(league, season)
  
  toc(log = TRUE)
}


# Updates index.md so https://rahulan-c.github.io/lichess4545-stats/ shows all
# seasons that have completed season stats reports, then pushes all updates to
# the public Github repo, so the website will be updated.
update_repo <- function(){
  # Push changes to repo
  source(paste0(path_scripts, "update_repo.R"))
  
  # Then render the lichess4545-stats homepage
  rmarkdown::render(input = paste0(path_root, "index.rmd"),
                    rmarkdown::md_document(variant = "gfm"),
                    quiet = TRUE)
  
  # Also render the post-contact landing page
  rmarkdown::render(input = paste0(path_root, "after_contact.rmd"),
                    rmarkdown::md_document(variant = "gfm"),
                    quiet = TRUE)
  
  # Then push the updated index.md
  source(paste0(path_scripts, "update_repo.R"))
}


# Delete all stats reports and sunburst plots in reports/
wipe_all_stats <- function(){
  # Delete any directories in reports/ that aren't images/
  dirs_to_delete <- dir_info(path_savereport) %>% 
    filter(type == "directory") %>% 
    filter(!(str_detect(path, "images")))
  if(nrow(dirs_to_delete) > 0){
    dirs_to_delete %>% 
      select(path) %>% 
      dplyr::pull() %>% 
      dir_delete(.)
    }
  # Delete any files in reports/ that aren't style.css or produce_season_stats.Rmd
  files_to_delete <- dir_info(path_savereport) %>% 
    filter(type == "file") %>% 
    filter(!(str_detect(path, "produce_season_stats.Rmd"))) %>% 
    filter(!(str_detect(path, "style.css")))
  if(nrow(files_to_delete) > 0){
    files_to_delete %>% 
      select(path) %>% 
      dplyr::pull() %>% 
      file_delete(.)
    }
  # Update repo
  update_repo()
}

build_season_reports <- function(wipe_stats_first = FALSE,
                                 request_data = FALSE,
                                 team_range = NULL, 
                                 lwopen_range = NULL, 
                                 lwu1800_range = NULL, 
                                 update_repo_after = 5){
  
  tic("Built season reports, updated website")
  
  # 4545
  if(!(is.null(team_range))){
    team_range <- sort(team_range, decreasing = T)
    team_range <- split(team_range, ceiling(seq_along(team_range)/update_repo_after))
    for(i in seq(1:length(team_range))){
      for(j in seq(1:length(team_range[[i]]))){
        instareport_season("team4545", team_range[[i]][j], from_scratch = request_data)
      }
    }
  }
  # LW Open
  if(!(is.null(lwopen_range))){
    lwopen_range <- sort(lwopen_range, decreasing = T)
    lwopen_range <- split(lwopen_range, ceiling(seq_along(lwopen_range)/update_repo_after))
    for(i in seq(1:length(lwopen_range))){
      for(j in seq(1:length(lwopen_range[[i]]))){
        instareport_season("lwopen", lwopen_range[[i]][j], from_scratch = request_data)
      }
    }
  }
  # LW U1800
  if(!(is.null(lwu1800_range))){
    lwu1800_range <- sort(lwu1800_range, decreasing = T)
    lwu1800_range <- split(lwu1800_range, ceiling(seq_along(lwu1800_range)/update_repo_after))
    for(i in seq(1:length(lwu1800_range))){
      for(j in seq(1:length(lwu1800_range[[i]]))){
        instareport_season("lwu1800", lwu1800_range[[i]][j], from_scratch = request_data)
      }
    }
  }
  
  # Push all changes to repo
  update_repo()
  toc(log = TRUE)
}



build_alltime_stats <- function(){
  tic("Refreshed all time stats page, updated website")
  
  # Produce all time stats report
  rmarkdown::render(paste0(path_loadrmd, paste0(alltime_stats_rmd_filename, '.Rmd')),
                    output_file = paste0(path_savereport, "alltime_stats.html"),
                    quiet = T)
  
  # Push changes to repo
  update_repo()
  toc(log = TRUE)
}

# One stop shop for updating the site
update_site <- function(wipe = FALSE, 
                        request = FALSE,
                        team = NULL,
                        lwopen = NULL,
                        lwu1800 = NULL){
  tic("Updated site")
  # First, build the necessary season reports
  build_season_reports(wipe_stats_first = wipe,
                       request_data = request,
                       team_range = team, 
                       lwopen_range = lwopen, 
                       lwu1800_range = lwu1800)
  # Then wait a bit
  Sys.sleep(120)
  # Then refresh the all time stats page
  build_alltime_stats()
  toc(log = TRUE)
}


# Obtain list of all accounts banned from Lichess (and the leagues) for Lichess ToS violations
# N.B. this calls a non-public script and refers to non-public data
source(paste0(path_scripts, "identify_tos_violators.R"))