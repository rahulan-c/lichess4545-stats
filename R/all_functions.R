
# ==== LICHESS4545-STATS FUNCTIONS ============================================

# Custom functions for working with Lichess and Lichess4545 data.

# ---- Required packages ------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, data.table, reactable, httr, jsonlite, xml2, 
               rvest, ndjson, reshape2, utf8, lubridate, tictoc, reticulate,
               rmarkdown, fs, stringi, git2r, glue, here, distill, htmltools,
               tidyjson)


# ---- User-defined parameters ------------------------------------------------

# Path to root folder
path_root <- here::here()

# Path to website directory
path_web <- paste0(path_root, "/docs/")

# Path to Python environment and chess_graph module
path_python <- Sys.getenv("RETICULATE_PYTHON")
chessgraph_path <- Sys.getenv("CHESSGRAPH_PATH")

# Lichess API token
token <- Sys.getenv("LICHESS_TOKEN")


# ---- Directory paths (do not change) ----------------------------------------

# Directory where this functions script is saved
path_scripts <- paste0(path_root, "/R/")

# Directory where 4545/LW season data is saved
path_savedata <- paste0(path_root, "/data/")

# Directory where season stats R Markdown file is saved
path_loadrmd <- paste0(path_root, "/site/reports/")

# R Markdown filenames
# RMD file that produces season stats
stats_rmd_filename <- "_produce_season_stats"
stats960_rmd_filename <- "_produce_960_season_stats"

# RMD file that produces all-time stats
alltime_stats_rmd_filename <- "_alltime_records"

# Directory where season stats HTML reports will be saved
path_savereport <- paste0(path_root, "/site/reports/")

# Directory where openings sunburst plots will be saved initially
path_sunburst_original <- path_scripts

# Directory where openings sunburst plots will be copied to
path_sunburst_new <- path_savereport


# ---- Functions --------------------------------------------------------------

# Extract Lichess game data for all games in seasons of the Lichess4545 Team
# and LoneWolf leagues.
LeagueGames <- function(league_choice, seasons_choice, rounds_choice = NULL,
                             lw_u1800_choice = FALSE){
  
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
          print(http_status(r)$message)
          break
          }
        
        # Extract season data as tibble
        res <- r %>% 
          httr::content("text", encoding = stringi::stri_enc_detect(httr::content(r, "raw"))[[1]][1,1]) %>% 
          jsonlite::fromJSON() %>% 
          purrr::pluck("games")
        
        # Filter by round (if rounds_choice specified)
        if(!(is.null(rounds_choice))){
          res <- res %>% filter(round %in% rounds_choice)
        }
        
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
          print(http_status(r)$message)
          break
          }
        
        # Extract season data as tibble
        res <- r %>% 
          httr::content("text", encoding = stringi::stri_enc_detect(httr::content(r, "raw"))[[1]][1,1]) %>% 
          jsonlite::fromJSON() %>% 
          purrr::pluck("games")
        
        # Add returned game data to season game IDs list
        season_data[[i]] <- res
        
      }
    
      # Chess960 league
      if(league_choice == "chess960"){
        
        r <- httr::GET(
          url = "https://www.lichess4545.com/api/get_season_games/",
          query = list(league = league_choice,
                       season = seasons_choice[[i]])
        )
        
        if(r$status_code != 200){
          print("Error!")
          print(http_status(r)$message)
          break
        }
        
        # Extract season data as tibble
        res <- r %>% 
          httr::content("text", encoding = stringi::stri_enc_detect(httr::content(r, "raw"))[[1]][1,1]) %>% 
          jsonlite::fromJSON() %>% 
          purrr::pluck("games")
        
        # Filter by round (if rounds_choice specified)
        if(!(is.null(rounds_choice))){
          res <- res %>% filter(round %in% rounds_choice)
        }
        
        # Add returned game data to season game IDs list
        season_data[[i]] <- res
        
      }

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
    if(l > 1){Sys.sleep(4)}
    
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
                   pgnInJson = "true",
                   literate = "true"),
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
  
  # Add number of moves per game to data
  all_games$num_moves <- ifelse(str_count(all_games$moves, "\\s") %% 2 == 1,
                                (str_count(all_games$moves, "\\s") / 2) + 0.5,
                                (str_count(all_games$moves, "\\s") / 2) + 1)
  
  # Only include games with at least 3 moves
  all_games <- all_games %>% 
    filter(num_moves >= 3) %>% 
    tibble::as_tibble()
  
  print(paste0("Obtained all game data (", nrow(all_games), " games)"))
  
  toc(log = TRUE) # how long to get games data from Lichess
  
  # Inform user about unanalysed games
  # Open them in the browser so analysis can be requested
  need_analysis <- all_games %>% 
    filter(is.na(players.white.analysis.acpl)) %>% 
    select(id) %>% 
    as_tibble()
  
  
  
  if(nrow(need_analysis) > 0) {
    print("Some games haven't been analysed by Lichess...")
    print(need_analysis$id)
    for (l in seq(1:length(need_analysis$id))) {
      browseURL(glue::glue("https://lichess.org/{need_analysis$id[l]}"), browser = getOption("browser"),
                encodeIfNeeded = FALSE)
    }
  }
  
  # Return Lichess game data on all game IDs in the Lichess4545 pairings data
  # and also return the Lichess4545 pairing data
  return(list(all_games, all_data, need_analysis$id))
  
}


# Get data on any Lichess user's games

UserGames <- function(username, since = NULL, until = NULL, perfs){
  
  # Function that converts a date from the "YYYY-MM-DD" format to the correct  
  # numeric format for the Lichess API. Also adds "00:01" or "23:59" depending
  # on whether the date is meant to signify the start or end of the search period.
  LichessDateFormat <- function(date, time_modifier){
    date <- as.numeric(formatC(
      as.numeric(lubridate::parse_date_time(paste0(date, " ", time_modifier), "ymdHMS")) * 1000,
      digits = 0, format = "f"
    ))
    return(date)
  }
  
  # If user doesn't enter anything for 'since' or 'until', use the default values
  # of "Account creation date" and "Now"
  since_entered <- ifelse(is.null(since), "Account creation date", LichessDateFormat(since, "00:00:01"))
  until_entered <- ifelse(is.null(until), "Now", LichessDateFormat(until, "23:59:59"))
  
  # Request games
  get_games <- httr::GET(
    url = "https://lichess.org",
    path = paste0("/api/games/user/", username),
    query = list(since = since_entered,
                 until = until_entered,
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

TidyGames <- function(games){

  tic("Tidied game data")
    
  # Exclude games that ended due to a) someone claiming a result due to their opponent 
  # disconnecting, b) cheat detection, or c) someone aborting the game
  # Also exclude games that started from custom positions
  games <- games %>% 
    tibble::as_tibble() %>% 
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
  
  if(!(nrow(games) > 0)){
    cli::cli_inform("No valid games in data. Nothing to tidy!")
    return(as_tibble(games))
  }
  
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
  
  
  # Tidy openings data (for non-960 games data)
  if(games$variant[1] != "chess960"){
    
    # # Fix foreign characters in openings names
    # games$opening.name <- games$opening.name %>%
    #   str_replace_all("Ã¼", "ü") %>%
    #   str_replace_all("Ã¶", "ö") %>%
    #   str_replace_all("Ã³", "ó") %>%
    #   str_replace_all("Ã©", "é")
    
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
  
  }
  
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
  times <- purrr::map(str_extract_all(games$pgn, "(?<=clk\\s)[0-1]\\:[0-5][0-9]\\:[0-5][0-9]"), lubridate::hms) %>% 
    purrr::map(lubridate::period_to_seconds) %>% 
    purrr::map2(games$clock.increment, ~ lag(.x, 2) - .x + .y)
  
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
  # evals <- reshape2::melt(evals, id.vars = c("id"))
  # Use tidyr::pivot_longer() instead
  evals <- tidyr::pivot_longer(evals, where(is.numeric))
  # evals <- tidyr::pivot_longer(evals, names_to = "id")
  evals$name <- as.numeric(as.character(evals$name))
  
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
  # mates <- melt(mates)
  if(ncol(mates) > 1){
    mates <- tidyr::pivot_longer(mates, where(is.numeric))
    mates$name <- as.numeric(as.character(mates$name))
  }
  
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
      mutate(eval_prev = lag(eval, 1)) %>% 
      # Cap evals to +/- 1000 before calculating CPL
      mutate(eval = pmax(pmin(eval, 1000), -1000)) %>% 
      mutate(eval_prev = pmax(pmin(lag(eval, 1), 1000), -1000)) %>% 
      mutate(cpl = case_when(
        ply == 0 ~ 0,
        ply %% 2 == 0 ~ pmax(eval_prev - eval, 0),
        ply %% 2 == 1 ~ (pmin(eval_prev - eval, 0)) * -1,
        TRUE ~ NA_real_
      ))
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
    mutate(eval_after_15 = unlist(map(evals, ~ .x$cpl[30])))
  
  games <- as_tibble(games)
  
  # Remove unnecessary variables from data
  games <- games %>% 
    select(-starts_with("analysis")) %>% 
    select(-c(clock.totalTime, createdAt, lastMoveAt))

  toc(log = TRUE)
  
  return(as_tibble(games))

}


# Get 4545/LW season pairings and player/team positions data
LeagueData <- function(league_choice, 
                            seasons_choice, 
                            rounds_choice, 
                            lw_u1800_choice = FALSE, 
                            boards_per_team_choice = 99){
  
  tic("Extracted pairing/rank data")
  
  # If only one round's games are required, repeat positions data extraction over the season to date
  all_pairings <- list()
  all_positions <- list()
  
  # For each selected season...
  for(i in seq(1:length(seasons_choice))){
    
    # For rounds 1:j+1 in each selected season...
    for(j in seq(1:(min(max(rounds_choice) + 1, 
                        ifelse(league_choice == "lonewolf", 
                               11, 
                               8))))){
      
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
        team_pairings <- rvest::read_html(url) %>%
          rvest::html_nodes("td") %>% 
          rvest::html_text() %>% 
          stringr::str_replace_all("[\r\n]" , "") %>% 
          stringr::str_squish() %>% 
          stringr::str_c()
        
        if(team_pairings[(length(team_pairings))] == "Unavailable"){
          team_pairings <- team_pairings[1:(length(team_pairings)- 2)]
        }
        
        # Identify players, result, and scheduled times; collate into tibble
        player1 = team_pairings[c(T, F, F, F)]
        result = team_pairings[c(F, T, F, F)]
        player2 = team_pairings[c(F, F, T, F)]
        sched_time = team_pairings[c(F, F, F, T)]
      
        team_pairings <- tibble::tibble(player1 = player1, 
                                player2 = player2, 
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
          stringr::str_squish() %>% 
          stringr::str_replace_all("Calendar" , "") %>% 
          stringr::str_replace_all("½", ".5")
        
        teams <- cbind.data.frame(split(teams, rep(1:5, times=length(teams)/5)), stringsAsFactors=F)
        names(teams) <- c("team_1", "score_1", "score_2", "team_2", "blank")
        teams <- teams[,1:4]
        
        
        # Add team scores
        teams$score_1 <- str_replace_all(teams$score_1, "\u00bd", ".5")
        teams$score_2 <- str_replace_all(teams$score_2, "\u00bd", ".5")
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
      
        Sys.sleep(0.2)
        
      }
      
      
      # Extract data on LoneWolf pairings
      if(league_choice %in% c("lonewolf", "chess960")){
        
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
        
        Sys.sleep(0.2)
      }
    } # finish round loop
    
    # LW / 960: extract final season standings
    if(league_choice %in% c("lonewolf", "chess960")){
      
      # URL for final LW/960 standings
      lw_standings_url <- paste0("https://www.lichess4545.com/", 
                                 league_choice, 
                                 "/season/", 
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
                                # Add a set of positions for the final standings
                                # Assign this set to round number last_round + 1 
                                "round" = rep(ifelse(league_choice == "lonewolf", 
                                                     12, # as LW has 11 rounds
                                                     9), # as chess960 has 8 rounds
                                              length(lw_final_positions)))
    }
  } # finish season loop
  
  # Combine player pairings
  all_pairings <- bind_rows(all_pairings)
  
  # For LoneWolf/960, combine player positions data too
  if(league_choice %in% c("lonewolf", "chess960")){
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
        summarise(mp = sum(mp, na.rm = F),
                  gp = sum(gp, na.rm = F)) %>% 
        # Order by match points then game points
        arrange(desc(mp), desc(gp)) %>% 
        # Add rank
        mutate(rank = dense_rank(row_number())) %>%
        # Add round indicator
        mutate(round = r)
    }
    # Once team standings computed for each round, combine into single data frame
    all_positions <- bind_rows(round_standings)
  }
  
  toc(log = TRUE)
  return(list(all_pairings, all_positions))
  
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
# Arguments:
# ids: vector of game IDs
# links: vector of game URLs
GetGamesFromURLs <- function(ids = NULL, links = NULL){
  
  # Max number of game IDs allowed per Lichess API query
  max_ids_per_request <- 300
  
  # If game IDs are provided...
  if (!(is.null(ids))) {
    all_ids <- ids %>% unique()
  # Or else if game URLs are provided...
  } else if (!(is.null(links))) {
    all_ids <- links %>% 
      str_extract("[:alnum:]{8}|(?<=lichess.org/)[:alnum:]{8}") %>% 
      unique()
  }

  # Split requested game IDs into list elements of size 300
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
  all_games <- bind_rows(all_games) %>% as_tibble()
  
  print(paste0("Obtained all game data (", nrow(all_games), " games)"))
  
  # Add number of moves per game to data
  all_games$num_moves <- ifelse(str_count(all_games$moves, "\\s") %% 2 == 1,
                            (str_count(all_games$moves, "\\s") / 2) + 0.5,
                            (str_count(all_games$moves, "\\s") / 2) + 1)
  
  # Only include games with at least 3 moves
  all_games <- all_games %>% 
    filter(num_moves >= 3)
  
  # Inform user about unanalysed games
  # Open them in the browser so analysis can be requested
  need_analysis <- all_games %>%
    filter(is.na(players.white.analysis.acpl)) %>%
    filter(perf != "fromPosition") %>% 
    select(id) %>%
    mutate(id = paste0("https://lichess.org/", id)) %>% 
    as_tibble()

  if(nrow(need_analysis) > 0) {
    print("Some games haven't been analysed by Lichess...")
    print(need_analysis$id)
    # if(nrow(need_analysis) < 21) {
    #   cli::cli_inform("Opening each un-analysed game in the browser for analysis to be requested...")
    #   for (l in seq(1:length(need_analysis$id))) {
    #     browseURL(id, browser = getOption("browser"),
    #               encodeIfNeeded = FALSE)
    #     Sys.sleep(1)
    #   }
    # } else {
    #   cli::cli_inform("There are too many un-analysed games to open in the browser. 
    #                   I don't want you to get an IP ban! :)")
    # }
  }
  
  
  return(list(all_games, need_analysis))
  
}


# Save all required data (games, pairings, positions) for a range of seasons
SaveSeasonData <- function(league_choice, seasons){
  
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
  
  if(league_choice == "chess960"){
    league <- "chess960"
    rounds <- c(1:8)
    lw_u1800 <- FALSE
  }
    
  for(i in seasons){
    
    season <- i
    
    # Get correct team_boards parameter
    if(league == "lonewolf"){team_boards <- 99} else
      if(season == 1){team_boards <- 4} else
        if(season == 2){team_boards <- 5} else
          if(season <= 15){team_boards <-  6} else 
            if(season <= 24){team_boards <-  8} else
              if(season <= 99){team_boards <-  10}
    
    if(league == "chess960"){
      if(season == 1){rounds <- c(1:7)}
    }
    
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
    games_pairings <- LeagueGames(league_choice = league, 
                                       seasons_choice =  season, 
                                       rounds_choice = rounds,
                                       lw_u1800_choice = lw_u1800)

    games <- games_pairings[[1]] %>% tibble::as_tibble()
    website_pairings <- games_pairings[[2]] %>% tibble::as_tibble()
    
    # For 4545, add both players' teams to games data (from website pairings data)
    # Increases the robustness of the subsequent merge with pairings data
    if(league == "team4545"){
      websub <- website_pairings %>% select(game_id, "teamw_web" = white_team, 
                                            "teamb_web" = black_team)
      games <- dplyr::left_join(games, websub, by = c("id" = "game_id"))
    }
    
    # Tidy game data
    tidied_games <- TidyGames(games) %>% tibble::as_tibble()
    
    # Save game data with appropriate labels
    league_save_label <- league
    if(league == "lonewolf"){
      if(lw_u1800){league_save_label <- "lwu1800"} else {league_save_label <- "lwopen"}
    }
    
    # Get league pairings/positions data
    data <- LeagueData(league, season, rounds, lw_u1800, team_boards)
    
    pairings <- data[[1]] %>% tibble::as_tibble()
    positions <- data[[2]] %>% tibble::as_tibble()
    
    # For 4545 datasets, add team information to games data (from pairings data)
    if(league == "team4545"){
      pairings_subset <- pairings %>% 
        select(season, round, board, "pairings_white" = white, 
               "pairings_black" = black, match, team_w, team_b, gp_w, gp_b, 
               team_res_w, team_res_b)
      tidied_games <- dplyr::left_join(tidied_games, pairings_subset, 
                                by = c("players.white.user.id" = "pairings_white",
                                       "teamw_web" = "team_w",
                                       "players.black.user.id" = "pairings_black",
                                       "teamb_web" = "team_b"))
      tidied_games <- tidied_games %>% 
        rename("team_w" = teamw_web, "team_b" = teamb_web)
    
    # For LoneWolf and 960, just add season and round fields to the games data
    } else {
      
      pairings_subset <- pairings %>% 
        select(season, round, "pairings_white" = white, "pairings_black" = black)
      tidied_games <- left_join(tidied_games, pairings_subset, 
                                by = c("players.white.user.id" = "pairings_white",
                                       "players.black.user.id" = "pairings_black"))
    }
    
    # For all leagues, make sure the usernames in the pairings data match 
    # those in the games data -- at least for played games
    for(i in seq(1:nrow(pairings))){
      matches <- str_extract(tidied_games$white, fixed(pairings$white[i], ignore_case=TRUE)) 
      matches <- matches[!is.na(matches)] %>% unique()
      pairings$white[i] <- ifelse(length(matches) > 0, matches, pairings$white[i])
      matches <- str_extract(tidied_games$black, fixed(pairings$black[i], ignore_case=TRUE)) 
      matches <- matches[!is.na(matches)] %>% unique()
      pairings$black[i] <- ifelse(length(matches) > 0, matches, pairings$black[i])
    }
    
    # Save season datasets
    # Game data
    rio::export(tidied_games, paste0(path_savedata, "games_", league_save_label, "_s", season, ".rds"))
    
    # Played pairings data (from Lichess4545 API) - aka website pairings
    rio::export(website_pairings, paste0(path_savedata, "website_pairings_", league_save_label, "_s", season, ".rds"))
    
    # Pairings data from round pairing pages
    # Not the same as website pairings - this includes unplayed pairing info
    rio::export(pairings, paste0(path_savedata, "pairings_", league_save_label, "_s", season, ".csv"))
    
    # Player/team positions data
    rio::export(positions, paste0(path_savedata, "positions_", league_save_label, "_s", season, ".csv"))
    
    # print(paste0("Saved all data for ", ifelse(league == "team4545", "4545 S", ifelse(lw_u1800, "LW U1800 S", "LW Open S")), 
    #              season))
    
    cli::cli_alert_success("Saved season data for {ifelse(league == 'team4545', '4545 S', ifelse(lw_u1800, 'LW U1800 S', 'LW Open S'))}{season}")
    
  }
  
  toc(log = TRUE)
  
}



# Report season stats and player awards for one or more seasons
SeasonStats <- function(league_choice, seasons){
  
  tic("Produced season stats report")
  
  if(league_choice == "team4545"){
    league <- "team4545"
    rounds <- c(seq(1:8))
    lw_u1800_choice <- FALSE
  } 

  if(league_choice == "lwopen"){
    league <- "lonewolf"
    rounds <- c(seq(1:11))
    lw_u1800_choice <- FALSE 
  }
  if(league_choice == "lwu1800"){
    league <- "lonewolf"
    rounds <- c(seq(1:11))
    lw_u1800_choice <- TRUE 
  }
  
  if(league_choice == "chess960"){
    league <- "chess960"
    rounds <- c(seq(1:8))
    lw_u1800_choice <- FALSE
  }
  
  for (s in seasons) {
    
    league <- league
    season <- s
    lw_section <- NULL
    lw_section <- ifelse(lw_u1800_choice, "u1800", "")
    get_data <- F
    load_data <- T
    
    if(league %in% c("lwopen", "lwu1800")) {
      
      if(season == 1) {rounds <- c(seq(1:5))}
      if(season %in% c(2:3)){rounds <- c(seq(1:8))
      } else {
      rounds <- c(seq(1:11))
      }
    }
    
    if(league == "chess960"){
      if(season == 1){
        cli::cli_inform("Can't produce a report for Chess960 League S1 - no data available.")
      }
    }
    
    if(league != "chess960"){
      # Render 4545/LW season stats report 
      rmarkdown::render(paste0(path_loadrmd, paste0(stats_rmd_filename, '.Rmd')), 
                        output_file = paste0("stats_",
                                             ifelse(league == "team4545", "4545", "lw"),
                                             ifelse(league == "lonewolf", 
                                                    ifelse(lw_u1800_choice, "u1800", "open"),
                                                    ""),
                                             "_",
                                             "s", 
                                             sprintf("%02d", s), 
                                             ".html"),
                        params = list(
                          league = league,
                          season = s,
                          rounds = rounds,
                          lw_section = lw_section
                                      ),
                        quiet = TRUE)
      print(paste0("Produced report for ", 
                   ifelse(league == "team4545", "4545 S", "LoneWolf S"),
                   s))
      
    } else {
      
      # Render 960 season stats report 
      rmarkdown::render(paste0(path_loadrmd, paste0(stats960_rmd_filename, ".rmd")), 
                        output_file = paste0("stats_chess960_",
                                             "s", 
                                             sprintf("%02d", s), 
                                             ".html"),
                        params = list(
                          league = league,
                          season = s,
                          rounds = rounds,
                          lw_section = lw_section
                        ),
                        quiet = TRUE)
      print(paste0("Produced report for Chess960 S",
                   s))
      
    }
    
    toc(log = TRUE)
    
  }
}

# Report round stats for one or more league/season/round combinations
RoundStats <- function(league_choice, seasons, rounds){
  
  tic("Produced round stats report(s)")
  
  if(league_choice == "team4545"){
    league <- "team4545"
    rounds <- rounds
    lw_u1800_choice <- FALSE
  } 
  
  if(league_choice == "lwopen"){
    league <- "lonewolf"
    rounds <- rounds
    lw_u1800_choice <- FALSE 
  }
  if(league_choice == "lwu1800"){
    league <- "lonewolf"
    rounds <- rounds
    lw_u1800_choice <- TRUE 
  }
  
  if(league_choice == "chess960"){
    league <- "chess960"
    rounds <- rounds
    lw_u1800_choice <- FALSE
  }
  
  for (s in seasons) {
    
    for (r in rounds){
    
      league <- league
      season <- s
      rounds <- r
      lw_section <- NULL
      lw_section <- ifelse(lw_u1800_choice, "u1800", "")
      get_data <- F
      load_data <- T
      
      
      
      if(league == "chess960"){
        if(season == 1){
          cli::cli_inform("Can't produce a report for Chess960 League S1 - no data available.")
        }
      }
      
      if(league != "chess960"){
        # Render 4545/LW season stats report 
        rmarkdown::render(paste0(path_loadrmd, paste0(stats_rmd_filename, '.Rmd')), 
                          output_file = paste0("stats_",
                                               ifelse(league == "team4545", "4545", "lw"),
                                               ifelse(league == "lonewolf", 
                                                      ifelse(lw_u1800_choice, "u1800", "open"),
                                                      ""),
                                               "_",
                                               "s", 
                                               sprintf("%02d", s),
                                               "r",
                                               sprintf("%02d", r),
                                               ".html"),
                          params = list(
                            league = league,
                            season = s,
                            rounds = r,
                            lw_section = lw_section
                          ),
                          quiet = TRUE)
        print(paste0("Produced report for ", 
                     ifelse(league == "team4545", "4545 S", "LoneWolf S"),
                     s, " R", r))
        
      } else {
        
        # Render 960 season stats report 
        rmarkdown::render(paste0(path_loadrmd, paste0(stats960_rmd_filename, ".rmd")), 
                          output_file = paste0("stats_chess960_",
                                               "s", 
                                               sprintf("%02d", s),
                                               "r",
                                               sprintf("%02d", r),
                                               ".html"),
                          params = list(
                            league = league,
                            season = s,
                            rounds = r,
                            lw_section = lw_section
                          ),
                          quiet = TRUE)
        print(paste0("Produced report for Chess960 S",
                     s,
                     " R",
                     r))
        
      }
      
      toc(log = TRUE)
      
    }
  }
}



# Make sunburst plot of all openings in games data
MakeSunburst <- function(league, season){
  
  # Renames and moves the openings sunburst HTML file created by 
  # make_openings_sunburst.py to the reports folder, so it can be linked in
  # the finals seasons stats reports
  MoveSunburst <- function(path_orig, path_new, league, season){
    
    if(league == "team4545"){league_var <- "4545"} else {league_var <- league}
    
    # Copy sunburst HTML to reports folder
    file.copy(from = paste0(path_root, "/sunburst.html"),
              to   = paste0(path_new, "/openings_", league_var, "_s", season, ".html"))
    
    # Do same for PNG file
    file.copy(from = paste0(path_root, "/sunburst.png"),
              to   = paste0(path_new, "/openings_", league_var, "_s", season, ".png"))
    
    # Remove original sunburst files
    file.remove(paste0(path_root, "/sunburst.html"))
    file.remove(paste0(path_root, "/sunburst.png"))
  }
  
  
  # Load games data for requested league and season
  league_load_label <- league
  if(league == "lonewolf"){if(lw_u1800){league_load_label <- "lwu1800"} else {league_load_label <- "lwopen"}}
  games <- readRDS(paste0(path_savedata, "games_", league_load_label, "_s", season, ".rds"))
  
  # Introduce rule that if there's under 200 games played in the season, don't produce a sunburst
  # They take too long and they're not informative
  # TODO
  
  # Construct single PGN with all games, but no evals or movetimes 
  games$pgn_noevals <- str_replace_all(games$pgn, "\\{ \\[%eval [:graph:]{1,}\\] \\[%clk [0-1]{1}:[0-5]{1}[0-9]{1}:[0-5]{1}[0-9]{1}\\] \\}", "") %>% 
    str_replace_all("\\s\\s\\d+\\.\\.\\.", "") %>% 
    str_replace_all("\\s(?=\\d+\\.)", "") %>% 
    str_replace_all("\\s(?=1-0)", "") %>% 
    str_replace_all("\\s(?=1/2)", "") %>% 
    str_replace_all("\\s(?=0-1)", "")
  pgn_noevals <- str_c(games$pgn_noevals, collapse = "")
  
  # Save PGN
  fileConn <- file(paste0(path_root, "/data/games_noevals_", league, "_s", season, ".pgn"))
  writeLines(pgn_noevals, fileConn)
  close(fileConn)
  
  # Now convert this PGN into an *actual* clean PGN :)
  # Uses Python code originally suggested by wdhorton
  # Required because the code above doesn't produce a totally valid clean PGN
  # for even a relative small sample of games (~1500)
  SaveCleanPGN(filename = paste0("games_noevals_", league, "_s", season), 
               new_filename = paste0("games_noevals_", league, "_s", season)
               )
  
  # Make sunburst
  pgn <- paste0(path_savedata, "games_noevals_", league, "_s", season, ".pgn")
  tic("Made openings sunburst plot")
  
  # Source make_openings_sunburst.py in R and call function to produce plot
  reticulate::import_from_path("chess_graph", path = chessgraph_path, convert = TRUE)
  reticulate::source_python(paste0(path_scripts, "/Python/make_openings_sunburst.py"))
  make_sunburst(pgn)
  
  # Move sunburst to reports folder
  MoveSunburst(path_sunburst_original, path_sunburst_new, league, season)
  toc(log = TRUE)

}



MakeSeasonReport <- function(league, season, 
                               from_scratch = T){
  
  # Make a stats report from scratch
  # Requests and saves season data, requests PGN for openings sunburst, makes 
  # sunburst, then compiles and saves season stats HTML report.
  
  if(from_scratch){tic("Produce season report from new data")}
  if(from_scratch == F){tic("Produce season report from saved data")}
  
  # 1. Save season data (if necessary)
  if(from_scratch){SaveSeasonData(league, season)}
  
  # 2. Make openings sunburst (only for 4545/LW reports)
  if(league %in% c("team4545", "lwopen", "lwu1800")){
    MakeSunburst(league, season)
  }
  
  # 3. Compile and produce season stats report
  SeasonStats(league, season)
  
  toc(log = TRUE)
}

MakeRoundReport <- function(league, season, round,
                             from_scratch = T){
  
  # Make a stats report from scratch
  # Requests and saves season data, requests PGN for openings sunburst, makes 
  # sunburst, then compiles and saves season stats HTML report.
  
  if(from_scratch){tic("Produce season report from new data")}
  if(from_scratch == F){tic("Produce season report from saved data")}
  
  # 1. Save season data (if necessary)
  if(from_scratch){SaveSeasonData(league, season)}
  
  # 2. Make openings sunburst (only for 4545/LW reports)
  if(league %in% c("team4545", "lwopen", "lwu1800")){
    MakeSunburst(league, season)
  }
  
  # 3. Compile and produce season stats report
  SeasonStats(league, season)
  
  toc(log = TRUE)
}




UpdateRepo <- function(){
  # Pushes latest local repo changes to Github
  source(paste0(here::here(), "/R/update_repo.R"))
}



BuildSeasonReports <- function(wipe_stats_first = FALSE,
                                 request_data = FALSE,
                                 team_range = NULL, 
                                 lwopen_range = NULL, 
                                 lwu1800_range = NULL,
                                 chess960_range = NULL,
                                 update_repo_after = 5){
  
  tic("Compiled season reports")
  
  
  # 4545
  if(!(is.null(team_range))){
    team_range <- sort(team_range, decreasing = T)
    team_range <- split(team_range, ceiling(seq_along(team_range)/update_repo_after))
    for(i in seq(1:length(team_range))){
      for(j in seq(1:length(team_range[[i]]))){
        MakeSeasonReport("team4545", team_range[[i]][j], from_scratch = request_data)
      }
    }
  }
  # LW Open
  if(!(is.null(lwopen_range))){
    lwopen_range <- sort(lwopen_range, decreasing = T)
    lwopen_range <- split(lwopen_range, ceiling(seq_along(lwopen_range)/update_repo_after))
    for(i in seq(1:length(lwopen_range))){
      for(j in seq(1:length(lwopen_range[[i]]))){
        MakeSeasonReport("lwopen", lwopen_range[[i]][j], from_scratch = request_data)
      }
    }
  }
  # LW U1800
  if(!(is.null(lwu1800_range))){
    lwu1800_range <- sort(lwu1800_range, decreasing = T)
    lwu1800_range <- split(lwu1800_range, ceiling(seq_along(lwu1800_range)/update_repo_after))
    for(i in seq(1:length(lwu1800_range))){
      for(j in seq(1:length(lwu1800_range[[i]]))){
        MakeSeasonReport("lwu1800", lwu1800_range[[i]][j], from_scratch = request_data)
      }
    }
  }
  # Chess960
  if(!(is.null(chess960_range))){
    chess960_range <- sort(chess960_range, decreasing = T)
    chess960_range <- split(chess960_range, ceiling(seq_along(chess960_range)/update_repo_after))
    for(i in seq(1:length(chess960_range))){
      for(j in seq(1:length(chess960_range[[i]]))){
        MakeSeasonReport("chess960", chess960_range[[i]][j], from_scratch = request_data)
      }
    }
  }
  
  toc(log = TRUE)
}





# Save a PGN file with all games in a season using the season's .RDS data file
# Takes: (1) filename: season games RDS data filename, eg "games_lwopen_s22.rds" 
# (note: this is expected to be saved in data/). 
# Returns: saves a PGN file in the data folder ("pgn_evals.pgn"). Nothing explicitly returned.
# Example: GetSeasonPGN("games_lwopen_s22.rds")
GetSeasonPGN <- function(filename, with_evals = T){
  games <- readRDS(paste0("data/", filename))
  pgn_evals <- str_c(games$pgn, collapse = NULL)
  fileConn <- file(paste0(path_root, "/data/", "pgn_evals.pgn"))
  writeLines(pgn_evals, fileConn)
  close(fileConn)
}


# Save a PGN with all games but no evals or movetimes
# Required for openings sunburst
SavePGN_NoEvalsClocks <- function(games, 
                                      save_path = "/data/tidied_games_noevals.pgn", 
                                      path_root = path_root){
  save_path <- paste0(path_root, save_path)
  pgn_data <- games$pgn
  pgn_data$noevals <- str_replace_all(pgn_data$pgn, 
                                       "\\{ \\[%eval [:graph:]{1,}\\] \\[%clk [0-1]{1}:[0-5]{1}[0-9]{1}:[0-5]{1}[0-9]{1}\\] \\}", 
                                       "") %>% 
    str_replace_all("\\s\\s\\d+\\.\\.\\.", "") %>% 
    str_replace_all("\\s(?=\\d+\\.)", "") %>% 
    str_replace_all("\\s(?=1-0)", "") %>% 
    str_replace_all("\\s(?=1/2)", "") %>% 
    str_replace_all("\\s(?=0-1)", "")
  pgn_noevals <- str_c(pgn_data$noevals, collapse = "")
  fileConn <- file(save_path)
  writeLines(pgn_noevals, fileConn)
  close(fileConn)
}


# Save PGN with evals and movetimes
# Eg needed for sac detection
SavePGN_EvalsClocks <- function(games, 
                                save_path = "/data/games_evals.pgn"){
  out_path <- paste0(path_root, save_path)
  pgn_evals <- str_c(games$pgn, collapse = "")
  fileConn <- file(out_path)
  writeLines(pgn_evals, fileConn)
  close(fileConn)
}


# Save 'clean' PGN (no variations or comments)
SaveCleanPGN <- function(filename, new_filename){
  # filename: path from root, excluding first / and ".pgn"
  # new_filename: similar
  tic("Saved clean PGN")
  reticulate::source_python(paste0(path_scripts, "Python/save_clean_pgn.py"))
  save_clean_pgn(read_path =  paste0(path_savedata, filename, ".pgn"),
                 write_path = paste0(path_savedata, new_filename, ".pgn"),
                 new_name = new_filename)
  # Move clean PGN to data/
  fs::file_move(paste0(path_root, "/", new_filename, ".pgn"),
                paste0(path_savedata))
  toc(log = T)
}

# Save PGN with evals
SaveEvalsPGN <- function(filename, new_filename){
  tic("Saved PGN with evals")
  reticulate::source_python(paste0(path_scripts, "Python/save_clean_pgn.py"))
  save_evals_pgn(read_path =  paste0(path_savedata, filename, ".pgn"),
                 write_path = paste0(path_savedata, new_filename, ".pgn"),
                 new_name = new_filename)
  # Move PGN to data/
  fs::file_move(paste0(path_root, "/", new_filename, ".pgn"),
                paste0(path_savedata))
  toc(log = T)
}



# PROBABLY SUPERSEDED - DELETE AFTER CONFIRMING
GetPlayedPairings <- function(league, latest_season){
  
  # poss league values: 
  # 4545: "team4545"
  # LW: "lonewolf"
  # C960: "chess960"
  
  tic("Get season data from the Lichess4545 website")
  url <- "https://www.lichess4545.com/api/get_season_games/"
  
  # Define seasons to iterate over
  # LW: combine Open and U1800 seasons (latter starts from 9)
  if(league != "lonewolf") {
    seasons <- c(1:latest_season)
  } else {
    seasons <- c(1:latest_season, paste0(c(9:latest_season), "u1800"))
  }
  
  res_league<- list() # for storing season pairings
  
  # Request season pairings from Lichess4545 API
  for(s in seasons){
    r <- httr::GET(
      url = url,
      query = list(league = league,
                   season = s)
    )
    if(r$status_code != 200){
      print("Error!")
      print(http_status(r)$message)
      break
    }
    res <- r %>% 
      httr::content("text", encoding = stringi::stri_enc_detect(httr::content(r, "raw"))[[1]][1,1]) %>% 
      jsonlite::fromJSON() %>% 
      purrr::pluck("games")
    res_league[[s]] <- res
    cli::cli_inform("Obtained pairings for {stringr::str_to_title(league)} Season {s}")
    Sys.sleep(0.5)
  }
  # Combine all obtained game data
  res_league <- data.table::rbindlist(res_league) %>% as_tibble()
  toc(log = TRUE)
  return(res_league)
}

# PROBABLY SUPERSEDED - CONFIRM THEN DELETE
UpdateAllTimeGames <- function(update_league, latest_season){
  
  # For league l and season number s:
  # (1) Gets IDs of all games in l played in seasons 1-s
  # (2) Checks for any IDs that aren't recorded in the current all-time games data for league l
  # (3) Obtains game data for any additional games
  # (4) Adds this data to the existing all-times data
  # (5) Saves the new dataset
  
  # Arguments:  1) update_league: league to be updated ("team4545", "lonewolf")
  #             2) latest_season: the season up to which all games should be checked (integer)
  
  # Returns:  n/a
  
  # Examples: UpdateAllTimeGames("team4545", 27)
  #           UpdateAllTimeGames("lonewolf", 24)  
  
  # TODO: add more leagues

  
  if(update_league == "team4545"){
    league_print <- "4545"
    filename <- "allgames_team.rds"
  } else if (update_league == "lonewolf"){
    league_print <- "LoneWolf"
    filename <- "allgames_lw.rds"
  }
  
  
  
  # Read current all-time games dataset (for specified league)
  all_games <- readRDS(paste0(here::here(), "/data/", filename))
  
  if(update_league == "team4545"){
  all_games <- tibble::as_tibble(all_games) %>% 
    select(id, clock.increment, clock.initial, moves, opening.eco, opening.name,
           pgn, players.black.analysis.acpl, players.black.analysis.blunder,
           players.black.analysis.inaccuracy, players.black.analysis.mistake, 
           players.black.user.id,
           players.white.analysis.acpl, players.white.analysis.blunder,
           players.white.analysis.inaccuracy, players.white.analysis.mistake, 
           players.white.user.id, rated, status, winner, players.black.user.title,
           players.white.user.title, num_moves, white, black, result, score_w, score_b,
           rating_w, rating_b, mean_rating, opening.broad, started, ended, year, month,
           day, wday, hour, week, date, first_moves, first_move_w, first_move_b, duration,
           duration_w, duration_b, perc_total_clock_w, perc_total_clock_b, clock_used_after_move10_w, 
           clock_used_after_move10_b, evals, eval_after_15, league, season, round, white_team, black_team)
  } else {
    # Exclude team columns
    all_games <- tibble::as_tibble(all_games) %>% 
      select(id, clock.increment, clock.initial, moves, opening.eco, opening.name,
             pgn, players.black.analysis.acpl, players.black.analysis.blunder,
             players.black.analysis.inaccuracy, players.black.analysis.mistake, 
             players.black.user.id,
             players.white.analysis.acpl, players.white.analysis.blunder,
             players.white.analysis.inaccuracy, players.white.analysis.mistake, 
             players.white.user.id, rated, status, winner, players.black.user.title,
             players.white.user.title, num_moves, white, black, result, score_w, score_b,
             rating_w, rating_b, mean_rating, opening.broad, started, ended, year, month,
             day, wday, hour, week, date, first_moves, first_move_w, first_move_b, duration,
             duration_w, duration_b, perc_total_clock_w, perc_total_clock_b, clock_used_after_move10_w, 
             clock_used_after_move10_b, evals, eval_after_15, league, season, round)
  }
  
  # Print details of current league all-time games dataset
  cli::cli_inform("{nrow(all_games)} games are recorded in the current all-time {str_to_title(update_league)} data")
  cli::cli_inform("Earliest recorded game started: {min(all_games$started)}")
  cli::cli_inform("Last recorded game ended: {max(all_games$ended)}")
  cli::cli_inform("")
  
  # Update dataset if its most recent game is more than a week old
  if(lubridate::today() - 7 > max(all_games$started)){
    
    cli::cli_inform("Starting update process (since the last game in the dataset was played more than a week ago")
    
    played_pairings <- GetPlayedPairings(update_league, latest_season)
    
    # Extract game IDs for all played pairings, incl. games later forfeited
    ids_valid <- played_pairings %>% select(game_id) %>% dplyr::pull() %>% unique()
    
    # Filter all-time dataset to only include valid game IDs
    valid_games_from_before <- all_games %>% 
      filter(id %in% ids_valid) %>% 
      distinct(id, .keep_all = T) %>% 
      tibble::as_tibble()
    
    # Find all valid game IDs not already captured in current all-time league games
    valid_ids_toadd <- setdiff(ids_valid, all_games$id)
    
    # Get the data for these new games
    valid_games_data <- GetGamesFromURLs(ids = valid_ids_toadd)
    
    # Find the IDs of any new games that don't have Lichess analysis
    # If any are identified, stop the update process 
    need_analysis <- valid_games_data[[2]] %>% tibble::as_tibble()
    if(nrow(need_analysis) > 0){
      cli::cli_inform("{nrow(need_analysis)} new games haven't been analysed by Lichess")
      cli::cli_inform("Stopping update process so analysis can be requested.")
      return(need_analysis)
    }
    
    valid_games_toadd <- tibble::as_tibble(valid_games_data[[1]])
    
    # Tidy new games data
    tidied_games_toadd <- TidyGames(valid_games_toadd)
    
    if(nrow(tidied_games_toadd) == 0) {
      cli::cli_inform("All 'new' games are invalid (probably just old invalid games)")
      cli::cli_inform("No update possible. Stopping process.")
      return(NULL)
    }
    
    tidied_games_toadd <- tibble::as_tibble(tidied_games_toadd)
    
    # Prepare both existing and new games datasets for combination
    # Neither should have provisional, initialFen vars or fens in evals data
    tidied_games_toadd <- tidied_games_toadd %>% 
      select(-c(players.white.provisional, 
                players.black.provisional, 
                initialFen,
                duration_official)) %>% 
      tibble::as_tibble()
    
    if(update_league == "team4545"){
    tidied_games_toadd <- tidied_games_toadd %>% 
      select(id, clock.increment, clock.initial, moves, opening.eco, opening.name,
             pgn, players.black.analysis.acpl, players.black.analysis.blunder,
             players.black.analysis.inaccuracy, players.black.analysis.mistake, 
             players.black.user.id,
             players.white.analysis.acpl, players.white.analysis.blunder,
             players.white.analysis.inaccuracy, players.white.analysis.mistake, 
             players.white.user.id, rated, status, winner, players.black.user.title,
             players.white.user.title, num_moves, white, black, result, score_w, score_b,
             rating_w, rating_b, mean_rating, opening.broad, started, ended, year, month,
             day, wday, hour, week, date, first_moves, first_move_w, first_move_b, duration,
             duration_w, duration_b, perc_total_clock_w, perc_total_clock_b, clock_used_after_move10_w, 
             clock_used_after_move10_b, evals, eval_after_15, league, season, round, white_team, black_team)
    } else {
      # Exclude team columns
      tidied_games_toadd <- tidied_games_toadd %>% 
        select(id, clock.increment, clock.initial, moves, opening.eco, opening.name,
               pgn, players.black.analysis.acpl, players.black.analysis.blunder,
               players.black.analysis.inaccuracy, players.black.analysis.mistake, 
               players.black.user.id,
               players.white.analysis.acpl, players.white.analysis.blunder,
               players.white.analysis.inaccuracy, players.white.analysis.mistake, 
               players.white.user.id, rated, status, winner, players.black.user.title,
               players.white.user.title, num_moves, white, black, result, score_w, score_b,
               rating_w, rating_b, mean_rating, opening.broad, started, ended, year, month,
               day, wday, hour, week, date, first_moves, first_move_w, first_move_b, duration,
               duration_w, duration_b, perc_total_clock_w, perc_total_clock_b, clock_used_after_move10_w, 
               clock_used_after_move10_b, evals, eval_after_15, league, season, round)
    }
             
    
    # Combine the previous and new games datasets
    new_all_games <- data.table::rbindlist(list(all_games, 
                                                tidied_games_toadd), 
                                           use.names = T)
    
    # Check that each game is unique
    length(unique(new_all_games$id)) == nrow(new_all_games)
    
    # Sort final dataset by game start date/time
    new_all_games <- new_all_games %>% 
      arrange(started)
    
    # Summarise results
    cli::cli_alert_success("Produced new {update_league} all-time games dataset")
    cli::cli_inform("{nrow(played_pairings)} played pairings")
    cli::cli_inform("{nrow(all_games)} games previously saved")
    cli::cli_inform("{nrow(tidied_games_toadd)} new games added")
    cli::cli_inform("{nrow(new_all_games)} games in final combined dataset")
    
    # Save new all-time games dataset
    saveRDS(new_all_games,
            paste0(here::here(), "/data/", filename))
  } else {
    cli::cli_inform("The most recent game in {filename} was played less than a week ago - best not to update the all-time dataset yet.")
  }
}

