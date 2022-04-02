
# =============================================================================
#     COLLATE LEAGUE DATASETS OVER ALL PLAYED/REPORTED SEASONS
#     (4545, LW [Open, U1800], Chess960, Series, Rapid Battle)
# =============================================================================

# Last updated: 2022-03-08

# Various functions for compiling league datasets and game data to support 
# further analysis. 

# League-specific workflows:

# When a 4545 season ends...
# 1. CompileCombinedData(league_choice == "team4545)
# 2. SaveLeaguePGN("team4545")
# 3. CombineTeamLWSeriesGames()
# 4. SaveTeamLWSeriesPGN()

# When a LW season ends...
# 1. CompileCombinedData("lwopen")
# 2. CompileCombinedData("lwu1800")
# 3. SaveLeaguePGN("lwopen")
# 4. SaveLeaguePGN("lwu1800")
# 5. CombineTeamLWSeriesGames()
# 6. SaveTeamLWSeriesPGN()

# When a Chess960 league season ends...
# 1. CompileCombinedData("chess960")
# 2. SaveChess960PGN()

# When a Series season ends...
# 1. UpdateAllSeriesGames(series_sheets)
# 2. SaveLeaguePGN("series")
# 3. CombineTeamLWSeriesGames()
# 4. SaveTeamLWSeriesPGN()
# 5. TODO: compile non-games season data across all seasons

# When a Rapid Battle season ends...
# 1. UpdateAllRapidBattleGames(rb_sheets)
# 2. SaveLeaguePGN("rb")
# 3. TODO: combine rapid leagues' games data into single .RDS file
# 4. TODO: save single PGN with all rapid league games



# Full list:

# - CompileCombinedData(league_choice)
#   Compiles season-specific games, pairings, website pairings, and player/team 
#   positions datasets for all reported seasons of the 4545, LW Open, LW 
#   U1800 and Chess960 leagues, and saves resulting all-seasons datasets in
#   data/.

# - SaveLeaguePGN(league)
#   Saves a single PGN with all games played over all previous completed seasons 
#   in a specified league (4545, LW Open, LW U1800 or Chess960)

# - SaveTeamLWSeriesPGN()
#   Saves a single PGN with all 4545, LW Open, LW U1800 and Series games saved in
#   the latest versions of each league's all-seasons PGN, which is assumed to cover
#   all previous seasons to date.

# - CombineTeamLWSeriesGames()
#   Saves a single RDS file with the games saved in the latest versions of the 
#   individual league-specific all-seasons RDS files for 4545, LW Open, LW U1800 
#   and Series, which are assumed to cover all previously played seasons to date.

# - SaveChess960PGN() - saves a single PGN with all 960 league games played in
#     reported seasons.

# - UpdateAllSeriesGames(series_sheets)
#     Adds new Series games to current Series version of all-seasons games data.  

# - UpdateAllRapidBattleGames(rb_sheets)
#     Adds new Rapid Battle games to current RB version of all-seasons games data.  

# - GetSeriesSeasonData(season) - UNFINISHED

# - GetRapidBattleSeasonData(season) - UNFINISHED



# ---- PRELIMINARIES ----------------------------------------------------------

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, lubridate, tictoc, fs, stringi, glue, 
               here, cli, googlesheets4)

options(cli.progress_show_after = 0)

# Source all_functions.R
source(paste0(here::here(), "/R/all_functions.R"))


# ---- SERIES SHEET IDS -------------------------------------------------------
# Read from lookup CSV
series_sheets <- readr::read_csv(paste0(here::here(), 
         "/data/lookup/series_seasons.csv"))

# ---- RAPID BATTLE SHEET DATA ------------------------------------------------
# Compile lookup data with sheet IDs for all previous Rapid Battle season 
# spreadsheets and the sheet names where all gamelinks are recorded
# TODO: Replace this once the RB lookup CSV has been completed.

rb_sheets <- tibble(
  "season" = c(1:17),
  "sheetid" = c(
    "1O6XkJO6J4uB49ChiRpSoZochBUwgXn4mUKffUa9taig", # S1
    "", # S2 - sheet exists but doesn't have any gamelinks. ID 1SawXGckVVIYKHD6WLMKL0Lni6pmsGfGnoPTc7Su0T6Y 
    "", # S3
    "1keDSVXeix95GhkdVQCiNaGNQMOn7iju7SctLvFeaW6I", # S4
    "1Uz1RPcudMfhngP1mD1xFwa8zLaNkNRY4Cah9lfjx_YQ", # S5
    "1VsjSOlV48fOfTOveA8cMUW6AcOdI4LpPDsnIqMBiDbc", # S6
    "1pwEnUOrDJMcEuI9jhIwXf2dY9Tt7cw4SBIIOizMFDJE", # S7
    "1_ZMlmfXg7sENk00NynkFNSlnaIoHcs7YhX5GYyCEG2M", # S8
    "106R8n8-rniq6tmr0buh1929tiosYxW_6yFnGbul6JcY", # S9
    "1lAgZ1FYvjM_BOTv_8QRPtAguAzVPHTNZhLyujoYNDkw", # S9B: for loser's bracket
    "1F0ynh03E9JLAjM-LdEH3EpPTdijbaWyAMzhfBsN1IY0", # S10
    "1CkMtgQR1fVCYBKKP-193tox2hZOB7AV8luZhmwgJB2Q", # S11
    "1zBL2oJXWgfuxgM98a68Fynn0TaJVyFqC6neQYx8ip9s", # S12
    "1uylkUxqGre9mJGyVWaja86PJMVjzntTWJlihMf6ZFd8", # S13
    "1esWor-776WtLePYTpsYHmbHcsBKP_13M_35uv_Wsxqg", # S14
    "1N2ixyY6r_COHhBoFrmAzBaMu3taTlEQFIEhC6wJhgSI", # S15
    "1y2cQTKyMN01nLCRDXbv7Rq8LZ1c-xvarO335ruhuYpE"  # S16 
  ),
  "sheetnames" = list(
    list("RR", "knock-out"),
    list(""),
    list(""),
    list("Matches"),
    list("Matches"),
    list("Links pool", "Elimination stage"),
    list("Links Division 1", "Links Division 2", "Elimination stage div 1", "Elimination stage div 2"),
    list("Links", "Elimination stage"),
    list("Links", "Elimination stage"),
    list("5-8", "9-16", "19-28"),
    list("Links div 1", "Links div 2", "Elimination stage div 1", "Elimination stage div 2"),
    list("Group Stage", "Knockouts", "U1800 Group Stage", "U1800 Knockouts"),
    list("Group Stage", "Open Playoffs", "U1800 Group Stage", "U1800 Playoffs"),
    list("Open Section Playoffs"),
    list("Div A Group Stage", "Div B Group Stage", "Div A Playoffs", "Div B Playoffs"),
    list("Copy of Copy of Section A Group Stage", "Section B Group Stage", "Playoffs A", "Playoffs B"),
    list("Div A Group", "Div B Group", "Div A Playoffs", "Div B Playoffs")
  ),
  "seasons" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 11, 12, 13, 14, 15, 16)
  # S9 has to appear twice to account for the additional S9 loser's bracket sheet
)




# ---- FUNCTIONS ---------------------------------------------------------------

CompileCombinedData <- function(league_choice){
  
  tic("Runtime")

  cli_h2("Compiling data for all {league_choice} seasons")
  
  
  # ---- 1/4 Games ------------------------------------------------------------
  
  savedfiles <- fs::dir_info(paste0(here::here(), "/data/")) %>% 
    filter(type == "file") 
  
  savedgames <- savedfiles %>% 
    filter(stringr::str_detect(path, ".rds")) %>% 
    filter(stringr::str_detect(path, paste0("games_", league_choice))) %>% 
    filter(!(stringr::str_detect(path, paste0("allgames_", league_choice))))
  
  lst_games <- list()
  s <- 0
  cli_progress_step("Games: extracted {s}/{nrow(savedgames)} {qty(nrow(savedgames))}file{?s}.")
  
  for (s in c(1:nrow(savedgames))){
    lst_games[[s]] <- readRDS(savedgames$path[s]) %>% 
      tibble::as_tibble() %>% 
      mutate(league = league_choice)
    Sys.sleep(0.2)
    cli_progress_update()
  }
  cli_progress_done()
  
  allgames <- data.table::rbindlist(lst_games, fill = T) %>% tibble::as_tibble()
  cli_alert_success("Games data combined")
  rm(lst_games)
  
  # ---- 2/4 Pairings ---------------------------------------------------------
  
  savedpairings <- savedfiles %>% 
    filter(stringr::str_detect(path, ".csv")) %>% 
    filter(stringr::str_detect(path, paste0("pairings_", league_choice))) %>% 
    filter(!(stringr::str_detect(path, "website_pairings"))) %>% 
    filter(!(stringr::str_detect(path, "webpairings"))) %>% 
    filter(!(stringr::str_detect(path, "allpairings")))
    
  lst_pairings <- list()
  s <- 0
  cli_progress_step("Pairings: extracted {s}/{nrow(savedpairings)} {qty(nrow(savedpairings))}file{?s}.")
  for (s in c(1:nrow(savedpairings))){
    lst_pairings[[s]] <- readr::read_csv(savedpairings$path[s],
                                      show_col_types = F) %>% 
      tibble::as_tibble() %>% 
      mutate(league = league_choice)
    Sys.sleep(0.2)
    cli_progress_update()
  }
  cli_progress_done()
  
  allpairings <- data.table::rbindlist(lst_pairings) %>% tibble::as_tibble()
  cli_alert_success("Pairings data combined")
  rm(lst_pairings)
  
  
  # ---- 3/4 Website pairings -------------------------------------------------
  
  savedwebpairings <- savedfiles %>% 
    filter(stringr::str_detect(path, ".rds")) %>% 
    filter(stringr::str_detect(path, paste0("website_pairings_", league_choice)))
  
  lst_webpairings <- list()
  s <- 0
  cli_progress_step("Website pairings: extracted {s}/{nrow(savedwebpairings)} {qty(nrow(savedwebpairings))}file{?s}.")
  for (s in c(1:nrow(savedwebpairings))){
    lst_webpairings[[s]] <- readRDS(savedwebpairings$path[s]) %>% 
      tibble::as_tibble() %>% 
      mutate(league = league_choice)
    Sys.sleep(0.2)
    cli_progress_update()
  }
  cli_progress_done()
  
  allwebpairings <- data.table::rbindlist(lst_webpairings) %>% tibble::as_tibble()
  cli_alert_success("Website pairings data combined")
  rm(lst_webpairings)
  
  
  # ---- 4/4 Positions --------------------------------------------------------
  
  savedpositions <- savedfiles %>% 
    filter(stringr::str_detect(path, ".csv")) %>% 
    filter(stringr::str_detect(path, paste0("positions_", league_choice)))
  
  lst_positions <- list()
  s <- 0
  cli_progress_step("Positions: extracted {s}/{nrow(savedpositions)} {qty(nrow(savedpositions))}file{?s}.")
  for (s in c(1:nrow(savedpositions))){
    lst_positions[[s]] <- readr::read_csv(savedpositions$path[s],
                                            show_col_types = F) %>% 
      tibble::as_tibble() %>% 
      mutate(league = league_choice)
    Sys.sleep(0.2)
    cli_progress_update()
  }
  cli_progress_done()
  
  allpositions <- data.table::rbindlist(lst_positions) %>% tibble::as_tibble()
  cli_alert_success("Positions data combined")
  rm(lst_positions)
  
  # ---- Save all datasets ----------------------------------------------------
  saveRDS(allgames, paste0(here::here(), "/data/", "allgames_", league_choice, ".rds"))
  saveRDS(allwebpairings, paste0(here::here(), "/data/", "allwebpairings_", league_choice, ".rds"))
  saveRDS(allpairings, paste0(here::here(), "/data/allpairings_", league_choice, ".rds"))
  saveRDS(allpositions, paste0(here::here(), "/data/allpositions_", league_choice, ".rds"))
  
  cli_alert_success("All {league_choice} data extracted, combined and saved!")
  toc(log = TRUE)
}


SaveLeaguePGN <- function(league){
  # Save a PGN of all games played in a league (4545, LW Open, LW U1800 or Chess960)
  tic(paste0("Save all-time games PGN for ", league))
  league_games <- readRDS(paste0(here::here(), "/data/allgames_", league, ".rds")) %>% 
    select(pgn)
  # Make a first attempt to remove evals and clock times from PGN data
  league_games$pgn <- str_replace_all(league_games$pgn, 
                                          "\\{ \\[%eval [:graph:]{1,}\\] \\[%clk [0-1]{1}:[0-5]{1}[0-9]{1}:[0-5]{1}[0-9]{1}\\] \\}", 
                                          "") %>% 
    str_replace_all("\\s\\s\\d+\\.\\.\\.", "") %>% 
    str_replace_all("\\s(?=\\d+\\.)", "") %>% 
    str_replace_all("\\s(?=1-0)", "") %>% 
    str_replace_all("\\s(?=1/2)", "") %>% 
    str_replace_all("\\s(?=0-1)", "")
  pgn <- str_c(league_games$pgn, collapse = "")
  # Save this effort locally
  fileConn <- file(paste0(here::here(), "/data/allgames_", league, ".pgn"))
  writeLines(pgn, fileConn)
  close(fileConn)
  # Then use python-chess to save a properly cleaned PGN without evals, clock times or errors
  SaveCleanPGN(filename = paste0("allgames_", league), 
               new_filename = paste0("allgames_", league)
  )
  toc(log = TRUE)
}


SaveTeamLWSeriesPGN <- function(){
  # Saves a PGN of all 4545, LW and Series games (without evals or clock times)
  
  tic("Save all-time 4545/LW/Series PGN")
  
  # Read and collate 4545 / LW / Series all-time games datasets
  team4545_games <- readRDS(paste0(here::here(), "/data/allgames_team4545.rds")) %>% select(pgn)
  lwopen_games <- readRDS(paste0(here::here(), "/data/allgames_lwopen.rds")) %>% select(pgn)
  lwu1800_games <- readRDS(paste0(here::here(), "/data/allgames_lwu1800.rds")) %>% select(pgn)
  series_games <- readRDS(paste0(here::here(), "/data/allgames_series.rds")) %>% select(pgn)
  
  combined <- rbind(team4545_games, lwopen_games, lwu1800_games, series_games)
  
  # Remove evals and clock times from PGN data
  combined$pgn <- str_replace_all(combined$pgn, 
                                          "\\{ \\[%eval [:graph:]{1,}\\] \\[%clk [0-1]{1}:[0-5]{1}[0-9]{1}:[0-5]{1}[0-9]{1}\\] \\}", 
                                          "") %>% 
    str_replace_all("\\s\\s\\d+\\.\\.\\.", "") %>% 
    str_replace_all("\\s(?=\\d+\\.)", "") %>% 
    str_replace_all("\\s(?=1-0)", "") %>% 
    str_replace_all("\\s(?=1/2)", "") %>% 
    str_replace_all("\\s(?=0-1)", "")
  
  # Produce single PGN with all leagues' games
  pgn <- str_c(combined$pgn, collapse = "")
  
  # Save PGN
  fileConn <- file(paste0(here::here(), "/data/allgames_teamlwseries.pgn"))
  writeLines(pgn, fileConn)
  close(fileConn)
  # Then use python-chess to save a properly cleaned PGN without evals, clock times or errors
  SaveCleanPGN(filename = paste0("allgames_teamlwseries"), 
               new_filename = paste0("allgames_teamlwseries")
  )
  toc(log = TRUE)
}

SaveChess960PGN <- function(){
  # Saves a PGN of all Chess960 league games without evals or clock times
  
  tic("Save all-time Chess960 PGN")
  
  # Read and collate 4545 / LW / Series all-time games datasets
  chess960_games <- readRDS(paste0(here::here(), "/data/allgames_chess960.rds")) %>% select(pgn)
  
  # Remove evals and clock times from PGN data
  pgn <- str_replace_all(chess960_games$pgn, 
                                          "\\{ \\[%eval [:graph:]{1,}\\] \\[%clk [0-1]{1}:[0-5]{1}[0-9]{1}:[0-5]{1}[0-9]{1}\\] \\}", 
                                          "") %>% 
    str_replace_all("\\s\\s\\d+\\.\\.\\.", "") %>% 
    str_replace_all("\\s(?=\\d+\\.)", "") %>% 
    str_replace_all("\\s(?=1-0)", "") %>% 
    str_replace_all("\\s(?=1/2)", "") %>% 
    str_replace_all("\\s(?=0-1)", "")
  
  # Produce single PGN with all leagues' games
  pgn <- str_c(pgn, collapse = "")
  
  # Save PGN
  fileConn <- file(paste0(here::here(), "/data/allgames_chess960.pgn"))
  writeLines(pgn_noevals, fileConn)
  close(fileConn)
  # Then use python-chess to save a properly cleaned PGN without evals, clock times or errors
  SaveCleanPGN(filename = paste0("allgames_chess960"), 
               new_filename = paste0("allgames_chess960")
  )
  
  toc(log = TRUE)
}


UpdateAllSeriesGames <- function(series_sheets){
  # ---- Get all Series gamelinks from season spreadsheets ----------------------
  
  cli_h1("COMPILE / UPDATE SERIES DATA")
  
  cli_h2("Step 1 - Identify all Series games from season spreadsheets")
  
  
  
  # Now go through each season and extract gamelinks
  all_links <- list()
  
  for (s in seq(1:nrow(series_sheets))) {
    
    # Read pairings sheet data 
    pairings <- range_read_cells(
      ss = series_sheets$sheetid[s],
      sheet = series_sheets$gamelink_sheet[s], 
      cell_data = "full",
      discard_empty = FALSE
    )
    
    # Extract gamelinks
    contents <- pairings$cell
    contents <- paste0(unlist(contents), collapse = "")
    links <- stringr::str_extract_all(contents, "https://lichess.org/[:alnum:]{8}")
    links <- links[[1]]
    links <- unique(links)
    season_games <- tibble::tibble(link = links,
                                   season = rep(s, length(links)))
    all_links[[s]] <- season_games
    cli::cli_inform("S{s}: extracted {length(links)} gamelinks")
    Sys.sleep(3)
  }
  
  
  # Get all gamelinks
  series_data <- data.table::rbindlist(all_links)
  series_data <- tibble::as_tibble(series_data) %>% 
    distinct(link, .keep_all = T)
  series_ids <- unlist(str_extract_all(series_data$link, "(?<=https://lichess.org/)[:alnum:]{8}"))
  cli::cli_inform("All spreadsheet data extracted. {length(series_ids)} Series gamelinks found.")
  
  
  rm(all_links, links, contents, pairings)
  
  
  
  
  # --- Find new Series games to add to all-seasons games data ---------------------
  
  cli_h2("Step 2 - Identify games to add to all-time games data")
  
  # Read current all-time Series games data
  current_allgames <- readRDS(paste0(here::here(), "/data/allgames_series.rds"))
  current_allgames <- tibble::as_tibble(current_allgames)
  current_oldest <- current_allgames %>% slice_min(date) %>% select(date) %>% dplyr::pull()
  current_latest <- current_allgames %>% slice_max(date) %>% select(date) %>% dplyr::pull()
  cli::cli_inform("Reading current Series all-seasons games data...")
  cli::cli_inform("The current dataset has {nrow(current_allgames)} games.")
  cli::cli_inform("Seasons covered: S{min(current_allgames$season)} to S{max(current_allgames$season)}.")
  cli::cli_inform("Oldest game: {current_oldest}. Most recent game: {current_latest}.")
  # Identify any Series gamelinks identified that aren't in the all-time data
  new <- setdiff(series_ids, current_allgames$id)
  cli::cli_inform("Comparing all seasons' gamelinks with the current all-seasons game dataset...")
  cli::cli_inform("{length(new)} Series games aren't in the current all-seasons dataset")
  
  # ---- Get data on new games --------------------------------------------------
  
  # Request data on new games
  new_data <- GetGamesFromURLs(links = new)
  # NOTE: returns gamelinks of un-analysed games. Good idea to check this first! 
  
  new_games <- new_data[[1]] %>% tibble::as_tibble()
  
  # ---- Add new games data to all-time data ------------------------------------
  
  # Tidy new games data
  new_games <- TidyGames(new_games)
  
  # Add new games to all-time data
  new_allgames <- data.table::rbindlist(list(current_allgames,
                                             new_games),
                                        fill = T) %>% 
    tibble::as_tibble()
  
  # Add season numbers to all-time data
  series_data <- series_data %>% 
    mutate(id = str_extract(link, "(?<=https://lichess.org/)[:alnum:]{8}")) %>% 
    select(id, season)
  new_allgames <- dplyr::left_join(new_allgames, series_data, by = c("id"))
  
  # ---- Save updated all-time games data --------------------------------------- 
  saveRDS(new_allgames, 
          paste0(here::here(), "/data/allgames_series.rds"))
  
  cli::cli_alert_success("Series all-time games data updated and saved - now contains {nrow(new_allgames)} games.")
}



UpdateAllRapidBattleGames <- function(rb_sheets){
  
  # ---- Get all Rapid Battle gamelinks from season spreadsheets ----------------------
  
  cli_h1("COMPILE / UPDATE RAPID BATTLE ALL-TIME GAMES DATA")
  
  cli_h2("Step 1 - Identify all RB games from season spreadsheets")
  
  # Now go through each season and extract gamelinks
  lst_all_season_games <- list()
  
  for (s in seq(1:nrow(rb_sheets))) {
    
    if (rb_sheets$sheetid[s] != "") {
      
      lst_season_games <- list()
      
      for (r in seq(1:length(rb_sheets$sheetnames[s][[1]]))) {
        
        # Read pairings sheet data 
        pairings <- range_read_cells(
          ss = rb_sheets$sheetid[s],
          sheet = rb_sheets$sheetnames[s][[1]][[r]], 
          cell_data = "full",
          discard_empty = FALSE
        )
        
        # Extract gamelinks
        contents <- pairings$cell
        contents <- paste0(unlist(contents), collapse = "")
        links <- stringr::str_extract_all(contents, "https://lichess.org/[:alnum:]{8}")
        links <- links[[1]]
        links <- unique(links)
        # season_pairings[[r]] <- links
        sheet_games <- tibble::tibble(link = links,
                                      season = rep(s, length(links)))
        lst_season_games[[r]] <- sheet_games
        cli::cli_inform("{s}. Sheet {r}: extracted {length(links)} gamelinks")
        Sys.sleep(2)
        
      }
      
      season_games <- data.table::rbindlist(lst_season_games)
      season_games <- tibble::as_tibble(season_games) %>% 
        distinct(link, .keep_all = T)
      
      lst_all_season_games[[s]] <- season_games
      cli::cli_inform("S{s}: extracted {nrow(season_games)} gamelinks")
      
    } else {
      cli::cli_inform("S{s}: no sheet found")
    }
  }
  
  # Get all gamelinks
  all_games <- data.table::rbindlist(lst_all_season_games)
  all_games <- tibble::as_tibble(all_games) %>% 
    distinct(link, .keep_all = T)
  
  rb_ids <- unlist(str_extract_all(all_games$link, "(?<=https://lichess.org/)[:alnum:]{8}"))
  cli::cli_inform("All season spreadsheets processed. {length(rb_ids)} RB gamelinks found.")
  
  # Change season numbers for all games with seasons listed as 10 or greater
  # Subtract 1 from the listed season number.
  # Original error arose due to the additional 9B loser bracket sheet.
  all_games$season[all_games$season >= 10] <- all_games$season[all_games$season >= 10] - 1
  
  
  # --- Find new RB games to add to all-time games data ---------------------
  
  cli_h2("Step 2 - Identify games to add to all-time games data")
  
  # Read current all-time RB games data (if file exists)
  data_exists <- fs::dir_info(paste0(here::here(), "/data/")) %>% 
    filter(type == "file") %>% 
    filter(str_detect(path, "allgames_rb.rds")) %>% 
    nrow()
  data_exists <- ifelse(data_exists == 0, FALSE, TRUE)
  
  if(data_exists){
    
    current_allgames <- readRDS(paste0(here::here(), "/data/allgames_rb.rds"))
    current_allgames <- tibble::as_tibble(current_allgames)
    
    current_oldest <- current_allgames %>% slice_min(date) %>% select(date) %>% dplyr::pull()
    current_latest <- current_allgames %>% slice_max(date) %>% select(date) %>% dplyr::pull()
    cli::cli_inform("Reading current Rapid Battle all-seasons games data...")
    cli::cli_inform("The current dataset has {nrow(current_allgames)} games.")
    cli::cli_inform("Seasons covered: S{min(current_allgames$season)} to S{max(current_allgames$season)}.")
    cli::cli_inform("Oldest game: {current_oldest}. Most recent game: {current_latest}.")
    
    # Identify any RB gamelinks identified that aren't in the all-time data
    new <- setdiff(rb_ids, current_allgames$id)
    cli::cli_inform("{length(new)} Rapid Battle games aren't in the current all-time dataset")
    
    
    # ---- Get data on new games --------------------------------------------------
    
    # Request data on new games
    new_data <- GetGamesFromURLs(links = new)
    
    new_games <- new_data[[1]] %>% tibble::as_tibble()
    
    # ---- Check for un-analysed new games ----------------------------------------
    
    new_unanalysed <- new_data[[2]] %>% tibble::as_tibble()
    
    # ---- Add new games data to all-time data ------------------------------------
    
    # Tidy new games data
    new_games <- TidyGames(new_games)
    
    # Add new games to all-time data
    new_allgames <- data.table::rbindlist(list(current_allgames,
                                               new_games),
                                          fill = T) %>% 
      tibble::as_tibble()
    
    # Add season numbers to all-time data
    all_games <- all_games %>% 
      mutate(id = str_extract(link, "(?<=https://lichess.org/)[:alnum:]{8}")) %>% 
      select(id, season)
    new_allgames <- dplyr::left_join(new_allgames, all_games, by = c("id"))
    
    # ---- Save updated all-time games data --------------------------------------- 
    saveRDS(new_allgames, 
            paste0(here::here(), "/data/allgames_rb.rds"))
    
    cli::cli_alert_success("Rapid Battle all-time games data updated and saved - now contains {nrow(new_allgames)} games.")
    
  } else {
    
    cli::cli_inform("No all-time games dataset found, so all identified games need to be saved.")
    
    # ---- Get data on new games --------------------------------------------------
    
    # Request data on new games
    new_data <- GetGamesFromURLs(links = rb_ids)
    new_games <- new_data[[1]] %>% tibble::as_tibble()
    
    # ---- Check for un-analysed new games ----------------------------------------
    
    new_unanalysed <- new_data[[2]] %>% tibble::as_tibble()
    # # Open unanalysed games in the browser in batches of max 20
    # batch <- 1
    # for (l in c(((20*batch)-19):min(20*batch, length(new_unanalysed$id)))) {
    #   browseURL(new_unanalysed$id[l], browser = getOption("browser"),
    #             encodeIfNeeded = FALSE)
    #   Sys.sleep(0.3)
    # }
    
    # ---- Add new games data to all-time data ------------------------------------
    
    # Tidy new games data
    new_games <- tidy_lichess_games(new_games)
    
    # Add season numbers to all-time data
    all_games <- all_games %>%
      mutate(id = str_extract(link, "(?<=https://lichess.org/)[:alnum:]{8}")) %>%
      select(id, season)
    new_allgames <- dplyr::left_join(new_games, all_games, by = c("id"))
    
    # ---- Save updated all-time games data ---------------------------------------
    saveRDS(new_allgames,
            paste0(here::here(), "/data/allgames_rb.rds"))
    
    cli::cli_alert_success("Rapid Battle all-time games data updated and saved - now contains {nrow(new_allgames)} games.")
    
  }
  
}

# TODO: complete this!
GetSeriesSeasonData <- function(season){
  # Compile and save games and group standings data for an individual Series 
  # season. 
  # Args: season (str): Series season number, eg 15
  
  # How it works:
  # - reads lookup CSV with info on all Series seasons. NEEDS TO BE KEPT UP TO DATE.
  # - gets Google sheet id for specified season.
  # - opens relevant season spreadsheet.
  # - compiles, tidies and saves data on all games played in the season.
  # - compiles, tidies and saves data on each group's final standings.
   
}


# TODO: complete this!
GetRapidBattleSeasonData <- function(season){
  # Compile and save games and group standings data for an individual Rapid 
  # Battle season. 
  # Args: season (str): RB season number, eg 15
  
  # How it works:
  # - reads lookup CSV with info on all RB seasons. NEEDS TO BE KEPT UP TO DATE.
  # - gets Google sheet id for specified season.
  # - opens relevant season spreadsheet.
  # - compiles, tidies and saves data on all games played in the season.
  # - compiles, tidies and saves data on each division's 1st/2nd/3rd placed 
  #   finishers, and playoff pairings/results.
  
}










CombineTeamLWSeriesGames <- function(){
  tic("Save combined games data")
  # Read and collate 4545 / LW / Series all-time games datasets
  team4545_games <- readRDS(paste0(here::here(), "/data/allgames_team4545.rds"))
  lwopen_games <- readRDS(paste0(here::here(), "/data/allgames_lwopen.rds"))
  lwu1800_games <- readRDS(paste0(here::here(), "/data/allgames_lwu1800.rds"))
  series_games <- readRDS(paste0(here::here(), "/data/allgames_series.rds"))
  combined <- data.table::rbindlist(list(team4545_games, lwopen_games, lwu1800_games, series_games),
                                    fill = T)
  saveRDS(combined, paste0(here::here(), "/data/", "allgames_teamlwseries.rds"))
  toc(log = TRUE)
}


###############################################################################


# ---- CALL FUNCTIONS --------------------------------------------------------- 
# league_choice options:
# "team4545", "lwopen", "lwu1800", "chess960"

# Compile all previously obtained season datasets into league-specific all-time
# datasets for 4545, LW and Chess960

# 4545
# CompileCombinedData(league_choice = "team4545")
# SaveLeaguePGN("team4545") # warning: took 4 mins for S1-S29

# LW Open
# CompileCombinedData(league_choice = "lwopen")
# SaveLeaguePGN("lwopen")

# LW U1800
# CompileCombinedData(league_choice = "lwu1800")
# SaveLeaguePGN("lwu1800")

# Chess960
# CompileCombinedData(league_choice = "chess960")
# SaveLeaguePGN("chess960")

# # Update all-seasons Series games data (.rds)
# UpdateAllSeriesGames(series_sheets)
# 
# # Update all-seasons Rapid Battle games data (.rds)
# UpdateAllRapidBattleGames(rb_sheets)

# Collate all-time 4545/LW/Series games data into single RDS file
CombineTeamLWSeriesGames()

# Save single PGN with all 4545, LW (both sections) and Series games (no evals or clock times)
SaveTeamLWSeriesPGN()




# Update all-time Infinite Quest games dataset
# TODO


                 