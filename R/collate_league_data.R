
# =============================================================================
#     COLLATE LEAGUE DATASETS OVER ALL PLAYED/REPORTED SEASONS
#     (4545, LW [Open, U1800], Chess960, Series, Rapid Battle)
# =============================================================================

# Last updated: 2022-05-16

# Various functions for compiling league datasets and game data to support 
# further analysis. 

# League-specific workflows:

# When a 4545 season ends...
# - CompileCombinedData(league_choice == "team4545)
# - SaveLeaguePGN(leagues = "team4545")

# When a LW season ends...
# - CompileCombinedData("lwopen")
# - SaveLeaguePGN(leagues = "lwopen")
# - CompileCombinedData("lwu1800")
# - SaveLeaguePGN(leagues = "lwu1800")

# When a Chess960 league season ends...
# - CompileCombinedData("chess960")
# - SaveLeaguePGN(leagues = "chess960")

# To update all-time Series games data RDS file and PGN
# - UpdateAllSeriesGames(series_sheets)
# - SaveLeaguePGN(leagues = "series")

# To update all-time Rapid Battle games data RDS file and PGN
# - UpdateAllRapidBattleGames(rb_sheets)
# - SaveLeaguePGN(leagues = "rb")

# To update all-time Quest games RDS file
# - UpdateQuestGames()
# - SaveLeaguePGN(leagues = "quest")

# To update combined 4545/LW/Series games data and PGN
# - SaveLeaguePGN(leagues = "teamlwseries")

# To update combined 4545/LW/Series/RB/Quest games data and PGN
# - SaveLeaguePGN(leagues = "tlsrq")









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
# Last updated on 2022-11-27. Includes RB 17, due to conclude by 30 Nov 2022.
# TODO: Replace this once the RB lookup CSV has been completed.

rb_sheets <- tibble(
  "season" = c(1:18),
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
    "1y2cQTKyMN01nLCRDXbv7Rq8LZ1c-xvarO335ruhuYpE", # S16 
    "1UsO4lIhJz2ml0JdKXjnB5pBtLKnyDqsmrwPgijsDsAY"  # S17
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
    list("Div A Group", "Div B Group", "Div A Playoffs", "Div B Playoffs"),
    list("Div A Group", "Div B Group", "Div A Playoffs", "Div B Playoffs")
  ),
  "seasons" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 11, 12, 13, 14, 15, 16, 17)
  # S9 has to appear twice to account for the additional S9 loser's bracket sheet
)

# ---- QUEST HISTORY PARAMETERS -----------------------------------------------

quest_sheet_id <- "1Y_jYuHJUnDqfYMpO3YIainspTJa7o3hgd6smeiHfe20"
quest_sheet_name <- "Results"


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
    Sys.sleep(0.1)
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
  # NOTE: this also returns gamelinks of un-analysed games. These should be analysed first, then the
  # previous line should be re-run to ensure new_data includes the newly analysed games 
  
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



UpdateAllRapidBattleGames <- function(rb_sheets,
                                      report_unanalysed = F,
                                      stop_if_unanalysed = F){
  
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
    if(report_unanalysed){
      new_unanalysed <- new_data[[2]] %>% 
        tibble::as_tibble() %>% select(id) %>% distinct(id) %>% arrange(id) %>% 
        dplyr::pull() 
      
      cli::cli_alert("{length(new_unanalysed)} new Rapid Battle games haven't yet been analysed by Lichess.")
      cli::cli_alert("You should request analysis for these games before adding them to the dataset.")
      
      # Save unanalysed game IDs in local TXT file
      readr::write_lines(new_unanalysed, 
                         paste0(here::here(), "/data/misc/unanalysed_gamelinks.txt"))
      cli::cli_inform("These gamelinks have been saved to /data/misc/unanalysed_gamelinks.txt")
    }
    
    if(stop_if_unanalysed){
      break
    }
    
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
    if(report_unanalysed){
      new_unanalysed <- new_data[[2]] %>% tibble::as_tibble()
      cli::cli_alert("{nrow(new_unanalysed)} Quest games haven't yet been analysed by Lichess.")
      cli::cli_alert("You should request analysis for these games before adding them to the dataset.")
    }
    
    if(stop_if_unanalysed){
      break
    }
    
    # ---- Add new games data to all-time data ------------------------------------
    
    # Tidy new games data
    new_games <- TidyGames(new_games)
    
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


UpdateQuestGames <- function(report_unanalysed = TRUE,
                             stop_if_unanalysed = FALSE){
  
  # Update all-time Infinite Quest games dataset
  tic("Updated all-time Quest games data")
  cli_h1("COMPILE / UPDATE INFINITE QUEST ALL-TIME GAMES DATA")
  cli_h2("Step 1 - Identify all Quest games from Quest history spreadsheet")
  
  sheetdata <- range_read_cells(
    ss = quest_sheet_id, 
    sheet = quest_sheet_name,
    cell_data = "full",
    discard_empty = FALSE
  )
  
  # Extract gamelinks
  contents <- sheetdata$cell
  contents <- paste0(unlist(contents), collapse = "")
  links <- stringr::str_extract_all(contents, "https://lichess.org/[:alnum:]{8}")
  links <- links[[1]]
  links <- unique(links)
  cli::cli_inform("Quest: extracted {length(links)} gamelinks")
  Sys.sleep(2)
  
  cli_h2("Step 2 - Identify games to add to all-time games data")
  
  # Read current all-time Quest games data (if file exists)
  data_exists <- fs::dir_info(paste0(here::here(), "/data/")) %>% 
    filter(type == "file") %>% 
    filter(str_detect(path, "allgames_quest.rds")) %>% 
    nrow()
  data_exists <- ifelse(data_exists == 0, FALSE, TRUE)
  
  if(data_exists){
    
    current_allgames <- readRDS(paste0(here::here(), "/data/allgames_quest.rds"))
    current_allgames <- tibble::as_tibble(current_allgames)
    
    current_oldest <- current_allgames %>% slice_min(date) %>% select(date) %>% dplyr::pull()
    current_latest <- current_allgames %>% slice_max(date) %>% select(date) %>% dplyr::pull()
    cli::cli_inform("Reading current Quest all-seasons games data...")
    cli::cli_inform("The current dataset has {nrow(current_allgames)} games.")
    cli::cli_inform("Oldest game: {current_oldest}. Most recent game: {current_latest}.")
    
    # Identify any Quest gamelinks identified that aren't in the all-time data
    new <- setdiff(links, paste0("https://lichess.org/", current_allgames$id))
    cli::cli_inform("{length(new)} Quest games aren't in the current all-time dataset")
    
    # Request data on new games
    new_data <- GetGamesFromURLs(links = new)
    new_games <- new_data[[1]] %>% tibble::as_tibble()
    
    # ---- Check for un-analysed new games ----------------------------------------
    if(report_unanalysed){
      new_unanalysed <- new_data[[2]] %>% tibble::as_tibble()
      cli::cli_alert("{nrow(new_unanalysed)} Quest games haven't yet been analysed by Lichess.")
      cli::cli_alert("You should request analysis for these games before adding them to the dataset.")
    }
    
    if(stop_if_unanalysed){
      break
    }
    
    # ---- Add new games data to all-time data ------------------------------------
    
    # Tidy new games data
    new_games <- TidyGames(new_games)
    
    # Add new games to all-time data
    new_allgames <- data.table::rbindlist(list(current_allgames,
                                               new_games),
                                          fill = T) %>% 
      tibble::as_tibble()
    # new_allgames <- dplyr::left_join(new_allgames, all_games, by = c("id"))
    
    # ---- Save updated all-time games data --------------------------------------- 
    saveRDS(new_allgames, 
            paste0(here::here(), "/data/allgames_quest.rds"))
    
    cli::cli_alert_success("Quest all-time games data updated and saved - now contains {nrow(new_allgames)} games.")
    
  } else {
    
    cli::cli_inform("No all-time games dataset found, so all identified games need to be saved.")
    
    # ---- Get data on new games --------------------------------------------------
    
    # Request data on new games
    new_data <- GetGamesFromURLs(links = links)
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
    
    if(report_unanalysed){
      new_unanalysed <- new_data[[2]] %>% tibble::as_tibble()
      cli::cli_alert("{nrow(new_unanalysed)} Quest games haven't yet been analysed by Lichess.")
      cli::cli_alert("You should request analysis for these games before adding them to the dataset.")
    }
    
    if(stop_if_unanalysed){
      break
    }
    
    # ---- Add new games data to all-time data ------------------------------------
    
    # Tidy new games data
    new_games <- TidyGames(new_games)
    # new_allgames <- dplyr::left_join(new_games, all_games, by = c("id"))
    
    # ---- Save updated all-time games data ---------------------------------------
    saveRDS(new_games,
            paste0(here::here(), "/data/allgames_quest.rds"))
    
    cli::cli_alert_success("Quest all-time games data saved for the first time ({nrow(new_games)} games)")
    
  }
  
  toc(log = TRUE)
  
}




SaveLeaguePGN <- function(leagues){
  
  # Options for "leagues": "team4545", "lwopen", "lwu1800", "series", "rb", "quest",
  # "teamlwseries", "tlsrq"
  
  # Save a PGN of all games played in a league (4545, LW Open, LW U1800 or Chess960)
  # Or a combination of leagues
  
  allgames_label <- paste0("allgames_", as.character(leagues))
  
  if(leagues %in% c("team4545", "lwopen", "lwu1800", "series", "rb", "quest", "chess960")){
    tic(paste0("Save all-time games PGN for ", leagues))
    league_games <- readRDS(paste0(here::here(), "/data/allgames_", leagues, ".rds"))
    league_games <- league_games %>% tibble::as_tibble()
    
  }
  
  if(leagues == "teamlwseries"){
    tic(paste0("Save all-time games PGN for 4545/LW/Series"))
    # Read and collate 4545 / LW / Series all-time games datasets
    team4545_games <- readRDS(paste0(here::here(), "/data/allgames_team4545.rds")) %>% select(pgn)
    lwopen_games <- readRDS(paste0(here::here(), "/data/allgames_lwopen.rds")) %>% select(pgn)
    lwu1800_games <- readRDS(paste0(here::here(), "/data/allgames_lwu1800.rds")) %>% select(pgn)
    series_games <- readRDS(paste0(here::here(), "/data/allgames_series.rds")) %>% select(pgn)
    league_games <- rbind(team4545_games, lwopen_games, lwu1800_games, series_games)
    saveRDS(league_games, paste0(here::here(), "/data/", "allgames_teamlwseries.rds"))
    cli::cli_inform("Saved all-time 4545/LW/Series games data ({nrow(league_games)} games)")
    league_games <- league_games %>% tibble::as_tibble()
  }
  
  if(leagues == "tlsrq"){
    tic(paste0("Save all-time games PGN for 4545/LW/Series/Rapid Battle/Quest"))
    cli::cli_h1("Combining prev saved games from 4545, LW, Series, RB and Quest")
    leagues <- c("team4545", "lwopen", "lwu1800", "series", "rb", "quest")
    league_games <- list()
    for (league in leagues) {
      games <- readRDS(paste0(here::here(), 
                              "/data/allgames_", 
                              league, 
                              ".rds"))
      games <- tibble::as_tibble(games) %>% 
        mutate(
          "acplw" = players.white.analysis.acpl,
          "acplb" = players.black.analysis.acpl,
          "sum_acpl" = acplw + acplb,
          "blundersw" = players.white.analysis.blunder,
          "blundersb" = players.black.analysis.blunder,
          "mistakesw" = players.white.analysis.mistake,
          "mistakesb" = players.black.analysis.mistake,
          "inaccuraciesw" = players.white.analysis.inaccuracy,
          "inaccuraciesb" = players.black.analysis.inaccuracy
        )
      league_games[[which(leagues == league)]] <- games
    }
    
    league_games <- data.table::rbindlist(league_games, fill = T)
    
    league_games <- league_games %>% 
      tibble::as_tibble() %>% 
      filter(perf %in% c("rapid", "classical")) # exclude blitz games from combined data
    
    saveRDS(league_games, paste0(here::here(), "/data/allgames_tlsrq.rds"))
    cli::cli_inform("Saved all-time 4545/LW/Series/RB/Quest games data ({nrow(league_games)} games)")
  }
  
  
  
  # Make a first attempt to remove evals and clock times from PGN data
  league_games$pgn <- str_replace_all(league_games$pgn, 
                                      "\\{ \\[%eval [:graph:]{1,}\\] \\[%clk [0-1]{1}:[0-5]{1}[0-9]{1}:[0-5]{1}[0-9]{1}\\] \\}", 
                                      "") %>% 
    str_replace_all("\\s\\s\\d+\\.\\.\\.", "") %>% 
    str_replace_all("\\s(?=\\d+\\.)", "") %>% 
    str_replace_all("\\s(?=1-0)", "") %>% 
    str_replace_all("\\s(?=1/2)", "") %>% 
    str_replace_all("\\s(?=0-1)", "")
  
  # Obtain PGN string
  pgn <- str_c(league_games$pgn, collapse = "")
  
  # Save this PGN to /data/
  fileConn <- file(paste0(here::here(), "/data/", allgames_label, ".pgn"))
  writeLines(pgn, fileConn)
  close(fileConn)
  
  # Then use python-chess to save a properly cleaned PGN without evals, clock times or errors
  SaveCleanPGN(filename = paste0(allgames_label), 
               new_filename = paste0(allgames_label))
  
  toc(log = TRUE)
  
}

###############################################################################


# ---- CALL FUNCTIONS --------------------------------------------------------- 
# league_choice options:
# "team4545", "lwopen", "lwu1800", "chess960"

# Compile all previously obtained season datasets into league-specific all-time
# datasets for 4545, LW and Chess960

# # 4545
# CompileCombinedData(league_choice = "team4545")
# SaveLeaguePGN("team4545") # took 6m 07s for S2-S37 (2024-01-03)
# 
# # LW Open
# CompileCombinedData(league_choice = "lwopen")
# SaveLeaguePGN("lwopen") # took 2m 43s for S1-S31 (2024-01-03)
# 
# # LW U1800
# CompileCombinedData(league_choice = "lwu1800")
# SaveLeaguePGN("lwu1800") # took 1m 15s for S9-S31 (2024-01-03)

# Chess960
# CompileCombinedData(league_choice = "chess960")
# SaveLeaguePGN("chess960")

# # Update all-seasons Series games data (.rds)
# UpdateAllSeriesGames(series_sheets)
# SaveLeaguePGN("series") # took 0m 24s on 2024-01-03

# # Update all-seasons Rapid Battle games data (.rds)
# UpdateAllRapidBattleGames(rb_sheets, 
#                           report_unanalysed = T,
#                           stop_if_unanalysed = T)
# SaveLeaguePGN("rb")

# Collate all-time 4545/LW/Series games data into single RDS file
# CombineTeamLWSeriesGames()

# Save single PGN with all 4545, LW (both sections) and Series games (no evals or clock times)
# SaveTeamLWSeriesPGN()

# Update Infinite Quest all-times games dataset
# UpdateQuestGames(report_unanalysed = T, stop_if_unanalysed = T)
# SaveLeaguePGN("quest")

# Combine all prev. saved games in 4545/LW/Series/RB/Quest into one RDS file
# CombineTeamLWSeriesRBQuestGames()



                 