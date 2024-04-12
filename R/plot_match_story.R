
# =============================================================================
#                         PLOT 4545 MATCH STORIES
# =============================================================================

# Plots the 'story' of matches played in the Lichess4545 Team League.
# Shows a match as a sequence of moves played in successive games, showing the 
# evaluation trajectory of each game as it reaches its result (1-0, 0-1, 
# 1/2-1/2, or other), while also showing how the overall match reached its final
# score. Made by essentially stitching together the evaluation graphs of all the
# games played in a match, ordering each game by the time it finished.


# OUTPUT(S)
# Produces a single PDF output. Optimised for the production of outputs for
# https://rahulan-c.github.io/lichess4545-stats/. 


# NOTES
# [1] Won't work for a round that hasn't been closed, because it requests team 
#     position data for the *following* round too, so obvs if a season's still 
#     in Round 1, there won't be any data for Round 2.
# [2] Remember to check for any un-analysed games in a round before creating
#     stories, using LeagueGames(). Process: 
#     - Run LeagueGames() once and identify games that require analysis
#     - Manually request server analysis for these games
#     - Once all games have been analysed, then run PlotMatchStory()


# Setup =======================================================================

# Load (extra) packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggrepel, cowplot, ggtext, gridExtra, qpdf, rsvg, extrafont, 
               cli, grImport2, png, ggimage, grid, prettyunits, svglite)

# Get env variables
token <- Sys.getenv("LICHESS_TOKEN")       # Lichess API token
gspath <- Sys.getenv("GHOSTSCRIPT_PATH")   # Ghostscript installation path (to embed fonts in PDF)

# Load scripts/all_functions.R (for get_league_data())
source(glue::glue("{here::here()}/R/all_functions.R"))

# Load fonts
# NEED TO HAVE FONTS ALREADY INSTALLED ON MACHINE
extrafont::loadfonts(device = "pdf", quiet = TRUE)

# Functions ===================================================================

## Fix character encoding issues ----------------------------------------------
fix_character_encoding <- function(df){
  
  # Approach 1: manually change wrong characters to correct Unicode
  df <- df %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "Ã¼", "\u00fc"))) %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "Ã¶", "\u00f6"))) %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "Ã³", "\u00f3"))) %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "Ã©", "\u00e9"))) %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "â€¾", "\u203e"))) %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "ãƒ„", "\u30c4"))) %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "â€™", "\u0027"))) %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "’", "\u0027"))) %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "Ã¤", "\u00e4")))
  
  # Approach 2: change encoding of strings in character columns to UTF-8 
  df <- df %>%
    mutate(across(where(is.character), ~ stringi::stri_enc_toutf8(.x)))
  return(df)
}

#' Makes a graphic visualising the story of a single Lichess4545 Team League 
#' match or a set of matches in a specific round. Produces a single PDF output.
#'
#' @param season_num The 4545 season to plot. Should be an integer.
#' @param round_num The round containing the match(es) to plot. Should be an 
#' integer between 1 and 8.
#' @param plot_whole_round Select TRUE to make plots for all matches in the 
#' selected round. Each match plot will take up a page in the final PDF. Select 
#' FALSE to only plot a single match. Default value: FALSE.
#' @param method The desired method of choosing a match to plot. Can take either 
#' "team", "number" or "top". Choose "team" to plot a match played by a specific 
#' team. Choose "number": to plot a match based on its order in the round's list 
#' of pairings. Choose "top" to plot the top match in the round.
#' @param details Details accompany user's choice of "method". For the method 
#' "teams", enter the relevant team name (eg "Tima's Titans"). For the method 
#' "number", enter the relevant match number (eg 3). If the method is "top", this
#' argument doesn't need to be entered. Default value: NULL.
#' @param request_data Whether game and team ranking data needs to be requested 
#' from the Lichess4545 API, website and Lichess before making the plot. 
#' Enter TRUE to request data. Default value: FALSE.
#' @param plot_width Final plot width in mm. Default value: 350.
#' @param plot_height Final plot height in mm. Default value: 215.
#' @param plot_format Final plot format. Default value: "pdf".
#' @param save Whether to save the final PDF locally or not. Default value: TRUE.
#' @param save_path Filepath for saving the final PDF, relative to the current 
#' working directory.
#' @return
#' @export
#'
#' @examples
#' PlotMatchStory(27, 5, method = "team", details = "Tima's Titans", request_data = T)
PlotMatchStory <- function(season_num, # season number
                           round_num,
                           plot_whole_round = F,
                           method,
                           details = NULL,
                           request_data = F,
                           plot_format = "pdf",
                           plot_width = 350, # 350, 420
                           plot_height = 215, # 210, 297
                           save = T,
                           save_path = paste0("/docs/reports/stories/prod/")) {

  # Colours
  team1_col <- "#58508d"           
  team2_col <- "#bc5090"            
  draw_col <- "#707C80"             
  forfeit_unplayed_col <- "#707C80" # Match score labels and game IDs when games are not played or forfeited
  eval_col <- "#003f5c"             # Match eval line
  gamelink_col <- "white"           # Gamelink label text [game ID]
  gamelink_fill_col <- "#3C6478"    # Gamelink label fill
  gamelink_summary_col <- "blue"    # For the gamelinks in the match summary section
  opening_col <- "#373D3F"          # ECO and opening names
  gametime_col <- "#373D3F"         # Game start and end times
  rank_col <- "#A7B0B2"             # Team rank (title)
  titledetail_col <- "#C1C7C9"      # For season, round, month/year details in title
  gamedetail_col <- "#8C979A"
  about_col <- "#373D3F"            # "About" text
  icon_col <- "#A7B0B2"             # Icons, eg link/calendar/timer
  gamedividers_col <- "#555F61"
  equalityline_col <- "#555F61"
  
  # Fonts
  title_font <- "Carter One"
  titledetail_font <- "Lato"
  players_font <- "Kalam"
  gameinfo_font <- "Abel"
  matchinfo_font <- "Roboto"
  plotinfo_font <- "Abel"
  fixedwidth_font <- "Roboto Mono"
  
  # Delete all PDFs currently in the directory where the final match story file 
  # will be saved. This is required to prevent the PDF merge process from 
  # introducing unnecessary blank pages into the final product.
  existing_pdfs <- fs::dir_info(glue::glue(here::here(), save_path)) %>% 
    filter(type == "file") %>% 
    filter(str_detect(path, ".pdf")) %>% 
    select(path) %>% 
    dplyr::pull()
  fs::file_delete(existing_pdfs)


  # Extract and tidy match data =================================================


  # Get details of all round matches --------------------------------------------
  pairings_url <- paste0(
    "https://www.lichess4545.com/team4545/season/",
    as.character(season_num),
    "/round/",
    as.character(round_num),
    "/pairings/"
  )

  all_matches <- read_html(pairings_url) %>%
    rvest::html_element("table") %>%
    rvest::html_table(header = FALSE)
  all_matches$X5[all_matches$X5 == "Calendar"] <- ""
  
  all_matches <- fix_character_encoding(all_matches)
  
  # Remove player pairings (leaving only team names and match scores)
  all_matches <- all_matches %>%
    filter(
      X5 == "",
      if_any(c(X2, X3), ~ . != ""),
      if_any(c(X2, X3), ~ !(str_detect(., "Z|F|X|01|10|½½")))
           )
    

  # Identify match to plot ------------------------------------------------------
  
  all_matches$X2 <- all_matches$X2 %>% str_replace_all("\u00bd", ".5")
  all_matches$X3 <- all_matches$X3 %>% str_replace_all("\u00bd", ".5")
  all_matches_2 <- all_matches # for summarising round pairings
  
  
  if(!(plot_whole_round)) {
    if (method == "team") {
      all_matches <- all_matches %>%
        filter(X1 == details | X4 == details)
    }
    if (method == "top") {
      all_matches <- all_matches[1, ]
    }
    if (method == "number") {
      all_matches <- all_matches[details, ]
    }
  }

  # Get match pairings data -----------------------------------------------------
  if(request_data){
  r <- httr::GET(
    url = "https://www.lichess4545.com/api/get_season_games/",
    query = list(league = "team4545", season = season_num)
  )

  if (r$status_code != 200) {
    print("Error!")
    print(http_status(r)$message)
  }

  all_pairings <- r %>%
    httr::content("text", encoding = stringi::stri_enc_detect(httr::content(r, "raw"))[[1]][1, 1]) %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("games") %>%
    dplyr::filter(round == round_num)
  }
  
  all_pairings <- fix_character_encoding(all_pairings)
  all_pairings <- all_pairings %>% 
    mutate(across(everything(), 
                  ~ stringr::str_trim(.)))
  
  all_pairings <- all_pairings %>% 
    mutate(pairing = paste0(str_to_lower(white), "-", str_to_lower(black)))
  
  # Isolate individual match pairings data (unless plotting all matches in a round)
  if(!(plot_whole_round)) {
    all_pairings <- all_pairings %>%
      dplyr::filter(white_team == all_matches$X1 | black_team == all_matches$X1)
  }
  
  # Request game data from Lichess, splitting requests into chunks of max 300
  # ids where necessary
  if(request_data){
    
  game_ids <- all_pairings %>% 
    select(game_id) %>% 
    dplyr::pull()
  game_ids <- split(game_ids, ceiling(seq_along(game_ids)/300))
  
  # Get data on requested games
  lst_games <- list(rep(NA, length(game_ids)))
  
  for(l in seq(1:length(game_ids))){
    
    if(l > 1){Sys.sleep(5)}
    
    batch_ids <- game_ids[[l]] %>% str_c(collapse = ",")
    
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
    
    if(query$status_code != 200){
      print("Error!")
      print(http_status(query)$message)
      Sys.sleep(20)
      break}
    
    # Convert response to NDJSON
    game_data <- query %>% 
      httr::content("text", encoding = "UTF-8") %>% 
      read_lines() %>% 
      ndjson::flatten()
    
    lst_games[[l]] <- game_data
    print(paste0("Obtained game data batch ", l, "/", length(game_ids)))
    
  }
  
  # Combine game data from all batches
  all_games <- bind_rows(lst_games)
  rm(lst_games, game_ids)
  
  all_games <- as_tibble(all_games)

  # Tidy game data
  all_games <- TidyGames(all_games)
  all_games <- as_tibble(all_games)
  all_games <- fix_character_encoding(all_games)
  
  all_games <- all_games %>% arrange(ended)
  
  # Now get alternative pairing data that includes originally scheduled game times
  # from the Lichess4545 website
  # Used for correctly re-ordering unplayed forfeits
  
  # First, get the correct number of boards per team for get_league_data()
  team_boards <- 99
  if(season_num == 1){team_boards <- 4} else
    if(season_num == 2){team_boards <- 5} else
      if(season_num <= 15){team_boards <-  6} else 
        if(season_num <= 24){team_boards <-  8} else
          if (season_num <= 29){team_boards <- 10} else
            if(season_num <= 99){team_boards <- 8}
  
  # Then get league data using LeagueData() function scripts/all_functions.R (already sourced)
  league_data <- LeagueData("team4545", season_num, round_num, FALSE, team_boards)
  all_pairings2 <- league_data[[1]] %>% as_tibble()
  all_pairings2 <- fix_character_encoding(all_pairings2)
  
  all_pairings2 <- all_pairings2 %>% filter(round == round_num)
  all_pairings2 <- all_pairings2 %>% 
    mutate(result = glue::glue("{str_sub(result, 1, nchar(result)/2)}-{str_sub(result, (nchar(result)/2) + 1, nchar(result))}"))
  
  all_positions <- league_data[[2]] %>% as_tibble()
  all_positions_postmatch <- all_positions %>% filter(round == round_num)
  all_positions <- all_positions %>% filter(round == round_num - 1)
  
  
  }
  
  # Create empty list for match plots
  lst_plots <- vector("list", length = nrow(all_matches))
  
  # For each match to be plotted...
  for (m in seq(1:length(lst_plots))) {

    # Extract match data subset -----------------------------------------------
      
    # Match pairings
    pairings <- all_pairings %>% 
      filter(white_team == all_matches$X1[m] | black_team == all_matches$X1[m])
    
    # Match summary
    matches <- all_matches %>% 
      filter(X1 %in% pairings$white_team)
      
    # Match games
    games <- all_games %>% 
      filter(id %in% pairings$game_id)
    
    # Get pairings data with scheduled game times
    pairings2 <- all_pairings2 %>% 
      filter(str_detect(match, str_replace_all(pairings$white_team[1], "\\[|\\]", "")[[1]]) | 
               str_detect(match, str_replace_all(pairings$black_team[1], "\\[|\\]", "")[[1]]))
    
    # Fix the result column in the alt. pairings data
    # Required because raw data extracted from the Lichess4545 pairings page 
    # gives the wrong (reversed) result for all even-numbered boards. These 
    # need to be reversed. 
    results_first <- str_extract(pairings2$result, "^.+-") %>% str_remove("-")
    results_second <- str_extract(pairings2$result, "-.+$") %>% str_remove("-")
    pairings2$result2 <- paste0(results_second, 
                               rep("-", nrow(pairings2)),
                               results_first)
    rm(results_first, results_second)
    pairings2$result <- ifelse(pairings2$board %% 2 == 0,
                               pairings2$result2,
                               pairings2$result)
    pairings2$result2 <- NULL
    
    # Remove played forfeits from pairings and games data
    # Added back in as unplayed games so they can be shown just like unplayed 
    # forfeits
    played_forfeits <- pairings %>% filter(str_detect(result, "X|Z")) %>% select(game_id) %>% dplyr::pull()
    pairings <- pairings %>% filter(!(game_id %in% played_forfeits))
    games <- games %>% filter(!(id %in% played_forfeits))
    
    pairings3 <- pairings2 # only to keep board numbers for the plot, def ought to be done more cleanly
    
    # If any games weren't played, find where they should be assigned in the 
    # match sequence using their originally scheduled times
    if(TRUE %in% str_detect("forfeit/unplayed", unique(pairings2$winner))) {
      # Order the pairings by their scheduled times 
      pairings2$sched_time <- glue::glue("{as.character(lubridate::year(all_games$started[1]))}-{stringr::str_sub(pairings2$sched_time, 1, 2)}-{stringr::str_sub(pairings2$sched_time, 4, 5)} {stringr::str_sub(pairings2$sched_time, 7, 12)}")
      pairings2$sched_time <- lubridate::ymd_hm(pairings2$sched_time)
      pairings2 <- pairings2 %>% 
        arrange(sched_time) %>% 
        mutate(sched_order = seq(1:nrow(.))) %>% 
        filter(winner == "forfeit/unplayed") %>% 
        mutate(game_id = glue("forfeit-{row_number()}"))
    }
  
    # Order (played) games by time of last move, and combine with pairings data
    games_subset <- games %>%
      arrange(ended) %>%
      mutate(order =  row_number()) %>%
      select(id, order, "eco" = opening.eco, rating_w, rating_b, started, ended, opening.name,
             num_moves,
             "acpl_w" = players.white.analysis.acpl,
             "acpl_b" = players.black.analysis.acpl,
             "blunders_w" = players.white.analysis.blunder,
             "blunders_b" = players.black.analysis.blunder,
             "inaccuracies_w" = players.white.analysis.inaccuracy,
             "inaccuracies_b" = players.black.analysis.inaccuracy,
             "mistakes_w" = players.white.analysis.mistake,
             "mistakes_b" = players.black.analysis.mistake,
             duration, duration_w, duration_b, evals)
  
    pairings <- left_join(pairings, games_subset, by = c("game_id" = "id"))
    
    # Add unplayed/forfeited games to pairings data
    if(TRUE %in% str_detect("forfeit/unplayed", unique(pairings2$winner))) {
      pairings <- pairings %>% 
        add_row(league = rep("Lichess4545 League", nrow(pairings2)),
                season = as.character(pairings2$season),
                round = as.character(pairings2$round),
                game_id = pairings2$game_id,
                white = pairings2$white,
                black = pairings2$black, 
                result = pairings2$result,
                white_team = pairings2$team_w,
                black_team = pairings2$team_b,
                order = pairings2$sched_order,
                eco = rep("", nrow(pairings2)),
                rating_w = pairings2$rating_w,
                rating_b = pairings2$rating_b,
                started = pairings2$sched_time,
                ended = pairings2$sched_time,
                opening.name = rep("", nrow(pairings2))
                ) %>% 
        # re-order games
        arrange(ended) %>% 
        mutate(order = seq(1:nrow(.)))
    }
  
    # Define Teams 1 and 2 - by alphabetical order
    teams <- sort(unique((pairings$white_team)))
    
    # Extract each team's league rank, and points (match/game) in the league before the match took place
    rank_t1 <- all_positions %>% filter(team == teams[[1]]) %>% dplyr::pull(rank)
    mp_t1 <- all_positions %>% filter(team == teams[[1]]) %>% dplyr::pull(mp)
    gp_t1 <- all_positions %>% filter(team == teams[[1]]) %>% dplyr::pull(gp)
    rank_t2 <- all_positions %>% filter(team == teams[[2]]) %>% dplyr::pull(rank)
    mp_t2 <- all_positions %>% filter(team == teams[[2]]) %>% dplyr::pull(mp)
    gp_t2 <- all_positions %>% filter(team == teams[[2]]) %>% dplyr::pull(gp)
    
    # Extract each team's league rank and points (match/game) after the match
    newrank_t1 <- all_positions_postmatch %>% filter(team == teams[[1]]) %>% dplyr::pull(rank)
    newmp_t1 <- all_positions_postmatch %>% filter(team == teams[[1]]) %>% dplyr::pull(mp)
    newgp_t1 <- all_positions_postmatch %>% filter(team == teams[[1]]) %>% dplyr::pull(gp)
    newrank_t2 <- all_positions_postmatch %>% filter(team == teams[[2]]) %>% dplyr::pull(rank)
    newmp_t2 <- all_positions_postmatch %>% filter(team == teams[[2]]) %>% dplyr::pull(mp)
    newgp_t2 <- all_positions_postmatch %>% filter(team == teams[[2]]) %>% dplyr::pull(gp)
    
    # Fix any NA ranks
    rank_t1 <- ifelse(is.numeric(rank_t1[1]), rank_t1[1], 99)
    rank_t2 <- ifelse(is.numeric(rank_t1[1]), rank_t2[1], 99)
    newrank_t1 <- ifelse(is.numeric(newrank_t1[1]), newrank_t1[1], 99)
    newrank_t2 <- ifelse(is.numeric(newrank_t2[1]), newrank_t2[1], 99)
    

    pairings$t1 <- rep(teams[1], nrow(pairings))
    pairings$t2 <- rep(teams[2], nrow(pairings))
    pairings <- pairings %>%
      arrange(order) %>%
      mutate(pts_t1 = case_when( # game points (0, 0.5, 1)
        result == "1-0" & white_team == t1 ~ 1,
        result == "0-1" & white_team == t1 ~ 0,
        result == "1-0" & white_team == t2 ~ 0,
        result == "0-1" & white_team == t2 ~ 1,
        result == "1/2-1/2" ~ 0.5,
        result == "1X-0F" & white_team == t1 ~ 1,
        result == "1X-0F" & white_team == t2 ~ 0,
        result == "0F-1X" & white_team == t1 ~ 0,
        result == "0F-1X" & white_team == t2 ~ 1,
        result == "0F-0F" ~ 0,
        str_count(result, "Z") == 2 ~  0.5,  # scheduling draws (½Z-½Z)
        TRUE ~ NA_real_
      )) %>%
      mutate(pts_t2 = ifelse(result == "0F-0F", 0, 1 - pts_t1)) %>%
      mutate(
        postgamescore_t1 = cumsum(pts_t1), # team match score after game
        postgamescore_t2 = cumsum(pts_t2),
        pregamescore_t1 = postgamescore_t1 - pts_t1, # team match score before game
        pregamescore_t2 = postgamescore_t2 - pts_t2,
        pregamediff = pregamescore_t1 - pregamescore_t2
      ) # diff. btw teams' match scores before game
    
    if(pairings$t1[1] == matches$X1){
      pairings$finalscore_t1 <- as.numeric(matches$X2)
      pairings$finalscore_t2 <- as.numeric(matches$X3)
    } else if (pairings$t1[1] == matches$X4) {
      pairings$finalscore_t1 <- as.numeric(matches$X3)
      pairings$finalscore_t2 <- as.numeric(matches$X2)
    }
  
    # Collate moves data from all games
    moves <- data.table::rbindlist(games$evals)
    moves <- tibble::as_tibble(moves)
    
    moves <- moves %>%
      dplyr::mutate(colour = case_when(
        ply %% 2 == 0 ~ "white",
        ply %% 2 == 1 ~ "black",
        TRUE ~ NA_character_
      ))
  
    pairings_sub <- pairings %>%
      dplyr::select(
        game_id, white, black, result, order, rating_w, rating_b, white_team, black_team, t1,
        t2, pregamediff, pts_t1, pts_t2, postgamescore_t1,
        postgamescore_t2, finalscore_t1, finalscore_t2, eco, started, ended,
        opening.name
      )
    
    
  
    moves <- dplyr::left_join(moves, pairings_sub, by = c("game_id")) %>%
      mutate(player = ifelse(colour == "white", white, black))
    
    # Make in-game evals (scaled) correspond to team "colours" (instead of player colours)
    moves <- moves %>%
      arrange(order, ply) %>% 
      mutate(eval_scaled_team = ifelse(white_team == t1, eval_scaled,
        (-1 * eval_scaled)
      )) %>%
      # Compute a scaled match eval measure - this is what gets plotted as the eval line
      mutate(eval_scaled_match = ifelse(white_team == t1, pregamediff + eval_scaled,
        pregamediff + (-1 * eval_scaled)
      ))
    
    
    # Account for unplayed games...
    if(length(unique(moves$order)) != team_boards){
      
      cli::cli_alert_warning("Found {nrow(pairings2)} unplayed game(s) in {pairings2$match[1]}:")
      
      # Add a set of moves for each unplayed game (so they're shown separately on the plot)
      uf <- pairings %>% 
        filter(str_detect(game_id, "forfeit"))

      for(u in seq(1:nrow(uf))){
        
        cli::cli_alert_warning("Accounting for {uf$white[u]}-{uf$black[u]} {uf$result[u]}")
          
        # Find where the missing games should go
        
        if(uf$order[u] == team_boards) {
          move_before_unplayed <- moves[nrow(moves),]
          row_before_unplayed <- nrow(moves)
          
        } else if(uf$order[u] == 1) {
          move_before_unplayed <- moves[1,]
          row_before_unplayed <- 0
        
        } else {
          # Extract details from the row just above where the unplayed moves should go
          move_before_unplayed <- moves %>% 
            mutate(orig_row = row_number()) %>% 
            filter(order == uf$order[u] - 1) %>% 
            arrange(desc(orig_row)) %>% 
            head(1) 
          row_before_unplayed <- move_before_unplayed %>% 
            select(orig_row) %>% 
            dplyr::pull()
        }
        
        # Get times of the unplayed game
        unplayed_schedtime <- uf[u,] %>% 
          select(started) %>% 
          dplyr::pull()
      
        # Insert 30 rows representing moves in the unplayed game
        moves <- moves %>% 
          add_row(game_id = rep(uf$game_id[u], 30),
                  order = rep(uf$order[u], 30),
                  result = rep(uf$result[u], 30),
                  white = rep(uf$white[u], 30),
                  black = rep(uf$black[u], 30),
                  rating_w = rep(uf$rating_w[u], 30),
                  rating_b = rep(uf$rating_b[u], 30),
                  white_team = rep(uf$white_team[u], 30),
                  black_team = rep(uf$black_team[u], 30),
                  t1 = rep(uf$t1[u], 30),
                  t2 = rep(uf$t2[u], 30),
                  pregamediff = rep(uf$pregamediff[u], 30),
                  pts_t1 = rep(uf$pts_t1[u], 30),
                  pts_t2 = rep(uf$pts_t2[u], 30),
                  postgamescore_t1 = rep(uf$postgamescore_t1[u], 30),
                  postgamescore_t2 = rep(uf$postgamescore_t2[u], 30),
                  finalscore_t1 = rep(uf$finalscore_t1[u], 30),
                  finalscore_t2 = rep(uf$finalscore_t2[u], 30),
                  eco = rep("", 30),
                  started = rep(unplayed_schedtime, 30),
                  ended = rep(unplayed_schedtime, 30),
                  opening.name = rep("", 30),
                  
                  # Insert the fake moves data in the right place to reflect when the 
                  # game was meant to be played
                  .after = row_before_unplayed)
        
        # Add ply number (0 to 29)
        moves$ply[moves$order == uf$order[u]] <- seq(from = 0, to = 29)
        
        # Add player name per move
        moves$player[moves$order == uf$order[u]] <- ifelse(
          moves$ply[moves$order == uf$order[u]] %% 2 == 0,
          moves$white[moves$order == uf$order[u]][1],
          moves$black[moves$order == uf$order[u]][1]
        )
        
        # print(glue::glue("{u} {uf$pregamediff[u]} {uf$postgamescore_t1[u] - uf$postgamescore_t2[u]}"))
        
        # Add match eval
        moves$eval_scaled_match[moves$order == uf$order[u]] <- seq(
          from = uf$pregamediff[u],
          to = uf$postgamescore_t1[u] - uf$postgamescore_t2[u],
          length.out = 30
            )
      }
    }
    
    # Final move should have an eval_scaled_match value consistent with the final match score
    moves$eval_scaled_match[nrow(moves)] <- moves$finalscore_t1[nrow(moves)] - moves$finalscore_t2[nrow(moves)]
  
    # Ensure players/games are equally spaced from each other in the final plot
    max_ply <- max(moves$ply)
    moves <- moves %>%
      group_by(game_id) %>%
      mutate(game_plies = max(ply)) %>%
      mutate(rev_ply = max_ply * (ply / game_plies)) %>%
      mutate(ply_match = ifelse(order == 1, rev_ply, (max_ply * (order - 1)) + rev_ply))
    
    # Make y-axis consistent height across plots while retaining each matches's range of margins
    y_scale_factor <- 5
    min_eval <- min(moves$eval_scaled_match)
    max_eval <- max(moves$eval_scaled_match)
    y_range <- max_eval - min_eval
    moves <- moves %>% 
      mutate(y_val = ((eval_scaled_match - min_eval) / y_range) * y_scale_factor
      )
    
    zero_val <- ((0 - min(moves$eval_scaled_match)) / y_range) * y_scale_factor
  
    # Supporting data for plot
    moves_extra <- moves %>%
      group_by(game_id) %>%
      summarise(
        first_ply = min(ply_match),
        last_ply = max(ply_match),
        start_margin = min(pregamediff),
        min_scaled_eval = min(y_val),
        max_scaled_eval = max(y_val),
        start_eval = first(y_val),
        final_eval = last(y_val),
        game_max_yrange = ((first(y_val) / y_scale_factor) * y_range) + min_eval + 1,
        game_min_yrange = ((first(y_val) / y_scale_factor) * y_range) + min_eval - 1,
        player_1 = ifelse(t1[1] == white_team[1], paste0(white[1]), paste0(black[1])),
        player_2 = ifelse(t2[1] == white_team[1], paste0(white[1]), paste0(black[1])),
        colour_1 = ifelse(t1[1] == white_team[1], paste0("W"), paste0("B")),
        colour_2 = ifelse(t2[1] == white_team[1], paste0("W"), paste0("B")),
        pts_t1 = min(pts_t1),
        pts_t2 = min(pts_t2),
        result = result[1],
        score = paste0(postgamescore_t1[1], "-", postgamescore_t2[1]),
        game_order = order[1],
        started = started[1],
        ended = ended[1],
        eco = eco[1],
        opening = opening.name[1],
        rating_1 = ifelse(t1[1] == white_team[1], paste0(rating_w[1]), paste0(rating_b[1])),
        rating_2 = ifelse(t2[1] == white_team[1], paste0(rating_w[1]), paste0(rating_b[1]))
      ) %>%
      arrange(first_ply) %>%
      mutate(end_margin = lead(start_margin))
  
    # Ensure that the end_margin value for the final game reflects the match result
    moves_extra$end_margin[nrow(moves_extra)] <- moves$finalscore_t1[1] - moves$finalscore_t2[1]
    
    # Some unnecessary fiddling to get board numbers into moves_extra to show in the final plot
    moves_extra <- moves_extra %>% 
      mutate(white = ifelse(colour_1 == "W", player_1, player_2),
             black = ifelse(colour_1 == "B", player_1, player_2))
    pairings3 <- pairings3 %>% select(board, white, black)
    moves_extra$white <- stringr::str_to_lower(moves_extra$white)
    moves_extra$black <- stringr::str_to_lower(moves_extra$black)
    moves_extra <- dplyr::left_join(moves_extra, pairings3, by = c("white", "black"))
    
    missed_games <- 0
    
  
    # Details for plot text labels
    moves_plus <- moves_extra %>%
      select(game_id, game_order, player_1, player_2, result, first_ply, last_ply, score) %>%
      unique() %>%
      filter(!(is.na(game_id)))
  
    # Get final match score for printing (convert .5 to fraction)
    finalscore_t1 <- moves$finalscore_t1[1] %>% str_replace("\\.5", "\u00bd")
    finalscore_t2 <- moves$finalscore_t2[1] %>% str_replace("\\.5", "\u00bd")
  
    # 
    moves_extra$score <- moves_extra$score %>%
      str_replace_all("\\.5", "\u00bd") %>%
      str_replace_all(paste0("0", "\u00bd"), "\u00bd")
    
    # Convert 0.5 player to points to ½ ("\u00bd")
    moves_extra$pts_t1_print <- moves_extra$pts_t1 %>% 
      stringr::str_replace("0.5", "\u00bd")
    moves_extra$pts_t2_print <- moves_extra$pts_t2 %>% 
      stringr::str_replace("0.5", "\u00bd")
    # Ensure Fs, Xs and Zs show up in the final game summary results
    moves_extra <- moves_extra %>% 
      mutate(pts_t1_print = ifelse(str_detect(game_id, "forfeit"), 
                                   ifelse(pts_t1 == 1, "1X",
                                          ifelse(pts_t1 == 0, "0F",
                                                 ifelse(pts_t1 == 0.5, paste0("\u00bd", "Z"),
                                                        pts_t1_print))),
                                   pts_t1_print)) %>% 
               mutate(pts_t2_print = ifelse(str_detect(game_id, "forfeit"), 
                                            ifelse(pts_t2 == 1, "1X",
                                                   ifelse(pts_t2 == 0, "0F",
                                                          ifelse(pts_t2 == 0.5, paste0("\u00bd", "Z"),
                                                                 pts_t2_print))),
                                            pts_t2_print))

    
    # Get min and max evals in match
    min_eval <- min(moves_extra$min_scaled_eval)
    max_eval <- max(moves_extra$max_scaled_eval)
  
    # When the first game was played
    match_started <- paste0(
      as.character(lubridate::wday(pairings$started[1], label = TRUE, week_start = 1)), "\n",
      as.character(sprintf("%02d", lubridate::hour(pairings$started[1]))), ":",
      as.character(sprintf("%02d", lubridate::minute(pairings$started[1]))), ""
    )
  
    # When the last game was played
    match_ended <- paste0(
      as.character(lubridate::wday(pairings$ended[length(pairings$ended)], label = TRUE, week_start = 1)), "\n",
      as.character(sprintf("%02d", lubridate::hour(pairings$ended[length(pairings$ended)]))), ":",
      as.character(sprintf("%02d", lubridate::minute(pairings$ended[length(pairings$ended)]))), ""
    )
    
    # Print match summary info
    cli::cli_alert_success("Obtained data for {str_trunc(teams[[1]], 8, 'right')} ({moves$finalscore_t1[1]}) vs {str_trunc(teams[[2]], 8, 'right')} ({moves$finalscore_t2[1]})")
    
    # Compile statistics for plot ---------------------------------------------
    
    # Data to show:
    # TEAM-SPECIFIC STATS:
    # - pre-match rank
    # - team ACPLs
    # # team blunder rates
    # - moves played in time trouble
    # MATCH-RELATED STATS
    # - avg game length (time)
    # - # White wins, # draws, # Black wins
    
    team_stats <- pairings %>% 
      mutate(acpl_t1 = ifelse(white_team == teams[1], acpl_w, acpl_b),
             acpl_t2 = ifelse(white_team == teams[2], acpl_w, acpl_b),
             inaccuracies_t1 = ifelse(white_team == teams[1], inaccuracies_w, inaccuracies_b),
             inaccuracies_t2 = ifelse(white_team == teams[2], inaccuracies_w, inaccuracies_b),
             mistakes_t1 = ifelse(white_team == teams[1], mistakes_w, mistakes_b),
             mistakes_t2 = ifelse(white_team == teams[2], mistakes_w, mistakes_b),
             blunders_t1 = ifelse(white_team == teams[1], blunders_w, blunders_b),
             blunders_t2 = ifelse(white_team == teams[2], blunders_w, blunders_b),
             duration_t1 = ifelse(white_team == teams[1], duration_w, duration_b),
             duration_t2 = ifelse(white_team == teams[2], duration_w, duration_b)
             ) %>% 
      select(game_id, num_moves, duration_t1, duration_t2, acpl_t1, acpl_t2, 
             inaccuracies_t1, inaccuracies_t2, mistakes_t1, mistakes_t2, 
             blunders_t1, blunders_t2) %>% 
      filter(!(stringr::str_detect(game_id, "forfeit"))) %>% 
      mutate(cpl_t1 = acpl_t1 * num_moves,
             cpl_t2 = acpl_t2 * num_moves) %>% 
      as_tibble()
    
    gamestats_t1 <- team_stats %>%
      select(num_moves, duration_t1, acpl_t1, cpl_t1, inaccuracies_t1, mistakes_t1, blunders_t1)
    gamestats_t2 <- team_stats %>% 
      select(num_moves, duration_t2, acpl_t2, cpl_t2, inaccuracies_t2, mistakes_t2, blunders_t2)
    
    acpl_t1 <- sum(gamestats_t1$cpl_t1) / sum(gamestats_t1$num_moves)
    acpl_t2 <- sum(gamestats_t2$cpl_t2) / sum(gamestats_t2$num_moves)
    
    clock_t1 <- sum(gamestats_t1$duration_t1)
    clock_t2 <- sum(gamestats_t2$duration_t2)
    
    match_duration <- clock_t1 + clock_t2
    
    pcclock_t1 <- clock_t1 / match_duration
    pcclock_t2 <- clock_t2 / match_duration
    
    forfeitwins_t1 <- moves_extra %>% 
      filter(str_detect(game_id, "forfeit")) %>% 
      filter(pts_t1 == 1) %>% 
      nrow()
    
    forfeitwins_t2 <- moves_extra %>% 
      filter(str_detect(game_id, "forfeit")) %>% 
      filter(pts_t2 == 1) %>% 
      nrow()
    
    wins_t1 <- moves_extra %>% 
      filter(pts_t1 == 1) %>% 
      nrow()
    
    wins_t1 <- wins_t1 - forfeitwins_t1
    
    wins_t2 <- moves_extra %>% 
      filter(pts_t2 == 1) %>% 
      nrow()
    
    wins_t2 <- wins_t2 - forfeitwins_t2
    
    forfeitlosses_t1 <- forfeitwins_t2
    forfeitlosses_t2 <- forfeitwins_t1
    losses_t1 <- wins_t2
    losses_t2 <- wins_t1
    
    draws_t1 <- moves_extra %>% 
      filter(pts_t1 == 0.5) %>% 
      nrow()
    
    scheddraws_t1 <- moves_extra %>% 
      filter(str_detect(game_id, "forfeit")) %>% 
      filter(pts_t1 == 0.5) %>% 
      nrow()
    
    draws_t1 <- draws_t1 - scheddraws_t1
    draws_t2 <- draws_t1
    scheddraws_t2 <- scheddraws_t1
    
    match_stats <- tibble(
      "Team" = c(teams[[1]], teams[[2]]),
      "Pts" = c(finalscore_t1, finalscore_t2),
      "W" = c(wins_t1, wins_t2),
      "L" = c(losses_t1, losses_t2),
      "D" = c(draws_t1, draws_t2),
      "FW" = c(forfeitwins_t1, forfeitwins_t2),
      "FL" = c(forfeitlosses_t1, forfeitlosses_t2),
      "FD" = c(scheddraws_t1, scheddraws_t2),
      "Clock" = c(prettyunits::pretty_sec(clock_t1), prettyunits::pretty_sec(clock_t2)),
      # "Clock %" = c(paste0(round(pcclock_t1,1), "%"), paste0(round(pcclock_t2, 1), "%")),
      "ACPL" = c(round(acpl_t1, 1), round(acpl_t2, 1)),
      "Inaccuracies" = c(paste0(round((sum(team_stats$inaccuracies_t1) / sum(team_stats$num_moves)) * 100, 1), "%"),
                         paste0(round((sum(team_stats$inaccuracies_t2) / sum(team_stats$num_moves)) * 100, 1), "%")),
      "Mistakes" = c(paste0(round((sum(team_stats$mistakes_t1) / sum(team_stats$num_moves)) * 100, 1), "%"),
                         paste0(round((sum(team_stats$mistakes_t2) / sum(team_stats$num_moves)) * 100, 1), "%")),
      "Blunders" = c(paste0(round((sum(team_stats$blunders_t1) / sum(team_stats$num_moves)) * 100, 1), "%"),
                         paste0(round((sum(team_stats$blunders_t2) / sum(team_stats$num_moves)) * 100, 1), "%")),
    )
    
    # Make stats tableGrob for plot
    tg <- tableGrob(match_stats, 
                    rows=NULL, 
                    theme = ttheme_minimal(base_size = 10.5, 
                                           base_family = gameinfo_font,
                                           base_colour = about_col,
                    padding = unit(c(2, 2), "mm")))
  
  
    # Create plot =================================================================
    
    make_plot <- function(){
  
    plt_story <- ggplot(data = moves_extra) +
  
      # Shade eval regions ----------------------------------------------------
      geom_ribbon(data = moves, 
                  aes(ymin = pmin(y_val, zero_val), 
                      ymax = zero_val, 
                      x = ply_match / 2, 
                      y = y_val), 
                  fill = team2_col, 
                  col = "#ffffff", 
                  alpha = 0.3, 
                  size = 0.1) +
      geom_ribbon(data = moves, 
                  aes(ymin = zero_val, 
                      ymax = pmax(y_val, zero_val), 
                      x = ply_match / 2, 
                      y = y_val), 
                  fill = team1_col, 
                  col = "#ffffff", 
                  alpha = 0.3, 
                  size = 0.1) +
  
      # Equality line ---------------------------------------------------------
      # geom_segment(
      #   data = moves_extra,
      #   aes(
      #     x = first_ply / 2, xend = last_ply / 2,
      #     y = zero_val, yend = zero_val
      #   ),
      #   colour = equalityline_col, # "grey25", "white"
      #   size = 0.3,
      #   alpha = 1,
      #   linetype = "solid"
      # ) +
      
      # Vertical game separators ----------------------------------------------
    geom_segment(
      data = moves_extra,
      aes(
        x = first_ply / 2,
        xend = first_ply / 2,
        y = (((start_margin - 0.3 - min_eval) / y_range) * y_scale_factor) + zero_val,
        yend = (((start_margin + 0.3 - min_eval) / y_range) * y_scale_factor) + zero_val
      ),
      colour = about_col,
      size = 0.1,
      linetype = "dotted"
    ) +
      
      # Game-specific equality lines ------------------------------------------
    geom_segment(
      data = moves_extra,
      aes(
        x = first_ply / 2,
        xend = (first_ply + ((last_ply - first_ply) * 0.4)) / 2,
        y = start_eval,
        yend = start_eval
      ),
      colour = about_col,
      size = 0.1,
      linetype = "dotted"
    ) +
  
      # Eval line -------------------------------------------------------------
      geom_line(data = moves, aes(x = ply_match / 2, y = y_val), 
                col = eval_col, 
                size = 1.2) +
      
      
      
      # # Game start points -----------------------------------------------------
      # annotate("point",
      #          x = moves_extra$first_ply / 2,
      #          y = (((moves_extra$start_margin - min_eval) / y_range) * y_scale_factor) + zero_val,
      #          colour = icon_col,
      #          size = 3) +
  
      # Player names/ratings --------------------------------------------------
      geom_richtext(
        data = moves_extra,
        aes(
          x = ((first_ply + last_ply) / 2) / 2,
          y = max_scaled_eval + 0.58
        ),
        label = paste0(
          "<b>", moves_extra$player_1, "</b>",
          "<span style='font-size:13px'><br>",
          moves_extra$rating_1,
          "</span>"
        ),
        colour = team1_col,
        family = players_font,
        fill = NA, label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt"),
        size = 4.1,
        lineheight = 0.6
      ) +
      geom_richtext(
        data = moves_extra,
        aes(
          x = ((first_ply + last_ply) / 2) / 2,
          y = min_scaled_eval - 0.44
        ),
        label = paste0(
          "<b>", moves_extra$player_2, "</b>",
          "<span style='font-size:13px'><br>",
          moves_extra$rating_2,
          "</span>"
        ),
        colour = ifelse(moves_extra$player_2 != "not played",
          team2_col,
          "grey50"
        ),
        family = players_font,
        fill = NA, label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt"),
        size = 4.1,
        lineheight = 0.6
      ) +
      
      # Colours --------------------------------------------------------------
      geom_point(data = moves_extra,
                 aes(x = ((first_ply + last_ply) / 2) / 2,
                     y = max_scaled_eval + 0.19
                 ),
                 shape = 21,
                 colour = team1_col,
                 fill = ifelse(moves_extra$colour_1 == "B", team1_col, NA),
                 size = 2
      ) +
      geom_point(data = moves_extra,
                 aes(x = ((first_ply + last_ply) / 2) / 2,
                     y = min_scaled_eval - 0.82
                     ),
                 shape = 21,
                 colour = team2_col,
                 fill = ifelse(moves_extra$colour_2 == "B", team2_col, NA),
                 size = 2
                 ) +
  
      # Match score updates ---------------------------------------------------
      geom_label(
        data = moves_extra,
        aes(
          x = (first_ply / 2) + (((last_ply / 2) - (first_ply / 2)) * 0.85),
          y = ifelse(pts_t1 > pts_t2,
            max_scaled_eval + 0.3,
            ifelse(pts_t1 < pts_t2,
              min_scaled_eval - 0.71,
              (min_scaled_eval + max_scaled_eval) / 2
            )
          )
        ),
        label = moves_extra$score,
        family = gameinfo_font,
        size = ifelse(moves_extra$game_order == max(moves_extra$game_order), 3, 3),
        colour = "white",
        fill = ifelse(str_detect(moves_extra$result, "F|X|Z"), forfeit_unplayed_col, 
                      ifelse(moves_extra$pts_t1 > moves_extra$pts_t2,
                          team1_col,
                          ifelse(moves_extra$pts_t1 < moves_extra$pts_t2,
                            team2_col,
                            draw_col))),
        alpha = 1,
        label.padding = unit(0.19, "lines")
      ) +
  
      # Openings info ---------------------------------------------------------
      geom_textbox(
        data = moves_extra,
        aes(
          x = (first_ply + ((last_ply - first_ply) / 2)) / 2,
          y = max_eval + 2.7,
          label = paste0(eco, " ", opening)
        ),
        width = grid::unit(0.085, "npc"), # 8% of plot panel width
        size = 2.5,
        fill = NA, # label.color = NA,
        # label.padding = grid::unit(rep(0, 4), "pt"),
        colour = opening_col, family = gameinfo_font,
        box.colour = NA,
        box.padding = unit(c(2.5, 2.5, 2.5, 2.5), "pt"),
        vjust = 1
      ) +
      
  
      # Title (and supplementary details) -------------------------------------
      # Labels for team names and match score
      ## Team 1 name & score --------------------------------------------------
      geom_richtext(
        data = moves_extra[1, ],
        aes(((max_ply * nrow(moves_extra)) / 2) * 0.49,
          y = max_eval + 5.6,
          label = paste0(
            "<span style='font-size:10pt'>",
            "<span style='color:", rank_col, "'>",
            ifelse(round_num > 1, ifelse(rank_t1 <= 10, paste0("#", rank_t1), ""), ""), "</span>",
            "<span style='color:white'>", "::", "</span>",
            "<span style='color:", team1_col, "'>",
            "<span style='font-size:", 
            ifelse(nchar(teams[1]) < 35, 22, ifelse(nchar(teams[1]) < 37, 20, 18)), 
            "pt'>", ifelse(nchar(teams[1]) < 37, teams[1], stringr::str_trunc(teams[1], width = 40, side = "right")), 
            "</span>",
            "<span style='font-size:22pt'>",
            "<span style='color:white'>", "::::", "</span>",
            finalscore_t1, "</span>",
            "</span>"
          )
        ),
        family = title_font,
        fill = NA, label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt"),
        size = 6,
        hjust = 1,
        vjust = 0.5
      ) +
      ## Team 2 name & score --------------------------------------------------
      geom_richtext(
        data = moves_extra[1, ],
        aes(((max_ply * nrow(moves_extra)) / 2) * 0.51,
          y = max_eval + 5.6,
          label = paste0(
            "<span style='color:", team2_col, "'>",
            "<span style='font-size:22pt'>", finalscore_t2, "</span>", 
            "<span style='color:white'>", "::::", "</span>",
            "<span style='font-size:10pt'>",
            "<span style='color:", rank_col, "'>",
            ifelse(round_num > 1, ifelse(rank_t2 <= 10, paste0("#", rank_t2), ""), ""), "</span>",
            "<span style='color:white'>", "::", "</span>",
            "<span style='font-size:",
            ifelse(nchar(teams[2]) < 35, 22, ifelse(nchar(teams[2]) < 37, 20, 18)), 
            "pt'>",
            ifelse(nchar(teams[2]) < 37, teams[2], stringr::str_trunc(teams[2], width = 40, side = "right")), 
            "</span>",
            "</span>"
          )
        ),
        family = title_font,
        fill = NA, label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt"),
        size = 3,
        hjust = 0,
        vjust = 0.5
      ) +
      ## Season/round header --------------------------------------------------
      geom_richtext(
        data = moves_extra[1, ],
        aes(((max_ply * nrow(moves_extra)) / 2) * 0.49,
            y = max_eval + 6.2,
            label = paste0(
              "Lichess4545 League, Season ", season_num, " Round ", round_num
            )
        ),
        family = title_font,
        colour = titledetail_col,
        fill = NA, label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt"),
        size = 3,
        hjust = 1,
        vjust = 0
      ) +
      
      ## Month/year header ----------------------------------------------------
      geom_richtext(
        data = moves_extra[1, ],
        aes(((max_ply * nrow(moves_extra)) / 2) * 0.51,
            y = max_eval + 6.2,
            label = paste0(
              as.character(lubridate::day(all_games$ended[1])),
              "-",
              as.character(lubridate::day(all_games$ended[nrow(all_games)])),
              " ",
                  as.character(lubridate::month(all_games$ended[nrow(all_games)], label = TRUE)),
                  " ",
                  as.character(lubridate::year(all_games$ended[nrow(all_games)]))
              )
        ),
        family = title_font,
        colour = titledetail_col,
        fill = NA, label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt"),
        size = 3,
        hjust = 0,
        vjust = 0
      ) +
      
      ## Game summary divider text --------------------------------------------
    annotate("text",
             x = ((max_ply * nrow(moves_extra)) / 2) * 0.5,
             y = max_eval + 4.9,
             label = glue::glue("Games"),
             size = 4.5,
             colour = titledetail_col,
             family = title_font,
             hjust = 0.5,
             vjust = 0.5
    ) +
      
      
      ## Match eval chart divider text --------------------------------------------
    annotate("text",
             x = ((max_ply * nrow(moves_extra)) / 2) * 0.5,
             y = max_eval + 1.3,
             label = glue::glue("Story"),
             size = 4.5,
             colour = titledetail_col,
             family = title_font,
             hjust = 0.5,
             vjust = 0
    ) +
      # geom_textbox(aes(
      #   x = ((max_ply * nrow(moves_extra)) / 2) * 0.5,
      #   y = max_eval + 1.1,
      #   label = glue::glue("Combines Lichess evaluations with final results to show how each game contributed to the match's outcome.")),
      #   size = 2,
      #   fill = NA, label.color = NA,
      #   label.padding = grid::unit(rep(0, 4), "pt"),
      #   colour = icon_col,
      #   family = gameinfo_font,
      #   hjust = 0.5,
      #   vjust = 1,
      #   # orientation = "right-rotated",
      #   box.colour = NA,
      #   box.padding = unit(c(0, 0, 0, 0), "pt")
      # ) +

    
      ## Match stats chart divider text --------------------------------------------
    annotate("text",
             x = ((max_ply * nrow(moves_extra)) / 2) * 0.5,
             y = min_eval - 1.5,
             label = glue::glue("Stats"),
             size = 4.5,
             colour = titledetail_col,
             family = title_font,
             hjust = 0.5,
             vjust = 0
    ) +
      

      
      # Match summary row =====================================================
      ## Board numbers ----------------------------------------------------------
      geom_richtext(
        data = moves_extra,
        aes(
          x = (first_ply / 2) + (((last_ply / 2) - (first_ply / 2)) * 0.05),
          y = max_eval + 4.2,
          label = glue::glue("B{board}")
        ),
        family = plotinfo_font,
        colour = gamedetail_col,
        fill = NA, label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt"),
        size = 2.9,
        hjust = 0,
        vjust = 0
      ) +
      ## Game start times -----------------------------------------------------
      geom_richtext(
        data = moves_extra,
        aes(
          x = (first_ply / 2) + (((last_ply / 2) - (first_ply / 2)) * 0.95),
          y = max_eval + 4.2,
          label = glue::glue("{ifelse(!(str_detect(game_id, 'forfeit')), 
                               paste0(as.character(lubridate::wday(lubridate::round_date(started, '15 mins'), label = T, week_start = 1)), ' ',
                               as.character(sprintf('%02d', lubridate::hour(lubridate::round_date(started, '15 mins')))),
                               ':',
                               as.character(sprintf('%02d', lubridate::minute(lubridate::round_date(started, '15 mins'))))),
                               '')}")
        ),
        family = plotinfo_font,
        colour = gamedetail_col,
        fill = NA, label.color = NA,
        label.padding = grid::unit(rep(0, 4), "pt"),
        size = 2.7,
        hjust = 1,
        vjust = 0
      ) +
      
    ## Player names/points ----------------------------------------------------
    # Player 1
    geom_richtext(
      data = moves_extra,
      aes(
        x = (first_ply / 2) + (((last_ply / 2) - (first_ply / 2)) * 0.05),
        y = max_eval + 3.8,
        label = glue::glue("{ifelse(nchar(player_1) < 17, player_1, str_trunc(player_1, 17, 'right'))}")
      ),
      family = plotinfo_font,
      colour = team1_col,
      fill = NA, label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"),
      size = 3.2,
      hjust = 0,
      vjust = 0
    ) +
    # Player 1's points
    geom_richtext(
      data = moves_extra,
      aes(
        x = (first_ply / 2) + (((last_ply / 2) - (first_ply / 2)) * 0.95),
        y = max_eval + 3.8,
        label = glue::glue("{pts_t1_print}")
      ),
      family = plotinfo_font,
      colour = team1_col,
      fill = NA, label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"),
      size = 3.2,
      hjust = 1,
      vjust = 0
    ) +
    # Player 2
    geom_richtext(
      data = moves_extra,
      aes(
        x = (first_ply / 2) + (((last_ply / 2) - (first_ply / 2)) * 0.05),
        y = max_eval + 3.5,
        label = glue::glue("{ifelse(nchar(player_2) < 17, player_2, str_trunc(player_2, 17, 'right'))}")
      ),
      family = plotinfo_font,
      colour = team2_col,
      fill = NA, label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"),
      size = 3.2,
      hjust = 0,
      vjust = 0
    ) +
    # Player 2's points
    geom_richtext(
      data = moves_extra,
      aes(
        x = (first_ply / 2) + (((last_ply / 2) - (first_ply / 2)) * 0.95),
        y = max_eval + 3.5,
        label = glue::glue("{pts_t2_print}")
      ),
      family = plotinfo_font,
      colour = team2_col,
      fill = NA, label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"),
      size = 3.2,
      hjust = 1,
      vjust = 0
    ) +
    
    ## Game IDs ---------------------------------------------------------------
    geom_richtext(
      data = moves_extra,
      aes(
        x = (first_ply / 2) + (((last_ply / 2) - (first_ply / 2)) * 0.5),
        y = max_eval + 3,
        label = glue::glue("{ifelse(str_detect(game_id, 'forfeit'), '', game_id)}")
      ),
      # width = grid::unit(0.08, "npc"), # 10% of plot panel width
      size = 3.2,
      hjust = 0.5,
      vjust = 0,
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"),
      colour = ifelse(str_detect(moves_extra$game_id, "forfeit"), forfeit_unplayed_col, gamelink_summary_col), 
      family = fixedwidth_font
    ) +
    # # Underline gamelinks manually
    # annotate("segment",
    #          x = (moves_extra$first_ply / 2) + (((moves_extra$last_ply / 2) - (moves_extra$first_ply / 2)) * 0.26),
    #          xend = (moves_extra$first_ply / 2) + (((moves_extra$last_ply / 2) - (moves_extra$first_ply / 2)) * 0.75),
    #          y = max_eval + 2.963,
    #          yend = max_eval + 2.963,
    #          colour = ifelse(str_detect(moves_extra$game_id, "forfeit"), NA, gamelink_summary_col), 
    #          size = 0.5
    # ) +
      
      ## Match stats table ----------------------------------------------------
       annotation_custom(tg,
                        xmin = ((max_ply * nrow(moves_extra)) / 2) * 0.1,
                        xmax = ((max_ply * nrow(moves_extra)) / 2) * 0.9,
                        ymin = min_eval - 3,
                        ymax = min_eval - 1.6) +
    
      ## Tick labels ----------------------------------------------------------
      annotate("text",
               x = ((max_ply * nrow(moves_extra)) / 2) * -0.012,
               y = (((unique(moves_extra$end_margin) - min_eval) / y_range) * y_scale_factor) + zero_val,
               label = paste0("+", abs(unique(moves_extra$end_margin))),
               colour = ifelse(unique(moves_extra$end_margin) > 0, team1_col, 
                               ifelse(unique(moves_extra$end_margin) < 0, team2_col, icon_col)), 
               family = plotinfo_font,
               size = 3
      ) +
      ## Team 1 "leading" arrow ------------------------------------------------
      geom_segment(aes(
        x = ((max_ply * nrow(moves_extra)) / 2) * -0.015,
        xend = ((max_ply * nrow(moves_extra)) / 2) * -0.015,
        y = (((0.2 - min_eval) / y_range) * y_scale_factor) + zero_val,
        yend = (((0.6 - min_eval) / y_range) * y_scale_factor) + zero_val),
        arrow = arrow(length = unit(0.01, "npc"),
                      type = "closed"),
        colour = team1_col,
        size = 0.1
        ) +
      ## Team 2 "leading" arrow -----------------------------------------------
      geom_segment(aes(
        x = ((max_ply * nrow(moves_extra)) / 2) * -0.015,
        xend = ((max_ply * nrow(moves_extra)) / 2) * -0.015,
        y = (((-0.2 - min_eval) / y_range) * y_scale_factor) + zero_val,
        yend = (((-0.6 - min_eval) / y_range) * y_scale_factor) + zero_val),
        arrow = arrow(length = unit(0.01, "npc"),
                      type = "closed"),
        colour = team2_col,
        size = 0.1
      ) +
      ## Y-axis label ----------------------------------------------------------
      geom_textbox(aes(
        x = ((max_ply * nrow(moves_extra)) / 2) * -0.025,
        y = (((0 - min_eval) / y_range) * y_scale_factor) + zero_val),
        label = paste0("Current match margin + in-game evaluation"),
                   size = 2.5,
                   fill = NA, # label.color = NA,
                   # label.padding = grid::unit(rep(0, 4), "pt"),
                   colour = icon_col,
                   family = gameinfo_font,
                   hjust = 0.5,
                   vjust = 0,
                   orientation = "left-rotated",
                   box.colour = NA,
                   box.padding = unit(c(0, 0, 0, 0), "pt")
                   ) +
      ## "About" text --------------------------------------------------------- 
      geom_textbox(
        data = moves_extra[1, ],
        aes(
          x = ((max_ply * nrow(moves_extra)) / 2) * -0.025,
          y = min_eval - 3.9,
          label = glue::glue("NOTES 'Games': all played and non-forfeited games have hyperlinked 8-character game IDs; the days/times show when each game started, shown in UTC and rounded to the nearest 15 minutes. 
                             'Story': the y-axis tracks the difference between each team's overall match score, scaling each game's move-by-move evaluation accordingly, while the x-axis shows moves played in the match; both axes are scaled for consistency and legibility; games in the match are ordered from left to right by the time of their last move (unplayed games are then assigned a place by their original scheduled time, and pairings without a scheduled time are shown last). 
                             'Stats' - (F)W/D/L: (forfeit) wins/losses/draws, Clock: total clock time used, ACPL: team average centipawn loss (adjusted for moves), Inaccuracies/Mistakes/Blunders: % moves of each error type. Also: ranks only shown for the top 10 teams at the start of the round, ignoring tiebreaks; ranks not shown for Round 1.
                             This plot was compiled on {paste0(lubridate::day(lubridate::today()), ' ', lubridate::month(lubridate::today(), label = T, abbr = F), ' ', lubridate::year(lubridate::today()))}.")
        ),
        width = grid::unit(0.92, "npc"), # % of plot panel width
        size = 2.25,
        fill = NA,
        # label.padding = grid::unit(rep(0, 4), "pt"),
        colour = about_col, 
        family = plotinfo_font,
        box.colour = NA,
        box.padding = unit(c(0.1, 0.1, 0.1, 0.1), "pt"),
        vjust = 1,
        hjust = 0
      ) +
      
      # Combines Lichess evaluations with final results to show how each game contributed to the match's outcome.
      theme_cowplot() +
      
      # Set plot limits - IMPORTANT
      ylim(c(min(moves$y_val - 4), max(moves$y_val + 6.2))) +
      xlim(c(((max_ply * nrow(moves_extra)) / 2) * -0.03,
             ((max_ply * nrow(moves_extra)) / 2) * 1)) +
  
      # Remove all gridlines
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        text = element_text(family = plotinfo_font, colour = "grey25")# ,
        # plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
      )
  
    # Show plot
    # plt_story
    return(plt_story)
    }
    
    # Make plot
    plt_story <- make_plot()
    
    # Turn game IDs into hyperlinks -------------------------------------------
    gamelinks = c(paste0("https://lichess.org/", unique(moves_extra$game_id)))
    ggsave(tf1 <- tempfile(fileext = ".svg"), plt_story,
           width = plot_width, height = plot_height, units = "mm", dpi = "retina")
    links <- with(moves_extra, setNames(gamelinks, unique(game_id)))
    xml <- read_xml(tf1)
    xml %>%
      xml_find_all(xpath="//d1:text") %>%
      keep(xml_text(.) %in% names(links)) %>%
      xml_add_parent("a", "xlink:href" = links[xml_text(.)], target = "_blank")
    write_xml(xml, tf2 <- tempfile(fileext = ".svg"))
    
    # Define filename for individual match plot
    filename = paste0("s", sprintf("%02d", season_num), 
                      "_r", sprintf("%0d", round_num), 
                      "_m", sprintf("%02d", m), 
                      "_story.pdf")
    
    # Save as PDF
    rsvg::rsvg_pdf(svg = tf2, 
                   file = paste0(here::here(), save_path, filename), 
                   width = plot_width,
                   height = plot_height)
  } # end for loop
  
  # print("Made all match stories")
  # cli::cli_alert_success("Produced all match plots")
  
  # Save combined PDF =========================================================
  
  # Function: combines PDFs with individual match stories into a single PDF with
  #   each match shown on a new page
  combine_plots <- function(){
  
    # Find paths of individual PDFs  
    pdfs_path <- paste0(here::here(), save_path)
    dir <- fs::dir_info(pdfs_path) %>% 
      filter(type == "file") %>% 
      filter(str_detect(path, paste0("s", sprintf("%02d", season_num), "_r", round_num)))
    pdfs <- as.character(dir$path)
    
    # Construct filename for combined PDF
    if(plot_whole_round){
      # Save as "sXX_rYY_allmatches.pdf"
      combined_filename <- paste0(here::here(), save_path, "s", sprintf("%02d", season_num), 
                                  "_r", round_num, "_allmatches.pdf")
    } else {
      # Save as "sXX_rYY_match.pdf"
      combined_filename <- paste0(here::here(), save_path, "s", sprintf("%02d", season_num), 
                                  "_r", round_num, "_singlematch.pdf")
    }
    
    # Combine match PDFs and save
    qpdf::pdf_combine(input = pdfs,
                      output = combined_filename)
    return()
  }
  
  # Call combine_plots()
  combine_plots()
  
  # Now embed fonts into the PDF - ensures it looks the same to all viewers 
  # regardless of which fonts happen to be locally installed.
  
  Sys.setenv(R_GSCMD = gspath) # required for extrafont's embedding functionality to work
  
  # Define final PDF filename
  if(plot_whole_round){
    # When plotting all matches in a round, save as "sXX_rYY_allmatches.pdf"
    # Save outside 'prod' folder - only do this for whole round plots
    combined_filename <- paste0(here::here(), save_path, "s", sprintf("%02d", season_num),
                                "_r", round_num, "_allmatches.pdf")
  } else {
    # When plotting a single match, save as "sXX_rYY_match.pdf"
    combined_filename <- paste0(here::here(), save_path, "s", sprintf("%02d", season_num),
                                "_r", round_num, "_singlematch.pdf")
  }
  

  files_to_delete <- fs::dir_info(glue::glue(here::here(), save_path)) %>% 
    filter(type == "file") %>% 
    filter(str_detect(path, paste0("s", sprintf("%02d", season_num), "_r", 
                                   round_num, "_m\\d\\d_story.pdf"))) %>% 
    select(path) %>% 
    dplyr::pull()

  # Replace combined PDF with an embedded PDF
  extrafont::embed_fonts(combined_filename,
                         outfile = combined_filename)
  
  # Then delete the individual match PDFs
  fs::file_delete(files_to_delete)
  
  # Move final PDF from output folder to publication folder
  # Overwrite any previously saved files with the same name
  
  # Filepath for final PDF (saved in /reports/stories/prod/)
  final_prod_filepath <- fs::dir_info(glue::glue(here::here(), save_path)) %>% 
    filter(type == "file") %>% 
    filter(str_detect(path, paste0("s", sprintf("%02d", season_num), "_r", 
                                   round_num))) %>% 
    select(path) %>% 
    dplyr::pull()
  
  # Filename of final PDF (saved in /reports/stories/prod/)
  final_prod_filename <- final_prod_filepath %>% 
    stringr::str_extract(paste0("s", sprintf("%02d", season_num), "_r", 
                                round_num, ".+$"))
  
  # Save a copy of the final PDF in /reports/stories/
  # Overwrites any previously saved file with the same name
  # Then delete the PDF from reports/stories/prod
  fs::file_copy(path = final_prod_filepath,
                new_path = paste0(here::here(), 
                                  paste0("/docs/reports/stories/"),
                                  final_prod_filename),
                overwrite = TRUE)
  fs::file_delete(path = final_prod_filepath) # delete PDF in prod
  cli::cli_alert_success("Saved match story PDF")
  
  # fs::file_show(combined_filename) # show final PDF
  
} # end function


# ---- Plot all match stories for a round ----
# PlotMatchStory(28, 2, plot_whole_round = T, request_data = T)

# ---- Plot all match stories for a season ----

# season <- 28
# for(r in c(1:8)){
#   PlotMatchStory(season, r, plot_whole_round = T, request_data = T)
# }
