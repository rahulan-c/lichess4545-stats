
# =============================================================================
#                    lichess4545-stats publication functions
# =============================================================================

# Functions that combine other functions to allow for one-step publication of
# outputs such as new season stats/awards reports, new 4545 round stories, new
# round summary reports (when they're developed), new all-time stats pages...


# ---- FUNCTIONS THAT NEED ALL RELEVANT GAMES TO HAVE BEEN ANALYSED FIRST -----

# PublishSeasonStats()
# PublishRoundStory() - TODO
# PublishRoundReport() - TODO



# Create new season stats/awards report(s), push them to the site, wait a bit, 
# re-make the all-time awards search page, then publish that
PublishSeasonStats <- function(request = FALSE, 
                               team_seasons = NULL, 
                               lwopen_seasons = NULL, 
                               lwu1800_seasons = NULL,
                               chess960_seasons = NULL) {
  
  post_publication_wait <- 240
  
  cli::cli_rule(left = "Compiling and publishing new {.field season report(s)}")
  
  sb <- cli::cli_status("{symbol$arrow_right} Preparing inputs...")
  
  # Load all required functions
  source(paste0(here::here(), "/scripts/all_functions.R"))
  source(paste0(here::here(), "/scripts/report_functions.R"))
  
  # Make season reports
  BuildSeasonReports(wipe_stats_first = FALSE,
                     request_data = request,
                     team_range = team_seasons,
                     lwopen_range = lwopen_seasons,
                     lwu1800_range = lwu1800_seasons,
                     chess960_range = chess960_seasons)
  
  cli_status_update(id = sb,
                    "{symbol$arrow_right} New season report(s) produced...")
  Sys.sleep(10)
  UpdateSite(new_stats_produced = TRUE)
  cli_status_update(id = sb,
                    "{symbol$arrow_right} Website updated...")
  cli_status_update(id = sb,
                    "{symbol$arrow_right} Waiting for {post_publication_wait / 60} minutes...")
  Sys.sleep(post_publication_wait / 60) # wait a bit
  # Redo and publish the all-time awards search page
  cli_status_update(id = sb,
                    "{symbol$arrow_right} Updating awards search page...")
  rmarkdown::render(paste0(path_root, "/reports/awards_search.rmd"))
  Sys.sleep(5)
  UpdateRepo()
  cli_status_update(id = sb,
                    "{symbol$arrow_right} Awards search page updated...")
  cli_status_clear(id = sb)
  cli_alert_success("Process completed")
  
}

