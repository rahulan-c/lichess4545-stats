
# =============================================================================
#                    lichess4545-stats publication functions
# =============================================================================

# Functions that combine other functions to allow for one-step publication of
# outputs such as new season stats/awards reports, new 4545 round stories, new
# round summary reports (when they're developed), new all-time stats pages...


# ---- FUNCTIONS THAT NEED ALL RELEVANT GAMES TO HAVE BEEN ANALYSED FIRST -----

# PublishSeasonStats()
# BuildSite()

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, cli, fs, glue, here, distill)


# Create new season stats/awards report(s), push them to the site, wait a bit, 
# re-make the all-time awards search page, then publish that
PublishSeasonStats <- function(need_data = FALSE,
                               update_awards = FALSE,
                               team_seasons = NULL, 
                               lwopen_seasons = NULL, 
                               lwu1800_seasons = NULL,
                               chess960_seasons = NULL) {
  
  # Define how many minutes to wait after publishing season reports before 
  # updating the searchable awards page
  post_publication_wait <- 10
  
  cli::cli_rule(left = "Compiling and publishing new {.field season report(s)}")
  
  sb <- cli::cli_status("{symbol$arrow_right} Preparing inputs...")
  
  # Load all required functions
  source(paste0(here::here(), "/R/all_functions.R"))
  source(paste0(here::here(), "/R/report_functions.R"))
  
  # Produce all selected 4545/LW/960 season reports
  BuildSeasonReports(wipe_stats_first = FALSE,
                     request_data = need_data,
                     team_range = team_seasons,
                     lwopen_range = lwopen_seasons,
                     lwu1800_range = lwu1800_seasons,
                     chess960_range = chess960_seasons)
  
  
  # Copy each non-RMD file in site/reports/ to docs/reports/ before deleting it
  # Make sure you don't delete the site/reports folder though
  files_to_move <- fs::dir_info(glue::glue(here::here(), "/site/reports")) %>% 
    filter(type == "file") %>% 
    filter(str_detect(path, "html|png")) %>% 
    filter(!(str_detect(path, "head-custom-google-analytics"))) %>% 
    select(path) %>% 
    dplyr::pull()
  
  for(file in files_to_move){
    fs::file_copy(path = file, new_path = "docs/reports/", overwrite = T)
    fs::file_delete(file)
  }
  
  
  cli_status_update(id = sb,
                    "{symbol$arrow_right} New season report(s) produced...")
  Sys.sleep(5)
  
  # Re-knit and publish the all season reports page
  BuildSite(update_core = F,
            update_countries = F,
            update_allreports = T,
            update_awards = F)
  
  cli_status_update(id = sb,
                    "{symbol$arrow_right} Website updated...")
  
  # If necessary, wait and then update the all-time awards search page
  if(update_awards){
    cli_status_update(id = sb,
                      "{symbol$arrow_right} Waiting for {post_publication_wait} minutes before updating awards search page.")
    
    Sys.sleep(post_publication_wait * 60) # wait before updating the all-awards page
    
    cli_status_update(id = sb,
                      "{symbol$arrow_right} Updating awards search page...")
    BuildSite(update_awards = T)
  }
  
  cli_status_update(id = sb,
                    "{symbol$arrow_right} Awards search page updated...")
  cli_status_clear(id = sb)
  cli_alert_success("Process completed")
  
}


# Build site function
BuildSite <- function(quiet = FALSE,
                      update_core = FALSE,
                      update_countries = FALSE,
                      update_allreports = FALSE,
                      update_awards = FALSE,
                      update_standings = FALSE){
  
  # Render core pages
  if(update_core){
    rmarkdown::render_site(input = "site", quiet = quiet)
    # Copy site/docs to docs/
    fs::dir_copy(path = "site/docs/", new_path = "docs/", overwrite = TRUE)
    # Delete site/docs
    fs::dir_delete(path = "site/docs/")
    
  }
  
  
  # Update article on league players by continent and country
  if(update_countries){
    rmarkdown::render("site/_countries.Rmd", 
                      output_file = "countries.html",
                      output_dir = "docs")
  }
  
  # Update list of all season stats reports by league and season
  if(update_allreports){
  rmarkdown::render("site/_list-all-season-reports.Rmd",
                    output_file = "season_stats.html",
                    output_dir = "docs")
  }
  
  # Update searchable all-time award winners page
  if(update_awards){
  rmarkdown::render("site/_awards_search.Rmd",
                    output_file = "awards_search.html",
                    output_dir = "docs")
  }
  
  # Update live standings page
  # Note: update_core also updates live standings
  if(update_standings){
    rmarkdown::render("site/live.Rmd",
                      output_file = "live.html",
                      output_dir = "docs")
  }
  
  # Update remote repo with all changes
  UpdateRepo()
}

