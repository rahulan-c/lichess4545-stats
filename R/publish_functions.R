
# =============================================================================
#                    lichess4545-stats publication functions
# =============================================================================

# Functions for publishing and updating the stats site.

# Current list:
# - BuildSite()
# - PublishSeasonStats()


# ---- Load packages ----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, cli, fs, glue, here, distill)




# ---- FUNCTIONS --------------------------------------------------------------


#' Build lichess4545-stats site
#' 
#' Rebuild stats site by rendering one or more of the frontpage, the all-seasons 
#' reports page, the all-seasons awards page, the live standings page, the 
#' "about" page, and the various standalone article pages.
#' @param quiet Do you want the console to show intermediate output as each RMD file is compiled?  
#' @param update_frontpage If TRUE, rebuilds the site frontpage
#' @param update_allreports Update the page listing all 4545/LW/C960 season reports?
#' @param update_awards Update the all-seasons award details page?
#' @param update_standings Update the 'live' standings page?
#' @param update_about Update the 'about' paeg?
#' @param update_articles If TRUE, re-knits all standalone articles
#' @param update_roundstories If TRUE, re-knits the 'round stories' page
#' @param update_logogallery If TRUE, re-knits the team logo gallery page
#'
#' @return
#' @export
#'
#' @examples
BuildSite <- function(quiet = FALSE,
                      update_frontpage = FALSE,
                      update_allreports = FALSE,
                      update_awards = FALSE,
                      update_standings = FALSE,
                      update_about = FALSE,
                      update_articles = FALSE,
                      update_roundstories = FALSE,
                      update_logogallery = FALSE){
  
  # Render frontpage
  if(update_frontpage){
    rmarkdown::render_site(input = "site", quiet = quiet)
    # Copy site/docs to docs/
    fs::dir_copy(path = "site/docs/", new_path = "docs/", overwrite = TRUE)
    # Delete site/docs
    fs::dir_delete(path = "site/docs/")
    
  }
  
  # Update list of all season stats reports by league and season
  if(update_allreports){
  rmarkdown::render("site/_list-all-season-reports.Rmd",
                    output_file = "season_stats.html",
                    output_dir = "docs",
                    quiet = quiet)
  }
  
  # Update searchable all-time award winners page
  if(update_awards){
  rmarkdown::render("site/_awards_search.Rmd",
                    output_file = "awards_search.html",
                    output_dir = "docs",
                    quiet = quiet)
  }
  
  # Update live standings page
  if(update_standings){
    rmarkdown::render("site/_live.Rmd",
                      output_file = "live.html",
                      output_dir = "docs",
                      quiet = quiet)
  }
  
  # Update "about" page
  if(update_about){
    rmarkdown::render("site/_about.Rmd",
                      output_file = "about.html",
                      output_dir = "docs",
                      quiet = quiet)
  }
  
  # Update articles
  if(update_articles){
    # Players by continent/country
    rmarkdown::render("site/_countries.Rmd", 
                      output_file = "countries.html",
                      output_dir = "docs",
                      quiet = quiet)
  }
  
  # Update round updates page
  if(update_roundstories){
    # Players by continent/country
    rmarkdown::render("site/_round_updates.Rmd", 
                      output_file = "round_updates.html",
                      output_dir = "docs",
                      quiet = quiet)
  }
  
  # Update team logo gallery page
  if(update_logogallery){
    rmarkdown::render("site/team_logos/_team_logos.rmd", 
                      output_file = "team_logos.html",
                      output_dir = "docs",
                      quiet = quiet)
    
    # Copy full-size team logos to /docs
    fullsize_files <- fs::dir_info(glue::glue(here::here(), "/site/team_logos/fullsize/")) %>% 
      filter(type == "file") %>% 
      select(path) %>% 
      dplyr::pull()
    for(file in fullsize_files){
      fs::file_copy(path = file, new_path = "docs/fullsize/", overwrite = T)
    }
    
    thumb_files <- fs::dir_info(glue::glue(here::here(), "/site/team_logos/thumbs/")) %>% 
      filter(type == "file") %>% 
      select(path) %>% 
      dplyr::pull()
    for(file in thumb_files){
      fs::file_copy(path = file, new_path = "docs/thumbs/", overwrite = T)
    }
    
    
  }
  
  
  # Update remote repo with all changes
  UpdateRepo()
}



#' Publish one or more 4545/LW/960 season stats reports
#' 
#' Produces season summary stats reports for specified seasons of the 4545, 
#' LoneWolf (Open), LoneWolf (U1800) and Chess960 leagues. If specified, the relevant
#' season games and pairings data is first obtained from the Lichess4545 website and 
#' Lichess API and then saved locally. After reports have been produced for all
#' requested seasons, they are pushed to the site. If the user then wants to
#' update the all-seasons awards page, that page is then re-compiled after a 
#' 10-minute wait (or similar, to ensure that the re-compilation captures any
#' newly published awards).    
#'
#' @param need_data boolean: whether you need to request games and season 
#' data from the Lichess API (default: FALSE)
#' @param update_awards boolean: whether the all-seasons awards page should be 
#' re-knit and re-published afterwards (default: FALSE) 
#' @param team_seasons vector: range of 4545 seasons to report (default: NULL)
#' @param lwopen_seasons vector: range of LoneWolf Open seasons to report (default: NULL)
#' @param lwu1800_seasons vector: range of LoneWolf U1800 seasons to report (default: NULL)
#' @param chess960_seasons vector: range of Chess960 league seasons to report (default: NULL)
#'
#' @return
#' @export
#'
#' @examples
PublishSeasonStats <- function(need_data = FALSE,
                               update_awards = FALSE,
                               team_seasons = NULL, 
                               lwopen_seasons = NULL, 
                               lwu1800_seasons = NULL,
                               chess960_seasons = NULL) {
  
  
  
  # Create new season stats/awards report(s), push them to the site, wait a bit, 
  # re-make the all-time awards search page, then publish that
  
  # Define how many minutes to wait after publishing season reports before 
  # updating the searchable awards page
  post_publication_wait <- 10
  
  cli::cli_rule(left = "Compiling and publishing new season report(s)")
  
  sb <- cli::cli_status("{symbol$arrow_right} Compiling reports...")
  
  # Load all required functions
  source(paste0(here::here(), "/R/all_functions.R"))
  source(paste0(here::here(), "/R/report_functions.R"))
  
  # Produce all selected 4545/LW/960 season reports
  # Calls BuildSeasonReports() from all_functions.R
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
                    "{symbol$arrow_right} New report(s) produced...")
  Sys.sleep(5)
  
  # Re-knit and publish the all season reports page
  BuildSite(update_allreports = T)
  
  cli_status_update(id = sb,
                    "{symbol$arrow_right} New reports published...")
  
  # If necessary, wait and then update the all-time awards search page
  if(update_awards){
    cli_status_update(id = sb,
                      "{symbol$arrow_right} Waiting for {post_publication_wait} mins before updating awards page...")
    
    Sys.sleep(post_publication_wait * 60) # wait before updating the all-awards page
    
    cli_status_update(id = sb,
                      "{symbol$arrow_right} Updating awards page...")
    BuildSite(update_awards = T)
    cli_status_update(id = sb,
                      "{symbol$arrow_right} New awards page published...")
    cli_status_clear(id = sb)
    cli_alert_success("DONE")
    
  } else {
    
    cli_status_clear(id = sb)
    cli_alert_success("DONE")
    
  }
}


PublishRoundStats <- function(league_choice = NULL,
                  season_choice = NULL,
                  round_choice = NULL){
  
  league <- league_choice
  season <- season_choice
  round <- round_choice
  lw_u1800 <- ifelse(league_choice == "lwu1800", TRUE, FALSE)
  
  
  rmarkdown::render(paste0(here::here(), "/site/_round_stats.rmd"),
                    output_file = paste0("round_stats_", league, "_s", sprintf("%02d", season), "r",
                                         sprintf("%02d", round),
                                         ".html"),
                    output_dir = "docs",
                    params = list(
                      league = league, 
                      season = season,
                      round = round,
                      lw_section = lw_u1800
                    ))
  
}
