
###############################################################################
#    ESSENTIAL CODE SNIPPETS FOR LICHESS4545 STATS STUFF 
###############################################################################


# ---- Load all_functions.R and report_functions.R ----------------------------
source(paste0(here::here(), "/scripts/all_functions.R"))
source(paste0(here::here(), "/scripts/report_functions.R"))


# ---- Fetch games played in a single round -----------------------------------
# To check whether any require server analysis
LeagueGames("team4545", 29, 5)
LeagueGames("lonewolf", 24)


# ---- Update site ----
UpdateSite(navbar_changed = F,
           update_frontpage = T,
           new_stats_produced = F,
           update_current = T,
           update_standings = T,
           update_status = T,
           update_about = F)


# ---- Push latest repo changes to site ---------------------------------------
source(paste0(here::here(), "/scripts/all_functions.R"))
UpdateRepo()


# ---- Produce season stats WITHOUT publishing anything -----------------------
.rs.restartR()
source(paste0(here::here(), "/scripts/all_functions.R"))
source(paste0(here::here(), "/scripts/report_functions.R"))
MakeSeasonReport(league = "team4545", 
                   season = 28,
                   from_scratch = F)
# league args: "team4545", "lwopen", "lwu1800"


# ---- Save season data for one or more seasons -------------------------------
# Suggest doing this first before producing seasons stats for all seasons
# Might avoid persistent R crashes this way
source(paste0(here::here(), "/scripts/all_functions.R"))
SaveSeasonData("team4545", c(28))


# ---- Compile season reports for one or more 4545/LW/C960 seasons ------------
.rs.restartR()
source(paste0(here::here(), "/scripts/all_functions.R"))
source(paste0(here::here(), "/scripts/report_functions.R"))
BuildSeasonReports(wipe_stats_first = FALSE,
                     request_data = TRUE,
                     team_range = NULL,
                     lwopen_range = NULL,
                     lwu1800_range = c(24),
                     chess960_range = NULL)


# ---- Update the awards search page ------------------------------------------
source(paste0(here::here(), "/scripts/all_functions.R"))
source(paste0(here::here(), "/scripts/report_functions.R"))
rmarkdown::render(paste0(here::here(), "/reports/awards_search.rmd"))
UpdateRepo()


# ---- Update all-time league games datasets ----------------------------------
library(here)
source(paste0(here::here(), "/scripts/all_functions.R"))
# TBC


# ---- Produce "match stories" PDF for a completed 4545 round -----------------
# Check that all games have analysis by running get_league_games() first
source(paste0(here::here(), "/scripts/plot_match_story.R"))
PlotMatchStory(29, 5, plot_whole_round = T, request_data = T)

# Plot one match
source(paste0(here::here(), "/scripts/plot_match_story.R"))
PlotMatchStory(29, 5, plot_whole_round = F, request_data = T, 
               method = "team", details = "Adult Decliners")


# ---- Update the 4545 live standings page ------------------------------------
source(paste0(here::here(), "/scripts/all_functions.R"))
source(paste0(here::here(), "/scripts/report_functions.R"))
UpdateSite(update_standings = T)


# ==== LOAD SAVED DATA FOR A SPECIFIC LEAGUE/SEASON ===========================

# Pick a league and season 
# 1: 4545, 2: LW Open, 3: LW U1800
league_choice <- "team4545"
season_choice <- 28

# Read season data
games <- readRDS(paste0(here::here(), "/data/", "games_", league_choice, "_s", season_choice, ".rds"))
website_pairings <- readRDS(paste0(here::here(), "/data/", "website_pairings_", league_choice, "_s", season_choice, ".rds"))
pairings <- read.csv(paste0(here::here(), "/data/", "pairings_", league_choice, "_s", season_choice, ".csv"))
positions <- read.csv(paste0(here::here(), "/data/", "positions_", league_choice, "_s", season_choice, ".csv"))



# ---- WEBSITE TODOS ----------------------------------------------------------
# Complete 'about' page
# Add a proper league status table to the frontpage
# Auto-schedule updates to the frontpage and live standings
# 

# ---- STATS TODOS ------------------------------------------------------------
# Add new suggested awards/stats to 4545/LW stats
# All-time stats and records (finally)
# Rapid Battle stats
# Series stats
# Cross-league trivia
# Endgame stats
# Tablebase position stats
# Openings-related stats


# ---- DEV TODOS --------------------------------------------------------------

# Make the ToS violators process non-interactive if poss. 
# STATUS: UNCLEAR - DON'T KNOW WHAT THIS MEANS ANY MORE :)

# Make function that goes over all saved season data and compiles suitable
# collated datasets of games, pairings, and website pairings
# STATUS: IN PROGRESS (started, made some progress, but not finished yet, 
# I think)

# Make a complete command-line interface for all regular stats-related tasks:
# - to check if a round's games have been analysed
# - to produce a set of "match story" plots for a completed round
# - to update 4545 and/or LW live standings
# - to produce a season stats report
# - to update all-time stats for one or more leagues
# - to publish all recent changes to the site
# Probably best to do this as an RStudio Addin
# https://rstudio.github.io/rstudioaddins/
# STATUS: NOT STARTED

# 


# ----------

# Website notes

# To change the top menu etc, you need to change _site.yml
# BUT then you need to re-produce every page on the site. Otherwise, the navbar
# changes won't show when each page is loaded.

