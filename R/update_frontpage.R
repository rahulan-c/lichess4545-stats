# Update the frontpage every week

# Source functions
source(paste0(here::here(), "/R/all_functions.R"))
source(paste0(here::here(), "/R/report_functions.R"))
source(paste0(here::here(), "/R/publish_functions.R"))

# Build site to just update 'core' pages, incl. frontpage
BuildSite(update_core = T,
          update_countries = F,
          update_allreports = F,
          update_awards = F,
          update_standings = F)

# Automate site builds
# Needs to be run in console
# taskscheduleR::taskscheduler_create(taskname = "update_stats_homepage",
#                                     rscript = glue::glue("{here::here()}/R/update_frontpage.R"),
#                                     schedule = "WEEKLY",
#                                     starttime = "09:00",
#                                     days = c("TUE"))