# Extract up to date list of ToS violators from the 4545 ban log
# NOT TO BE UPLOADED TO PUBLIC REPO

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, googlesheets4)

options(gargle_oauth_email = TRUE) # to allow non-interactive use

identify_tos_violators <- function(){

banlog_id <- "1AlQIYEosp9CKEbR__1jQXPmYzVNOdZg_0MsBWOlhTyQ"

banned <- read_sheet(banlog_id)



# Identify explanatory reasons in log that would merit exclusion from 
# these stats/awards
boot_reasons <- c("Marked as using computer assistance on lichess",
                  "Cheating alt and marked as engine on lichess",
                  "Admitted to using assistance during a 4545 game",
                  "Cheating alt / Multi accounting in the league / LW",
                  "Violated terms of service on lichess",
                  "Violated Lichess terms of service")

# If you want to include players w/ cheat detected league games...
boot_reasons <- c(boot_reasons, "Cheat detected league game", "cheat detected league game")

# Identify accounts banned for these reasons
tos_violators <- banned %>% 
  filter(Reason %in% boot_reasons) %>% 
  # Don't include accounts where the mark was reversed on appeal
  # or they were reinstated for another reason
  # or they were booted for non-ToS reasons (like oddskill)
  filter(!(name %in% c("like-a-hurricane", "zopherus", "MrScribbles",
                       "PartyMagier", "amirkhaled21", "johnjaison2008",
                       "HowHorseyMoves", "makeareason", "oddskill"))) %>% 
  dplyr::pull(name)

# Add certain names by hand to the list
tos_violators <- c(
  tos_violators, 
  "tushar20008", # kungfupanda20008's 1st account
  "linail",      # v. likely to have cheated in their prev. 33 league games (see CR), but aren't marked 
  "Jaivl",       # reported multiple times for suspected 4545 cheating, I think they were...
  "SMC",         # eg https://lichess.org/kRmGga91 vs marked account
  "Chaskar"      # lost a game by cheat detection, but not marked. 
  )

return(tos_violators)

}


tos_violators <- identify_tos_violators()