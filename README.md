# Lichess4545 Statistics

This repository provides the code behind my Lichess4545 Statistics site (https://rahulan-c.github.io/lichess4545-stats/), which hosts detailed statistical reports for all previous seasons of the Lichess4545 and LoneWolf online chess leagues. 

## How are the stats produced?

There are three key scripts that do most of the work: 

- `/scripts/all_functions.R` has several functions that are called upon to extract and clean data on games, pairings, and player/team positions.
- `scripts/make_openings_sunburst.py` makes sunburst plots of all openings played in a season.
- `/reports/produce_season_stats.Rmd` is the [R Markdown](https://rmarkdown.rstudio.com/) script that produces each season's report.

## Can these stats be replicated?

In theory, almost every step taken to produce the final season reports is replicable. However, in practice, things might not be so straightforward. 

One step that can't be replicated is where all players that have banned on Lichess for ToS violations are removed from the final tables for presentation in a season report before the HTML file is produced. This step refers to non-public data that isn't in this repository.

Also, anyone attempting to reproduce these reports on their own machine may encounter issues when they're trying to make the openings sunburst plots.

Finally, the latest season reports (as of 2021-08-15) refer to a dataset of all 4545/LW games, which is assumed to have been previously compiled. By breaking the modular nature of the process by which data is obtained, saved and analysed for each league and season, this particular step makes complete replication significantly challenging. I hope to amend this step in a future update. 

*Updated 2021-08-15*
