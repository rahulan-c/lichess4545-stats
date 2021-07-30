``` r
if (!require("pacman")) install.packages("pacman")
```

    ## Loading required package: pacman

``` r
pacman::p_load(tidyverse, knitr)
```

# Hello

Statistical reports for the [Lichess4545](https://www.lichess4545.com/)
[Team](https://www.lichess4545.com/team4545/) (45+45) and
[LoneWolf](https://www.lichess4545.com/lonewolf/) (30+30) online chess
leagues.

``` r
completed_seasons <- c("4545_s20")

completed <- tibble("season" = completed_seasons)

completed <- completed %>% 
  mutate(report_url = paste0("https://rahulan-c.github.io/lichess4545-stats/reports/stats_", completed_seasons, ".html"))

knitr::kable(completed)
```

| season   | report_url                                                                  |
|:---------|:----------------------------------------------------------------------------|
| 4545_s20 | <https://rahulan-c.github.io/lichess4545-stats/reports/stats_4545_s20.html> |
