
library(data.table)
library(dplyr)
library(ggplot2); theme_set(theme_bw())

seasons = fread("../IMDB-dataset-exploration-data/seasons.csv")


# drop series without rating
seasons[is.na(averageRating), .N]  # 68558 seasons don't have a rating
seasons[is.na(averageRating), parentTconst] %>% unique() %>% length()  # from 50421 series
seasons = seasons[!is.na(averageRating)]

# look at seasonNumbers > 25
k = 25
long_series = unique(seasons[seasonNumber >= k, parentTconst])  # 447 series have a seasonNumber over 25
seasons[(parentTconst %in% long_series), .(.N, maxSeasonNumber = max(seasonNumber)), by = parentTconst] %>% View()
# drop seasons with seasonNumber > 25
seasons = seasons[seasonNumber <= 25]

# select seasons with individual rating
seasons[, individualRating := (sd(averageRating) != 0), by = parentTconst]
seasons[(individualRating), .N, by = parentTconst]  # 7629 series have individual ratings for at least one episode
seasons = seasons[(individualRating)]

k = 5
seasons_atleastk = seasons
seasons_atleastk[, maxSeasonNumber := max(seasonNumber), by = parentTconst]
seasons_atleastk = seasons_atleast10[maxSeasonNumber >= k]

ggplot(seasons_atleastk, aes(x = seasonNumber, y = averageRating, group = parentTconst)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.3, size = 0.1) +
  coord_cartesian(xlim = c(0.8, k))
