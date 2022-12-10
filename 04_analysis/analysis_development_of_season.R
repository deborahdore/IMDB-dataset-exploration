library(data.table)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
options(scipen = 1000000)
library(scales)
library(viridis)


setwd("..")
path <- paste0(getwd(), "/dataset")

seasons = fread(paste0(path, "/seasons.csv"))
series = fread(paste0(path, "/merged_series_withNA.csv"))

sum(seasons[, parentTconst] %in% series[, tconst])  # 27505
sum(series[, tconst] %in% seasons[, parentTconst])  # 13515

# merge
colnames(seasons)[[2]] = "tconst"
seasons = seasons[tconst %in% series[, tconst]]
seasons = merge(seasons[, .(ID, tconst, seasonNumber, nEpisodes, averageRuntimeMinutes, genres, numVotes, averageRating, minRating, maxRating)],
                series[, .(tconst, primaryTitle, success, averageRating, numVotes, runtimeMinutes, nTranslations, nSeasons, maxSeasonNumber, startYear)],
                by = "tconst", suffixes = c("Season", "Series"))
seasons[(success), tconst] %>% unique() %>% length()

# check for missing seasons
seasons[maxSeasonNumber > nSeasons, tconst] %>% unique() %>% length() # 147 have not all seasons in the data set --> delete
seasons = seasons[maxSeasonNumber == nSeasons]

series = unique(seasons[, .(tconst, primaryTitle, success, averageRatingSeries, numVotesSeries, runtimeMinutes, nTranslations, nSeasons, startYear)])
series[, Seasons := ifelse(nSeasons == 1, "1", ifelse(nSeasons %in% 2:4, "2-4", ifelse(nSeasons %in% 5:7, "5-7", ifelse(nSeasons %in% 8:12, "8-12", "13+"))))]
series[, Seasons := factor(Seasons, levels = c("1", "2-4", "5-7", "8-12", "13+"))]

# select seasons with individual rating
seasons[, individualRating := (sd(averageRatingSeason, na.rm = TRUE) != 0), by = tconst]
seasons[(individualRating), .N, by = tconst]  # 4249 series have individual ratings for at least one episode
seasons = seasons[(individualRating)][, individualRating := NULL]

# Boxplot average rating
ggpubr::ggarrange(
ggplot(series, aes(x = Seasons, y = averageRatingSeries, alpha = success)) +
  scale_alpha_manual(values = c(0.2, 0.8),
                    labels = c("no", "yes")) +
  geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5)) +
  xlab("Number of Seasons") + ylab("") +
  ggtitle("Average Rating")
,
ggplot(series, aes(x = Seasons, y = numVotesSeries, alpha = success)) +
  scale_y_continuous(trans = "log10") +
  scale_alpha_manual(values = c(0.2, 0.8),
                     labels = c("no", "yes")) +
  geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5)) +
  xlab("Number of Seasons") + ylab("") +
  ggtitle("Number of Votes")
,
ggplot(series, aes(x = Seasons, y = nTranslations, alpha = success)) +
  scale_alpha_manual(values = c(0.2, 0.8),
                     labels = c("no", "yes")) +
  geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5)) +
  coord_cartesian(ylim = c(0, 80)) +
  xlab("Number of Seasons") + ylab("") +
  ggtitle("Number of Translations")
,
ggplot(series[runtimeMinutes <= 200], aes(x = Seasons, y = runtimeMinutes, alpha = success)) +
  scale_alpha_manual(values = c(0.2, 0.8),
                     labels = c("no", "yes")) +
  geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5)) +
  coord_cartesian(ylim = c(0, 100)) +
  xlab("Number of Seasons") + ylab("") +
  ggtitle("Runtime in Minutes")
, ncol = 2, nrow=2, common.legend = TRUE, legend = "right")
ggsave("plots/analysis/analysis_SeasonsBoxplots.pdf", height = 5, width = 7)


# genres
season_genres = seasons
season_genres[, genres := strsplit(genres, "\\|")]
# all_genres = c("Action", "Adventure", "Animation", "Comedy", "Crime", "Drama", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller")
all_genres = c("Action", "Comedy", "Crime", "Drama", "Mystery")
for (g_col in all_genres) {
  season_genres[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
season_genres = melt(season_genres,
                     id.vars = c("tconst", "seasonNumber", "maxSeasonNumber", "averageRatingSeason", "success", "numVotesSeason", "averageRuntimeMinutes", "nTranslations"),
                     measure.vars = all_genres,
                     # id.vars = c("tconst", "seasonNumber", "maxSeasonNumber", "nEpisodes", "averageRatingSeason", "averageRuntimMinutes", "success", "numVotesSeason"),
                     variable.name = "genre", value.name = "genreBool")
season_genres = season_genres[(genreBool)][, genreBool := NULL]
season_genres[, icon := ifelse(success, "plots/analysis/icon_color.png", "plots/analysis/icon_black.png")]

# Line plot with genres
k = 8
linesize = 0.5
ggpubr::ggarrange(
# average Rating
ggplot() +
  geom_violin(data = season_genres[(seasonNumber <= k) & !(success)],
               mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRatingSeason)) +
  geom_violin(data = season_genres[(seasonNumber <= k) & (success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRatingSeason, color = genre, fill = genre),
              alpha = 0.2, size = linesize) +
  geom_line(data = season_genres[(seasonNumber <= k) & !(success)],
            mapping = aes(x = seasonNumber, y = averageRatingSeason, group = tconst),
            alpha = 0.1) +
  geom_line(data = season_genres[(seasonNumber <= k) & (success)],
            mapping = aes(x = seasonNumber, y = averageRatingSeason, group = tconst, color = genre),
            alpha = 0.7, size = linesize) +
  scale_x_continuous(breaks = 1:k) +
  scale_fill_viridis(discrete = TRUE, end = 0.85) +
  scale_color_viridis(discrete = TRUE, end = 0.85) +
  ylab("Average Rating\nof Episode") + xlab("") +
  facet_wrap(~ genre, ncol = 6) +
  theme(legend.position = "none"),
# number of votes
ggplot() +
  geom_violin(data = season_genres[(seasonNumber <= k) & !(success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = numVotesSeason)) +
  geom_violin(data = season_genres[(seasonNumber <= k) & (success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = numVotesSeason, color = genre, fill = genre),
              alpha = 0.2, size = linesize) +
  geom_line(data = season_genres[(seasonNumber <= k) & !(success)],
            mapping = aes(x = seasonNumber, y = numVotesSeason, group = tconst),
            alpha = 0.1) +
  geom_line(data = season_genres[(seasonNumber <= k) & (success)],
            mapping = aes(x = seasonNumber, y = numVotesSeason, group = tconst, color = genre),
            alpha = 0.7, size = linesize) +
  scale_x_continuous(breaks = 1:k) +
  scale_y_continuous(trans = "log10") +
  scale_fill_viridis(discrete = TRUE, end = 0.85) +
  scale_color_viridis(discrete = TRUE, end = 0.85) +
  ylab("Average Number of Votes\nof Episodes") + xlab("") +
  facet_wrap(~ genre, ncol = 6) +
  theme(legend.position = "none"),
# run time
ggplot() +
  geom_violin(data = season_genres[(seasonNumber <= k) & !(success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRuntimeMinutes)) +
  geom_violin(data = season_genres[(seasonNumber <= k) & (success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRuntimeMinutes, color = genre, fill = genre),
              alpha = 0.2, size = linesize) +
  geom_line(data = season_genres[(seasonNumber <= k) & !(success)],
            mapping = aes(x = seasonNumber, y = averageRuntimeMinutes, group = tconst),
            alpha = 0.1) +
  geom_line(data = season_genres[(seasonNumber <= k) & (success)],
            mapping = aes(x = seasonNumber, y = averageRuntimeMinutes, group = tconst, color = genre),
            alpha = 0.7, size = linesize) +
  scale_x_continuous(breaks = 1:k) +
  scale_y_continuous(breaks = c(20, 40, 60, 80)) +
  scale_fill_viridis(discrete = TRUE, end = 0.85) +
  scale_color_viridis(discrete = TRUE, end = 0.85) +
  ylab("Average Runtime\nof Episodes in Minutes") + xlab("Season Number") +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ genre, ncol = 6) +
  theme(legend.position = "none"),
ncol = 1)

ggsave("plots/analysis/analysis_DevelopmentOfSeasons.pdf", height = 6, width = 12)
