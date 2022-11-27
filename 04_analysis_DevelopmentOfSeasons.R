
library(data.table)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
options(scipen = 1000000)
library(scales)
library(viridis)

path = "../IMDB-dataset-exploration-data/"

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
, ncol = 4, common.legend = TRUE, legend = "right")
ggsave("plots/04_analysis_SeasonsBoxplots.pdf", height = 3, width = 14)

# Difference to previous season
# seasons[, diffRating := NaN]
# seasons[, diffRating := as.numeric(diffRating)]
# for (i in 2:seasons[, max(seasonNumber)]) {
#   t1 = seasons[seasonNumber == i-1, tconst]
#   t2 = seasons[seasonNumber == i, tconst]
#   t = base::intersect(t1, t2)
#   prev = seasons[(tconst %in% t) & seasonNumber == i-1, averageRatingSeason]
#   seasons[(tconst %in% t) & (seasonNumber == i), diffRating := averageRatingSeason - prev]
# }

# difference in ratings
# k = 5
# ggplot(seasons[!is.na(diffRating) & (seasonNumber <= k)],
#        aes(x = factor(seasonNumber), y = diffRating, fill = success)) +
#   geom_violin() +
#   coord_cartesian(ylim = c(-3, 3))
# 
# ggplot(seasons[!is.na(diffRating) & (seasonNumber <= k)],
#        aes(x = diffRating, y = numVotesSeason, color = success, alpha = success)) +
#   geom_point() +
#   scale_alpha_manual(values = c(0.1, 1)) +
#   scale_y_continuous(trans = "log10") +
#   facet_wrap(~ seasonNumber)

# genres
season_genres = seasons
season_genres[, genres := strsplit(genres, "\\|")]
# all_genres = c("Action", "Adventure", "Animation", "Comedy", "Crime", "Drama", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller")
all_genres = c("Action", "Crime", "Mystery", "Drama", "Sci-Fi", "Comedy")
for (g_col in all_genres) {
  season_genres[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
season_genres = melt(season_genres,
                     id.vars = c("tconst", "seasonNumber", "maxSeasonNumber", "averageRatingSeason", "success", "numVotesSeason", "averageRuntimeMinutes", "nTranslations"),
                     measure.vars = all_genres,
                     # id.vars = c("tconst", "seasonNumber", "maxSeasonNumber", "nEpisodes", "averageRatingSeason", "averageRuntimMinutes", "success", "numVotesSeason"),
                     variable.name = "genre", value.name = "genreBool")
season_genres = season_genres[(genreBool)][, genreBool := NULL]
season_genres[, icon := ifelse(success, "plots/icon_color.png", "plots/icon_black.png")]

# Barplot with genres
# box_genre = season_genres[, .(seasonNumber, genre, success)]
# box_genre[, count := .N, by = c("seasonNumber", "genre", "success")]
# box_genre = unique(box_genre)
# box_genre[, total := sum(count), by = .(seasonNumber, genre)]
# k = 4
# ggplot(box_genre[(seasonNumber <= k) & (success)], aes(x = genre, y = count/total, fill = genre)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(x = genre, y = count/total+0.003, label = paste0(round(100*count/total), "%"))) +
#   facet_wrap(~ seasonNumber, ncol = k) +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 40, hjust=1))
# ggsave("plots/04_analysis_DevelopmentOfSeasons_barplot.pdf", height = 5, width = 12)


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

ggsave("plots/04_analysis_DevelopmentOfSeasons.pdf", height = 6, width = 14)

# nTranslation doesn't differ between seasons

