
library(data.table)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
options(scipen = 1000000)

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
seasons[!(individualRating), .N, by = tconst]  # 4249 series have individual ratings for at least one episode
seasons = seasons[(individualRating)][, individualRating := NULL]

# Boxplot average rating
ggplot(series, aes(x = Seasons, y = averageRatingSeries, fill = success)) +
  geom_boxplot()
ggsave("plots/04_analysis_DevelopmentOfSeasons_nSeasonsRating.pdf", height = 4, width = 8)

# Difference to previous season
seasons[, diffRating := NaN]
seasons[, diffRating := as.numeric(diffRating)]
for (i in 2:seasons[, max(seasonNumber)]) {
  t1 = seasons[seasonNumber == i-1, tconst]
  t2 = seasons[seasonNumber == i, tconst]
  t = base::intersect(t1, t2)
  prev = seasons[(tconst %in% t) & seasonNumber == i-1, averageRatingSeason]
  seasons[(tconst %in% t) & (seasonNumber == i), diffRating := averageRatingSeason - prev]
}

# difference in ratings
k = 5
ggplot(seasons[!is.na(diffRating) & (seasonNumber <= k)],
       aes(x = factor(seasonNumber), y = diffRating, fill = success)) +
  geom_violin() +
  coord_cartesian(ylim = c(-3, 3))

ggplot(seasons[!is.na(diffRating) & (seasonNumber <= k)],
       aes(x = diffRating, y = numVotesSeason, color = success, alpha = success)) +
  geom_point() +
  scale_alpha_manual(values = c(0.1, 1)) +
  scale_y_continuous(trans = "log10") +
  facet_wrap(~ seasonNumber)

# genres
season_genres = seasons
season_genres[, genres := strsplit(genres, "\\|")]
all_genres = c("Action", "Adventure", "Animation", "Comedy", "Crime", "Drama", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller")
for (g_col in all_genres) {
  season_genres[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
season_genres = melt(season_genres,
                     id.vars = c("tconst", "seasonNumber", "maxSeasonNumber", "averageRatingSeason", "success", "numVotesSeason"),
                     measure.vars = all_genres,
                     # id.vars = c("tconst", "seasonNumber", "maxSeasonNumber", "nEpisodes", "averageRatingSeason", "averageRuntimMinutes", "success", "numVotesSeason"),
                     variable.name = "genre", value.name = "genreBool")
season_genres = season_genres[(genreBool)][, genreBool := NULL]

# Barplot with genres
box_genre = season_genres[, .(seasonNumber, genre, success)]
box_genre[, count := .N, by = c("seasonNumber", "genre", "success")]
box_genre = unique(box_genre)
box_genre[, total := sum(count), by = .(seasonNumber, genre)]
k = 4
ggplot(box_genre[(seasonNumber <= k) & (success)], aes(x = genre, y = count/total, fill = genre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = genre, y = count/total+0.003, label = paste0(round(100*count/total), "%"))) +
  facet_wrap(~ seasonNumber, ncol = k) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 40, hjust=1))
ggsave("plots/04_analysis_DevelopmentOfSeasons_barplot.pdf", height = 5, width = 12)

# Line plot with genres
k = 10
# average Rating
ggplot() +
  geom_violin(data = season_genres[(seasonNumber <= k) & !(success)],
               mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRatingSeason)) +
  geom_violin(data = season_genres[(seasonNumber <= k) & (success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRatingSeason, color = genre, fill = genre),
              alpha = 0.2) +
  geom_line(data = season_genres[(seasonNumber <= k) & !(success)],
            mapping = aes(x = seasonNumber, y = averageRatingSeason, group = tconst),
            alpha = 0.1) +
  geom_line(data = season_genres[(seasonNumber <= k) & (success)],
            mapping = aes(x = seasonNumber, y = averageRatingSeason, group = tconst, color = genre)) +
  scale_x_continuous(breaks = 1:k) +
  facet_wrap(~ genre) +
  theme(legend.position = "none")
ggsave("plots/04_analysis_DevelopmentOfSeasons_rating.pdf", height = 8, width = 12)
# number of votes
ggplot() +
  geom_violin(data = season_genres[(seasonNumber <= k) & !(success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = numVotesSeason)) +
  geom_violin(data = season_genres[(seasonNumber <= k) & (success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = numVotesSeason, color = genre, fill = genre),
              alpha = 0.2) +
  geom_line(data = season_genres[(seasonNumber <= k) & !(success)],
            mapping = aes(x = seasonNumber, y = numVotesSeason, group = tconst),
            alpha = 0.1) +
  geom_line(data = season_genres[(seasonNumber <= k) & (success)],
            mapping = aes(x = seasonNumber, y = numVotesSeason, group = tconst, color = genre)) +
  scale_x_continuous(breaks = 1:k) +
  scale_y_continuous(trans = "log10") +
  facet_wrap(~ genre) +
  theme(legend.position = "none")
ggsave("plots/04_analysis_DevelopmentOfSeasons_votes.pdf", height = 8, width = 12)
# run time
ggplot() +
  geom_violin(data = season_genres[(seasonNumber <= k) & !(success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = numVotesSeason)) +
  geom_violin(data = season_genres[(seasonNumber <= k) & (success)],
              mapping = aes(x = seasonNumber, group = seasonNumber, y = numVotesSeason, color = genre, fill = genre),
              alpha = 0.2) +
  geom_line(data = season_genres[(seasonNumber <= k) & !(success)],
            mapping = aes(x = seasonNumber, y = numVotesSeason, group = tconst),
            alpha = 0.1) +
  geom_line(data = season_genres[(seasonNumber <= k) & (success)],
            mapping = aes(x = seasonNumber, y = numVotesSeason, group = tconst, color = genre)) +
  scale_x_continuous(breaks = 1:k) +
  scale_y_continuous(trans = "log10") +
  facet_wrap(~ genre) +
  theme(legend.position = "none")
ggsave("plots/04_analysis_DevelopmentOfSeasons_votes.pdf", height = 8, width = 12)
