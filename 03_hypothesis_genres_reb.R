
library(data.table)
library(magrittr)
library(tidyr)
library(ggplot2); theme_set(theme_bw())
path = "../IMBD-dataset-exploration-data/"


tb = fread(paste0(path, "/title.basics_cleaned.csv"))
# tb = fread("../IMDB-dataset-exploration-data/title.basics_cleaned.csv", sep2="\\|") # sep2="|" should transform the genres into a list, but it doesn't work
tb[, genres := strsplit(tb[,genres], "\\|")]


# AFTER WORLD WAR II ...

##### ... DID THE DISTRIBUTION OF GENRES CHANGE?
tb_years = tb[(!is.na(startYear)) & (startYear <= 2022) & (startYear >= 1897)]
tb_head = tb_years[sample(1:nrow(tb_years), 10000), 1:9, with = FALSE]
# all_genres = tb[1:1000000, genres] %>% unlist() %>% unique()
all_genres = c("Adventure", "Animation", "Biography", "Comedy", "Crime",
               "Documentary", "Drama", "Family", "Film-Noir", "Game-Show",
               "History", "Horror", "Music", "News", "Reality-TV",
               "Romance", "Sci-Fi", "Short", "Sport", "Talk-Show",
               "Thriller", "War", "Western")

# create dataframe for plot
genres_distr = lapply(all_genres, function(g) {
  x = tb_years[, .(startYear, genres)]
  x = x[lapply(genres, function(i) any(g == i)) %>% unlist(), .N, by = startYear]
  y = x[, .N, by = startYear]
  x = merge(x, y, by = "startYear")
  x[, genre := g]
}) %>% do.call(what = rbind)
genres_distr[, share := N.x / sum(N.y), by = startYear]
genres_distr = merge(merge(data.frame("startYear" = 1897:2022), data.frame("genre" = all_genres)) %>% as.data.table(),
                     genres_distr, all = TRUE, by = c("startYear", "genre"))
genres_distr[, `:=`(N.x = NULL, N.y = NULL)]
genres_distr[is.na(share), share := 0]

# plot
ggplot(genres_distr, aes(x = startYear, y = share)) +
  geom_rect(xmin = 1939, xmax = 1945, ymin = 0, ymax = 1, alpha = 0.005, fill = "red") +
  geom_line() +
  facet_wrap(~ genre, ncol = 4, scales = "free") +
  scale_x_continuous(breaks = seq(1900, 2022, by = 25)) +
  xlab("Start year (red: World War II)") +
  ylab("Share of each genre in all titles of one year") +
  ggtitle("Development of the distribution of genres before and after World War II")
ggsave("plots/03_hypothesis_genres_distribution.jpg", width = 10, height = 8)



##### ... HOW WAS THE GENRE "WAR" CONNECTED TO OTHER GENRES?

all_genres = c("War", "Documentary", "News", "History", "Biography", "Sport", "Crime",
               "Drama", "Adventure", "Romance", "Thriller", "Comedy", "Animation")

# create boolean columns for every genre
tb_genres = tb[!is.na(startYear) & (startYear <= 2022) & (startYear >= 1897), .(startYear, genres)]
tb_genres[, (all_genres) := FALSE]
for (g_col in all_genres) {
  tb_genres[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}

# get "War" in combination with every other genre per year
tb_genres = melt(tb_genres[, !("genres")], id.vars = c("startYear", "War"))
tb_genres = tb_genres[(value)]
tb_genres[, `:=`(genre = variable, variable = NULL, value = NULL)]

# count cases (total, War, share = War / total)
tb_genres = tb_genres[, .N, by = .(startYear, genre, War)]
tb_genres[, `:=`(total = N, N = NULL)]  # rename N to total
tb_genres[, War := as.numeric(War) * total]
tb_genres[, `:=`(total = sum(total), War = sum(War)), by = .(startYear, genre)]
tb_genres[, share := War/total]
tb_genres[is.na(share), share := 0]

# fill up such that every year and genre is there
tb_genres = merge(merge(data.frame("startYear" = 1897:2022), data.frame("genre" = all_genres)) %>% as.data.table(),
                     tb_genres, all = TRUE, by = c("startYear", "genre"))
tb_genres[is.na(War), War := 0][is.na(total), total := 0]
tb_genres = tb_genres[genre != "War"]

# plot
ggplot(tb_genres, aes(x = startYear, y = share)) +
  geom_rect(xmin = 1939, xmax = 1945, ymin = 0, ymax = 1, alpha = 0.005, fill = "red") +
  geom_line() +
  facet_wrap(~ genre, ncol = 4, scales = "free") +
  scale_x_continuous(breaks = seq(1900, 2022, by = 25)) +
  xlab("Start year (red: World War II)") +
  ylab("Share of genre \"War\" in titles of each genre") +
  ggtitle("Contribution of genre \"War\" to other genres before and after World War II")
ggsave("plots/03_hypothesis_genres_war.jpg", width = 10, height = 6)












# not used:
##### ... DID THE CORRELATION PATTERN BETWEEN THE GENRES CHANGE?

all_genres = tb[1:1000000, genres] %>% unlist() %>% unique()
all_genres = c("Adventure", "Animation", "Biography", "Comedy", "Crime",
               "Documentary", "Drama", "Family", "Film-Noir",
               "History", "News", "Romance", "Sport", "War")
all_genres = c("Documentary", "War", "News", "History", "Biography")

prepare_heatmap = function(data, all_genres) {
  
  # create a contingency table for the genres
  tb_genres = data[, .(startYear, genres)]
  tb_genres = tb_genres[!is.na(startYear)]
  tb_genres[, (all_genres) := FALSE]
  for (g_col in all_genres) {
    tb_genres[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
  }
  tb_genres[, genres := NULL]
  dist_dt = tb_genres[, lapply(.SD, function(g1) lapply(.SD, function(g2) 1 - (sum(g1 & g2) / sum(g1 | g2)))), .SDcols = all_genres]  # create contingency table
  
  # transform the contingency table to a usable distance matrix
  dist_mat = as.matrix(dist_dt[, lapply(.SD, as.numeric), .SDcols = all_genres])
  dist_mat[is.na(dist_mat)] <- 1
  dist_mat = (dist_mat - min(dist_mat[dist_mat != 0])) / (max(dist_mat) - min(dist_mat[dist_mat != 0]))
  dist_mat[dist_mat < 0] = 0
  dist_dt = as.data.table(dist_mat)
  
  # prepare the distance matrix for plotting
  # ord = hclust(as.dist(dist_mat), method = "ward.D")$order
  dist_df = as.data.frame(dist_mat)
  dist_df$genre1 = all_genres
  dist_df = pivot_longer(dist_df, cols = all_genres, names_to = "genre2", values_to = "count")
  dist_df$count = as.numeric(dist_df$count)
  dist_df$genre1 = factor(dist_df$genre1, levels = all_genres)
  dist_df$genre2 = factor(dist_df$genre2, levels = all_genres)
  # dist_df$genre1 = factor(dist_df$genre1, levels = all_genres[ord])
  # dist_df$genre2 = factor(dist_df$genre2, levels = all_genres[ord])
  
  return(dist_df)
}

dist_df_WW2 = prepare_heatmap(tb[(startYear <= 1945) & (startYear > 1930)], all_genres)
dist_df_after = prepare_heatmap(tb[(startYear <= 2020) & (startYear > 2005)], all_genres)

ggplot(dist_df_WW2, aes(x = genre1, y = genre2, fill = count)) +
  geom_tile(color = "grey")
ggplot(dist_df_after, aes(x = genre1, y = genre2, fill = count)) +
  geom_tile(color = "grey")
