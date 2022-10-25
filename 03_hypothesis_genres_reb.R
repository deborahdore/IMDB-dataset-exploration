
library(data.table)
library(magrittr)
library(tidyr)
library(cld2)
library(ggplot2); theme_set(theme_bw())
library(viridis)
options(scipen = 1000000)
path = "../IMBD-dataset-exploration-data/"


tb = fread(paste0(path, "/title.basics_cleaned.csv"))
tb = fread("../IMDB-dataset-exploration-data/title.basics_cleaned.csv") # sep2="|" should transform the genres into a list, but it doesn't work

# MORE PREPROCESSING
# delete unused columns
tb = tb[, `:=`(primaryTitle = NULL, originalTitle = NULL, isAdult = NULL, endYear = NULL, runtimeMinutes = NULL)]
gc()
# delete in total 7737722 rows (83.5%) ==> 1530175 rows remaining
1 - tb[titleType %in% c("movie", "short", "tvSeries", "tvSeries", "tvEpisode"), .N] / tb[, .N]  # delete 1245621 rows (13.4%)
tb = tb[(!is.na(startYear)) & (startYear <= 2022) & (startYear >= 1897)]
tb = tb[genres != ""] # delete 427915 rows (4.6%)
tb = tb[titleType %in% c("movie", "short", "tvSeries", "tvSeries")] # delete 7519250 rows (81.1% of which 75.4% tvEpisode)
# transform genres to a list
tb[, genres := strsplit(genres, "\\|")]


# AFTER WORLD WAR II ...

##### ... DID THE DISTRIBUTION OF GENRES CHANGE?

# all_genres = tb[1:1000000, genres] %>% unlist() %>% unique()
all_genres = c("History", "Drama", "Family", "Music",
               "War", "Romance", "Crime", "Musical",
               "Documentary", "Crime", "Adventure", "Sport",
               "Biography", "Comedy", "Action", "Talk-Show",
               "News", "Short", "Sci-Fi", "Film-Noir")

# create dataframe for plot
tb_distr = tb
tb_distr[, `:=`(tconst = NULL, primaryTitle = NULL, originalTitle = NULL, isAdult = NULL, endYear = NULL, runtimeMinutes = NULL)]
for (g_col in all_genres) {
  tb_distr[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
tb_distr2 = tb_distr
tb_distr = tb_distr[, lapply(.SD, sum), by = startYear, .SDcols = all_genres]
tb_distr = melt(tb_distr, id.vars = "startYear", variable.name = "genre", value.name = "count")
tb_distr[, total := sum(count), by = startYear]
tb_distr[, share := count/total]
tb_distr[, ypos := max(share), by = genre]

# plot
ggplot(tb_distr, aes(x = startYear, y = share)) +
  # annotate("text", label = "World War II", x = 1915, y = 0.01, size = 3) +
  geom_area(position = "stack") +
  facet_wrap(~ genre, ncol = 4, scales = "free") +
  scale_x_continuous(breaks = seq(1900, 2022, by = 25)) +
  # scale_fill_viridis(discrete = TRUE, option = "H") +
  geom_rect(xmin = 1939, xmax = 1945, ymin = 0, ymax = 1, alpha = 0.005, fill = "grey") +
  xlab("Start year      (red: World War II)") +
  ylab("Share of each genre in all titles of one year") +
  ggtitle("Development of the distribution of genres before and after World War II")
  # geom_text(mapping = aes(x = 1945, y = ymax * 0.95, label = "World War II"), data = unique(tb_distr[, .(genre, ymax)]), size = 3) +
ggsave("plots/03_hypothesis_genres_distribution_colored.jpg", width = 10, height = 8)






##### ... IS THE DEVELOPMENT OF THE GENRE WAR DIFFERENT IN ITALY, GERMANY AND OTHERS?

# preprocessing
ta = fread("../IMDB-dataset-exploration-data/title.akas_cleaned.csv")
colnames(ta)[[1]] = "tconst"
ta = ta[types == "original"]  # filter original titles
ta[region == "", region := NA]
ta[language == "", language := NA]
ta[is.na(language), language := detect_language(title)]
gc()

m1 = merge(ta, tb, by=c("tconst"), all.y = TRUE)
m1[tconst %in% m1[, .N, by = tconst][N > 1, tconst]] %>% View()
# 133 rows have duplicate IDs ==> these titles have different titles in both data sets
# m1[tconst %in% m1[, .N, by = tconst][N > 1, tconst], remove := TRUE]
# m1[(remove) & (!is.na(region) | !is.na(language)), remove := FALSE]

# TODO: delete the first of duplicate column
m1[, keep := TRUE]
m1[tconst %in% m1[, .N, by = tconst][N > 1, tconst], keep := lapply(tconst, function(t) {
  
})]










### BELOW NOT USED ------------------------------------------------------------

# Analyzing missing values
tb[, genres_list := strsplit(genres, "\\|")]
nrow(tb[(genres == "") & is.na(startYear)])  # 77232 (0.8%) titles have neither a startYear nor a genre
# missing startYear
missing_startYear = table(tb[is.na(startYear), genres_list] %>% unlist()) %>% sort(decreasing = TRUE) %>% as.data.frame()
ggplot(missing_startYear, aes(x = Var1, y = Freq)) +
  ggtitle("Genres of titles with missing start year") +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("plots/03_hypothesis_genres_missing_startYear.jpg", width = 8, height = 4)
# missing genre
missing_genre = table(tb[(genres == "") & !is.na(startYear), startYear]) %>% as.data.frame()
ggplot(missing_genre, aes(x = Var1, y = Freq)) +
  ggtitle("Start years with missing genre") +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Count") +
  scale_x_discrete(breaks = seq(1900, 2020, 10))
ggsave("plots/03_hypothesis_genres_missing_genre.jpg", width = 8, height = 4)


##### ... WHAT IS THE DISTRIBUTION OF THE GENRE "WAR" IN THE OTHER GENRES?
all_genres = c("Comedy", "Short", "Documentary", "News",
               "Fantasy", "Drama", "Biography",
               "Romance", "Family", "Adventure", "Action", "History")
tb_war = tb[, .(startYear, genres)]
tb_war[, War := lapply(genres, function(g) "War" %in% g) %>% unlist()]
tb_war = tb_war[War == TRUE]
for (g_col in all_genres) {
  tb_war[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
tb_war[, genres := NULL]
tb_war = tb_war[, lapply(.SD, sum), by = startYear, .SDcols = c(all_genres, "War")]
tb_war = melt(tb_war, id.vars = c("startYear", "War"))
tb_war[, genre := variable][, variable := NULL]
tb_war[, total := value / War, by = startYear]

ggplot(tb_war, aes(x = startYear, y = War)) +
  geom_line() +
  xlab("Start year") +
  ylab("Titles of genre \"War\"")
ggsave("plots/03_hypothesis_genres_war_timeline.jpg", width = 6, height = 3)

ggplot(tb_war, aes(x = startYear)) +
  geom_rect(xmin = 1939, xmax = 1945, ymin = 0, ymax = 0.9, alpha = 0.005, fill = "red") +
  annotate("text", label = "World War II", x = 1945, y = 0.95, size = 3) +
  geom_line(aes(y = total)) +
  facet_wrap(~ genre, ncol = 4) +
  # coord_cartesian(ylim = c(0, 1)) +
  # ggtitle("Contribution of each genre to the genre \"War\" before and after World War II") +
  xlab("Start year") +
  ylab("Share of each genre in all titles of genre \"War\"")
ggsave("plots/03_hypothesis_genres_war.jpg", width = 10, height = 6)




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
