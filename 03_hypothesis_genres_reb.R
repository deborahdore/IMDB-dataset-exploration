
library(data.table)
library(magrittr)
library(tidyr)
# library(cld2)
library(ggplot2); theme_set(theme_bw())
library(viridis)
path = "../IMBD-dataset-exploration-data/"


tb = fread(paste0(path, "/title.basics_cleaned.csv"))
tb = fread("../IMDB-dataset-exploration-data/title.basics_cleaned.csv") # sep2="|" should transform the genres into a list, but it doesn't work

# MORE PREPROCESSING
# delete unused columns
tb = tb[, `:=`(primaryTitle = NULL, originalTitle = NULL, isAdult = NULL, endYear = NULL, runtimeMinutes = NULL)]
gc()
# delete in total 7737722 rows (83.5%) ==> 1530175 rows remaining
tb = tb[(!is.na(startYear)) & (startYear <= 2022) & (startYear >= 1897)] # delete 1245621 rows (13.4%)
tb = tb[genres != ""] # delete 427915 rows (4.6%)
tb = tb[titleType %in% c("movie", "short", "tvSeries", "tvMovie")] # delete 7381423 rows (79.6% of which 75.4% tvEpisode)
# transform genres to a list
tb[, genres := strsplit(genres, "\\|")]


# AFTER WORLD WAR II ...

##### ... DID THE DISTRIBUTION OF GENRES CHANGE?

# all_genres = tb[1:1000000, genres] %>% unlist() %>% unique()
all_genres = c("History", "Drama", "Family", "Music",
               "War", "Romance", "Animation", "Musical",
               "Documentary", "Crime", "Adventure", "Sport",
               "Biography", "Comedy", "Action", "Talk-Show",
               "News", "Short", "Sci-Fi", "Film-Noir")

# create dataframe for plot
tb_distr = tb
for (g_col in all_genres) {
  tb_distr[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
tb_distr = tb_distr[, lapply(.SD, sum), by = .(startYear, titleType), .SDcols = all_genres]
tb_distr = melt(tb_distr, id.vars = c("startYear", "titleType"), variable.name = "genre", value.name = "count")
tb_distr[, total := sum(count), by = startYear]
tb_distr[, share := count/total]
tb_distr[, `:=`(count = NULL, total = NULL)]

tb_distr_nocolor = copy(tb_distr)
tb_distr_nocolor[, titleType := NULL]
tb_distr_nocolor[, share := sum(share), by = .(startYear, genre)]
tb_distr_nocolor = unique(tb_distr_nocolor)
tb_distr_text = copy(tb_distr_nocolor)
tb_distr_text[, startYear := NULL]
tb_distr_text[, share := max(share), by = genre]
tb_distr_text = unique(tb_distr_text)
# tb_distr_text[, titleType := 1]


# plot without color
ggplot(tb_distr_nocolor, aes(x = startYear, y = share)) +
  geom_rect(xmin = 1929, xmax = 1945, ymin = 0, ymax = 1.2, alpha = 0.05, fill = "grey") +
  geom_line(position = "stack", alpha = 0.8) +
  facet_wrap(~ genre, ncol = 4, scales = "free") +
  scale_x_continuous(breaks = seq(1900, 2022, by = 25)) +
  xlab("Start year") +
  ylab("Share of each genre in all titles of one year") +
  geom_text(data = tb_distr_text, mapping = aes(x = 1937, y = share * 0.95, label = "1929 - 1945", fill = NULL), size = 3) +
  theme(axis.title = element_text(size=14,face="bold"))
ggsave("plots/03_hypothesis_genres_distribution.jpg", width = 14, height = 10)


# plot with color
ggplot(tb_distr, aes(x = startYear, y = share, fill = titleType)) +
  geom_rect(xmin = 1929, xmax = 1945, ymin = 0, ymax = 1, alpha = 0.8, fill = "grey") +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap(~ genre, ncol = 4, scales = "free") +
  scale_x_continuous(breaks = seq(1900, 2022, by = 25)) +
  scale_fill_viridis(discrete = TRUE, option = "D", end = 0.8) +
  xlab("Start year") +
  ylab("Share of each genre in all titles of one year") +
  geom_text(data = tb_distr_text, mapping = aes(x = 1937, y = share * 0.95, label = "1929 - 1945", fill = NULL), size = 3) +
  theme(axis.title = element_text(size=14,face="bold"))
ggsave("plots/03_hypothesis_genres_distribution_colored.jpg", width = 14, height = 10)




# ... IS THE DISTRIBUTION OF GENRES DIFFERENT FROM THE TIME PERIOD 1999-2015?

# all_genres = tb[1:1000000, genres] %>% unlist() %>% unique()
all_genres = all_genres[which(all_genres != "War")]

all_genres = c("Drama", "Short", "Documentary", "Romance", "Comedy", "History", "Adventure", "Action", "Thriller", "Biography")

# create data.table for the plot
tb_war = rbind(tb[(startYear >= 1929) & (startYear <= 1945)],
               tb[(startYear >= 1999) & (startYear <= 2015)])
tb_war[, Period := ifelse((startYear >= 1929) & (startYear <= 1945), "1929-1945", "1999-2015") %>% factor()]
tb_war = tb_war[lapply(genres, function(g) any(g == "War")) %>% unlist()]
for (g_col in all_genres) {
  tb_war[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
tb_war = tb_war[, lapply(.SD, sum), by = Period, .SDcols = all_genres]
tb_war = melt(tb_war, id.vars = "Period", variable.name = "genre", value.name = "count")
tb_war[, total := sum(count), by = Period]
tb_war[, share := count/total]
o = order(tb_war[Period == "1929-1945", count])
tb_war = tb_war[order(Period, -share)]
tb_war[, genre := factor(genre, levels = tb_war[seq_along(all_genres), genre])]

# plot
ggplot(tb_war, aes(x = genre, y = share, fill = Period, label = Period)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Start year") +
  ylab("Share of genres which occur together with genre \"War\"") +
  theme(legend.position = c(0.9, 0.8)) +
  scale_fill_viridis(discrete = TRUE, begin = 0, end = 0.8) +
  theme(axis.title = element_text(size=10,face="bold"))
ggsave("plots/03_hypothesis_genres_war.jpg", width = 8, height = 5)

