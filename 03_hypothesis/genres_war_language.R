library(data.table)
library(magrittr)
library(tidyr)
library(ggplot2); theme_set(theme_bw())
library(viridis)

setwd("..")
path <- paste0(getwd(), "/dataset")

# Do the same (and more) preprocessing for original titles
original = fread(paste0(path, "/title.original.csv"))

# original = fread(paste0(path, "title.original.csv"))
nrow(original) - length(unique(original[, tconst]))  # 164 duplicate tconst
original[, N := .N, by = tconst]
original[N > 1, N := lapply(unique(tconst), function(t) {
  dat = original[tconst == t]
  keep = !is.na(dat[, language]) | !is.na(dat[, region])
  if (sum(keep) == 1) return(keep)
  keep = !is.na(dat[, language])
  if (sum(keep) == 1) return(keep)
  else return(c(TRUE, rep(FALSE, nrow(dat) - 1)))
}) %>% unlist()]
original = original[N == 1]
original[, N := NULL]
nrow(original) - length(unique(original[, tconst]))  # now everything is all right

original[, `:=`(tconst = NULL, title = NULL, isAdult = NULL, runtimeMinutes = NULL, averageRating = NULL, numVotes = NULL)]
original = original[(!is.na(startYear)) & (startYear <= 2022) & (startYear >= 1897)]  # delete 1245621 rows (13.4%)
original = original[genres != ""]  # delete 427915 rows (4.6%)
original = original[titleType %in% c("movie", "short", "tvSeries", "tvMovie")]  # delete 7381423 rows (79.6% of which 75.4% tvEpisode)
original[, language := ifelse(language == "it", "Italian", ifelse(language == "de", "German", "Other"))]
original[, genres := strsplit(genres, "\\|")]
gc()



### GENRES TO ANALYZE ---------------------------------------------------------
all_genres = c("History", "Drama", "Family", "Music",
               "War", "Romance", "Animation", "Musical",
               "Documentary", "Crime", "Adventure", "Sport",
               "Biography", "Comedy", "Action", "Talk-Show",
               "News", "Short", "Sci-Fi", "Film-Noir")

all_genres = c("History", "Drama", "Family", "News",
               "War", "Romance", "Animation", "Music",
               "Documentary", "Crime", "Adventure", "Sport",
               "Biography", "Comedy", "Action", "Talk-Show")#

all_genres = c("History", "War", "Documentary", "Talk-Show",
               "Drama", "Romance", "Crime", "Comedy",
               "Family", "Animation", "Adventure", "Action")

all_genres = c("War", "History", "Family", "Crime",
               "Talk-Show", "Biography", "Comedy", "Adventure")

### PLOT GENRE AND TITLE TYPE -------------------------------------------------
tb_distr = original
tb_distr = tb_distr[(startYear >= 1920) & (startYear <= 1960)]
for (g_col in all_genres) {
  tb_distr[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
tb_distr = tb_distr[, lapply(.SD, sum), by = .(startYear, titleType), .SDcols = all_genres]
tb_distr = melt(tb_distr, id.vars = c("startYear", "titleType"), variable.name = "genre", value.name = "count")
tb_distr[, total := sum(count), by = .(startYear)]
tb_distr[, share := count/total]
tb_distr[, `:=`(count = NULL, total = NULL)]
tb_distr[, Type := titleType]

tb_distr_nocolor = copy(tb_distr)
tb_distr_nocolor[, titleType := NULL]
tb_distr_nocolor[, share := sum(share), by = .(startYear, genre)]
tb_distr_nocolor = unique(tb_distr_nocolor)
tb_distr_text = copy(tb_distr_nocolor)
tb_distr_text[, startYear := NULL]
tb_distr_text[, share := max(share), by = genre]
tb_distr_text = unique(tb_distr_text)

ggplot(tb_distr, aes(x = startYear, y = share, fill = Type)) +
  geom_rect(xmin = 1933, xmax = 1945, ymin = 0, ymax = 1, alpha = 0.8, fill = "grey60") +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap(~ genre, ncol = 4, scales = "free") +
  scale_x_continuous(breaks = seq(1920, 1960, by = 10)) +
  scale_fill_viridis(discrete = TRUE, option = "D", end = 0.8) +
  xlab("Start year") +
  ylab("Share of selected genres\nin all titles of one year") +
  geom_text(data = tb_distr_text, mapping = aes(x = 1939, y = share * 0.95, label = "1933 - 1945", fill = NULL), size = 2.5) +
  theme(axis.title = element_text(size=12,face="bold"))
ggsave("plots/EDA/hypothesis_genres_types.pdf", width = 10, height = 3)




# ... IS THE DISTRIBUTION OF THE GENRE WAR DIFFERENT FROM THE TIME PERIOD 1999-2015?

all_genres = c("Drama", "Short", "Documentary", "Romance", "Comedy", "History", "Action")


# create data.table for the plot
tb_war = rbind(original[(startYear >= 1933) & (startYear <= 1945)],
               original[(startYear >= 2003) & (startYear <= 2015)])
tb_war[, Period := ifelse((startYear >= 1933) & (startYear <= 1945), "1933-1945", "2003-2015") %>% factor()]
tb_war = tb_war[lapply(genres, function(g) any(g == "War")) %>% unlist()]
for (g_col in all_genres) {
  tb_war[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
tb_war = tb_war[, lapply(.SD, sum), by = .(Period, language), .SDcols = all_genres]
tb_war = melt(tb_war, id.vars = c("Period", "language"), variable.name = "genre", value.name = "count")
tb_war[, total := sum(count), by = .(Period, language)]
tb_war[, share := count/total]
tb_war[, language := factor(language, levels = c("Other", "Italian", "German"))]
o = order(tb_war[Period == "1929-1945" & language == "Other", count])
tb_war = tb_war[order(Period, language, -share)]
tb_war[, genre := factor(genre, levels = tb_war[seq_along(all_genres), genre])]

# plot
ggplot(tb_war, aes(x = genre, y = share, fill = language, alpha = Period, label = Period)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Start year") +
  ylab("Share of genres which occur\ntogether with genre \"War\"") +
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_fill_manual(values = c("black", "mediumpurple3", "brown")) +
  guides(fill = "none") +
  # theme(legend.position = c(0.92, 0.75)) +
  # scale_fill_viridis(discrete = TRUE, begin = 0, end = 0.8) +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  facet_wrap(~ language, ncol = 3)
ggsave("plots/EDA/hypothesis_genres_war_language.pdf", width = 10.5, height = 3.5)