
library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(viridis)
library(cowplot)

setwd("..")
path <- paste0(getwd(), "/dataset")


original = fread(paste0(path, "/title.original.csv"))

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

original[, `:=`(tconst = NULL, title = NULL, isAdult = NULL, runtimeMinutes = NULL)]
original = original[(!is.na(startYear)) & (startYear <= 2022) & (startYear >= 1897)]  # delete 1245621 rows (13.4%)
original = original[genres != ""]  # delete 427915 rows (4.6%)
original = original[titleType %in% c("movie", "short", "tvSeries", "tvMovie")]  # delete 7381423 rows (79.6% of which 75.4% tvEpisode)
original[, language := ifelse((language == "it" | region == "IT"), "Italian",
                              ifelse(language == "de" | region == "DEU", "German", "Other"))]
original[, genres := strsplit(genres, "\\|")]
original[, `:=`(Type = titleType, titleType = NULL)]
gc()

original_text = original[(startYear >= 1920 & startYear <= 1960), .N, by = .(startYear, language)]
original_text = original_text[, max(N), by = language]
original_text = rbindlist(list(original_text, original_text, original_text))


### PLOT LANGUAGE -------------------------------------------------------------
original[(startYear >= 1920 & startYear <= 1960), ] %>%
  ggplot(aes(x = startYear, fill = Type)) +
  annotate("rect", fill = "grey20", alpha = 0.4, xmin = 1933, xmax = 1945, ymin = -Inf, ymax = Inf) +
  facet_wrap(~ language, scales = "free") +
  scale_fill_viridis(discrete = TRUE, end = 0.8) +
  geom_histogram(binwidth = 1, alpha = 0.8) +
  geom_text(data = original_text, mapping = aes(x = 1939, y = V1 * 0.9, label = "1933 - 1945", fill = NULL), size = 2.5) +
  xlab("Start year") +
  ylab("Number of titles") +
  theme(axis.title = element_text(size=12,face="bold"))
ggsave("plots/EDA/title_language.pdf", width = 8, height = 2)


### PLOT POPULARITY AND LANGUAGE ----------------------------------------------
original_pop = original[!is.na(averageRating) & !is.na(numVotes) & Type %in% c("movie", "short")]  # delete 72.5% of rows

original_mean = original_pop
original_mean[, mean := weighted.mean(averageRating, numVotes), by = .(startYear, language, Type)]
original_mean[, `:=`(region = NULL, genres = NULL, averageRating = NULL, numVotes = NULL)]
original_mean = unique(original_mean)

breaks_fun = function(x) {
  c(0, ifelse(max(x) > 1000, 7500, 200))
}

rating = ggplot(original_mean, aes(x = startYear, y = mean,
                          color = language, size = language, alpha = language)) +
  annotate("rect", fill = "grey20", alpha = 0.4, xmin = 1933, xmax = 1945, ymin = -Inf, ymax = Inf) +
  facet_wrap( ~ Type) +
  scale_color_manual(values = c("brown", "mediumpurple3", "black")) +
  scale_size_manual(values = c(0.7, 0.7, 1)) +
  scale_alpha_manual(values = c(0.8, 0.8, 1)) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  coord_cartesian(xlim = c(1897, 2022)) +
  ylab("Average rating per year\nweighted by number of votes") +
  geom_line() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 8), "points"),
        axis.title = element_text(size=12,face="bold"))

count = ggplot(original_pop, aes(x = startYear, fill = language)) +
  geom_histogram(bins = length(1897:2022)) +
  scale_fill_manual(values = c("brown", "mediumpurple3", "black")) +
  facet_grid(language ~ Type, scale = "free") +
  scale_y_continuous(breaks = breaks_fun) +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  coord_cartesian(xlim = c(1897, 2022)) +
  xlab("Start year") +
  ylab("Number of titles") +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title = element_text(size=12,face="bold"))

plot_grid(rating,
          count,
          get_legend(count + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")),
          ncol = 1, rel_heights = c(1, 0.6, 0.1))

ggsave("plots/EDA/popularity_language.pdf", width = 8, height = 5)
