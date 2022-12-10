library(data.table)
library(magrittr)
library(tidyr)
library(ggplot2); theme_set(theme_bw())
library(viridis)
library(scales)
library(cowplot)

options(scipen = 1000000)
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

# 1792049 rows
original[, `:=`(tconst = NULL, title = NULL, isAdult = NULL, runtimeMinutes = NULL, language = NULL, region = NULL)]
original = original[(!is.na(startYear)) & (startYear <= 2022) & (startYear >= 1897)]  # delete 1245621 rows (13.4%)
original = original[genres != ""]  # delete 427915 rows (4.6%)
original = original[titleType %in% c("movie", "short", "tvSeries", "tvMovie")]  # delete 7381423 rows (79.6% of which 75.4% tvEpisode)
gc()


# development and distribution of titleTypes
dt_titles = original
dt_titles = dt_titles[, .N, by = .(startYear, titleType)]
dt_titles[, share := N/sum(N), by = startYear]
dt_titles[, `:=`(Type = titleType, titleType = NULL)]
dt_titles = merge(data.table("startYear" = rep(1897:2022, each = 4), "Type" = rep(c("movie", "short", "tvMovie", "tvSeries"), length(1897:2022))),
      dt_titles, by = c("startYear", "Type"), all.x = TRUE)
dt_titles[is.na(N), N := 0]
dt_titles[is.na(share), share := 0]

t_abs = ggplot(dt_titles, aes(x = startYear, y = N, fill = Type)) +
  geom_area(position = "stack") +
  scale_x_continuous(breaks = seq(1900, 2022, by = 25)) +
  xlab("Start year") +
  ylab("Number of titles") +
  scale_fill_viridis(discrete = TRUE, end = 0.8) +
  theme(legend.position = "none")
  
t_rel = ggplot(dt_titles, aes(x = startYear, y = share, fill = Type)) +
  geom_area(position = "stack") +
  scale_x_continuous(breaks = seq(1900, 2022, by = 25)) +
  xlab("Start year") +
  ylab("Distribution of titles") +
  scale_fill_viridis(discrete = TRUE, end = 0.8) +
  theme(legend.position = "none")

# distribution averageRating
rating = ggplot(original[!is.na(averageRating)], aes(x = averageRating, y = ..density..)) +
  geom_histogram(bins = 30, fill = "royalblue4", color = "black") +
  geom_density() +
  scale_x_continuous(breaks = 1:10) +
  xlab("Average rating of original titles") +
  ylab("Density")

# distribution numVotes
votes = ggplot(original[!is.na(numVotes)], aes(x = numVotes, y = ..density..)) +
  geom_histogram(bins = 30, fill = "royalblue4", color = "black") +
  geom_density() +
  scale_x_continuous(trans = "log10", breaks = c(0, 10, 100, 1000, 10000, 100000)) +
  xlab("Number of votes of original titles (log scale)") +
  ylab("Density")

# Connection averageRating and numVotes
rat_vot = ggplot(original[!is.na(averageRating) & !is.na(numVotes) & (numVotes > 0)],
       aes(x = numVotes, y = averageRating)) +
  geom_hex(bins = 15) +
  xlab("Number of votes of original titles\n(log scale)") +
  ylab("Average rating of original titles") +
  scale_fill_viridis(begin = 0.9, end = 0, trans = log10_trans(), option = "A") +
  scale_x_continuous(trans = "log10", breaks = c(0, 10, 100, 1000, 10000, 100000)) +
  theme(legend.position = "bottom")

plot_grid(
plot_grid(t_abs, t_rel,
          get_legend(t_abs + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom")),
          ncol = 1, rel_heights = c(1, 1, .2)),
plot_grid(rating, votes, ncol = 1),
plot_grid(ggplot() + theme(panel.border = element_blank()),
          rat_vot,
          ggplot() + theme(panel.border = element_blank()),
          rel_heights = c(0.1, 1, 0.1), ncol = 1),
ncol = 3)
ggsave("plots/EDA/EDA.pdf", width = 10, height = 3.5)
