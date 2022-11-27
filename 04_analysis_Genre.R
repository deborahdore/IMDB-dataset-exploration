
library(data.table)
library(magrittr)
library(tidyr)
library(ggplot2); theme_set(theme_bw())
options(scipen = 1000000)
library(dplyr)
library(readr)
library(tidyr)
library(viridis)
# library(editrules)
# library(splitstackshape)

# path = "/Users/marianabarbosa/Desktop/DV_Project/IMDB/"
path = "../IMDB-dataset-exploration-data/"
tb = fread(paste0(path, "/merged_series_withNA.csv"))


# PREPARE GENRES 
tb[, genres := strsplit(genres, "\\|")]
#choose genres to analyze
all_genres = c("Action", "Adventure", "Animation" ,"Comedy", 
               "Crime", "Drama", "Fantasy", "Mistery", "Romance", 
               "Sci-Fi", "Thriller")
all_genres = c("Action", "Crime", "Mystery", "Drama", "Sci-Fi", "Comedy")
for (g_col in all_genres) {tb[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]}
tb = melt(tb, id.vars = c("tconst", "success", "runtimeMinutes", "averageRating", "numVotes", "nTranslations"),
          measure.vars = all_genres,
          variable.name = "genres", value.name = "count")
tb = tb[(count == TRUE)]
tb[, count := NULL]


ggpubr::ggarrange(
# rating
ggplot(tb, aes(x = genres, y = averageRating, fill = genres, alpha = success)) +
  geom_violin(scale = "width", position = position_dodge(0.5)) +
  scale_alpha_manual(values = c(0.2, 0.8), labels = c("no", "yes")) +
  coord_cartesian(ylim = c(5, 10)) +
  ggtitle("Average Rating") + xlab("") + ylab("") +
  guides(fill = "none") +
  guides(alpha = guide_legend(override.aes = list(fill = "grey50"))) +
  theme(axis.text.x=element_text(angle=40,hjust=1)),
# votes
ggplot(tb, aes(x = genres, y = numVotes, fill = genres, alpha = success)) +
  geom_violin(scale = "width", position = position_dodge(0.5)) +
  scale_alpha_manual(values = c(0.2, 0.8)) +
  scale_y_continuous(trans = "log10") +
  ggtitle("Number of Votes") + xlab("") + ylab("") +
  theme(axis.text.x=element_text(angle=40,hjust=1)),
# translations
ggplot(tb, aes(x = genres, y = nTranslations, fill = genres, alpha = success)) +
  geom_violin(scale = "width", position = position_dodge(0.5)) +
  scale_alpha_manual(values = c(0.2, 0.8)) +
  coord_cartesian(ylim = c(0, 80)) +
  ggtitle("Number of Translations") + xlab("") + ylab("") +
  theme(axis.text.x=element_text(angle=40,hjust=1)),
# run time
ggplot(tb[runtimeMinutes <= 200], aes(x = genres, y = runtimeMinutes, fill = genres, alpha = success)) +
  geom_violin(scale = "width", position = position_dodge(0.5)) +
  scale_alpha_manual(values = c(0.2, 0.8)) +
  scale_y_continuous(breaks = c(20, 40, 60, 80)) +
  coord_cartesian(ylim = c(0, 100)) +
  ggtitle("Runtime in Minutes") + xlab("") + ylab("") +
  theme(axis.text.x=element_text(angle=40,hjust=1)),
ncol = 4, common.legend = TRUE, legend = "right", widths = c(1, 1.2, 1, 1))

ggsave("plots/04_analysis_GenresViolin.pdf", height = 2.5, width = 10)



# MORE GENRES
tb = fread(paste0(path, "/merged_series_withNA.csv"))
tb[, genres := strsplit(genres, "\\|")]
all_genres = c("Action", "Adventure", "Crime", "Mystery", "Drama", "Animation", "Sci-Fi", "Thriller", "Fantasy", "Comedy", "Romance")
for (g_col in all_genres) {tb[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]}
tb = melt(tb, id.vars = c("tconst", "success", "runtimeMinutes", "averageRating", "numVotes", "nTranslations"),
          measure.vars = all_genres,
          variable.name = "genres", value.name = "count")
tb = tb[(count == TRUE)]
tb[, count := NULL]
tb[, total := .N, by = genres]
tb[, total_success := .N, by = .(genres, success)]
tb[, share := total_success/total]
tb = unique(tb[, .(success, genres, total, total_success, share)])

ggpubr::ggarrange(
  ggplot(tb[(success)], aes(x = genres, y = share, fill = genres,
                            label = paste0(" ", round(share, 3) * 100, "%"))) +
    geom_bar(aes(color = genres), stat = "identity", alpha = 0.7) +
    geom_text(nudge_y = 0.0007) +
    xlab("") + ylab("") +
    scale_y_continuous(breaks = c(0.005, 0.01, 0.015), labels = c("0.5%", "1%", "1.5%")) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Share of successfull series in each genre") +
    theme(axis.text.x=element_text(angle=40,hjust=1),
          legend.position = "none")
  ,
ggplot(tb[(success)], aes(x = genres, y = total_success, fill = genres,
                          label = total_success)) +
  geom_bar(aes(color = genres), stat = "identity", alpha = 0.7) +
  geom_text(nudge_y = 3) +
  xlab("") + ylab("") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Number of successfull series in each genre") +
  theme(axis.text.x=element_text(angle=40,hjust=1),
        legend.position = "none")
, ncol = 2)
ggsave("plots/04_analysis_genresDistribution.pdf", height = 3.5, width = 12)



#genres ordered for the histogram to be ordered 
all_genres = c("Drama", "Action", "Crime" ,"Adventure", 
               "Fantasy", "Animation", "Romance", "Thriller", "Sci-Fi")


#runtimeMinuytes
ggplot(tb[runtimeMinutes<120], aes(x = runtimeMinutes, y =..density..)) + geom_histogram(colour = "black", fill = "blue") + 
  geom_density()



