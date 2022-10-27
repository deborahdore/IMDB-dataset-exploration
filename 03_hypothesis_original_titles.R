library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(cld3)
library(cld2)
library(viridis)

path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"
plot_path = "/Users/deborah/Documents/IMDB-dataset-exploration/plots/original_titles_hypothesis/"

titles = fread(paste0(path, "/title.merged.csv"))

original = titles[types == "original" & (
  titleType == "movie" |
    titleType == "tvMovie" |
    titleType == "tvSeries" |
    titleType == "short"
), c(
  "tconst",
  "originalTitle",
  "region",
  "language",
  "titleType",
  "isAdult",
  "startYear",
  "runtimeMinutes",
  "genres",
  "averageRating",
  "numVotes"
)]

rm(titles)
gc()

str(original)

original = original[!duplicated(original[, tconst]), ]
original[region == "", region := NA]
original[language == "", language := NA]
colnames(original)[2] = "title"

original[is.na(language), language := cld3::detect_language(title)]

paste0("Percentage of NA values in language ",
       as.integer(((count(
         original[is.na(language)]
       )) / nrow(original)) * 100), "%")
paste0("Percentage of NA values in region ",
       as.integer(((count(
         original[is.na(region)]
       )) / nrow(original)) * 100), "%")

# the cld3 package is not able to guess all the languages, let's use also cld2
original[is.na(language), language := cld2::detect_language(title)]

# now we have only 16% of missing values
paste0("Percentage of NA values in language ",
       as.integer(((count(
         original[is.na(language)]
       )) / nrow(original)) * 100), "%")

nrow(original)

original = na.omit(original, cols = "averageRating")
original = na.omit(original, cols = "startYear")

original = original[startYear >= 1900 & startYear <= 2022,]


ggplot(original, aes(x = startYear, fill = titleType)) +
  annotate(
    "rect",
    fill = "grey20",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  scale_fill_viridis(discrete = TRUE, end = 0.8) +
  facet_wrap( ~ titleType) +
  geom_bar()

ggsave(
  paste0(plot_path, "distribution_titles_per_titleType.pdf"),
  width = 10,
  height = 4
)


# Did the popularity of Italian/German/Japanese movie decreased/increased after
# WW2? -------------------------------------------------------------------------
it_original = original[language == 'it' | region == 'IT', ]
de_original = original[language == 'de' | region == 'DEU', ]
others_original = original[!(language %in% c("it", "de"))]
others_original[, region := NULL]
de_original[, region := NULL]
it_original[, region := NULL]

# ITALY ------------------------------------------------------------------------
count(it_original[is.na(language), ])
it_original = it_original[!(is.na(startYear))]

# 13394
paste0("Original italian movies: ", nrow(it_original))

summary(it_original)

# GERMANY ----------------------------------------------------------------------
count(de_original[is.na(language), ])
de_original = de_original[!(is.na(startYear))]

# 20728
paste0("Original german movies: ", nrow(de_original))

summary(de_original)

#-------------------------------------------------------------------------------
df_original = rbindlist(list(it_original[, language := "italian"],
                             de_original[, language := "german"],
                             others_original[, language := "others"]))


# distribution titles
df_original[(startYear >= 1920 & startYear <= 1960), ] %>%
  ggplot(aes(x = startYear, fill = titleType)) +
  annotate(
    "rect",
    fill = "grey20",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  facet_wrap(~ language, scales = "free") +
  scale_fill_viridis(discrete = TRUE, end = 0.8) +
  geom_histogram(binwidth = 1, alpha = 0.8)

ggsave(
  paste0(plot_path, "distribution_titles_per_language.pdf"),
  width = 14,
  height = 4
)


# POPULARITY -------------------------------------------------------------------

it_mean = it_original %>% group_by(startYear, titleType) %>%
  summarise(mean = weighted.mean(averageRating, numVotes),
            .groups = 'drop')

it_mean$language = "italian"


de_mean = de_original %>% group_by(startYear, titleType) %>%
  summarise(mean = weighted.mean(averageRating, numVotes),
            .groups = 'drop')

de_mean$language = "german"

other_mean = others_original %>% group_by(startYear, titleType) %>%
  summarise(mean = weighted.mean(averageRating, numVotes),
            .groups = 'drop')

other_mean$language = "others"

# POPUlARITY OF TITLES DURING THE YEARS

df = rbindlist(list(it_mean,de_mean,other_mean))

df = df[titleType %in% c("movie", "short"),]

# ggplot(df, aes(x = startYear, y = mean, color = titleType)) +
#   annotate(
#     "rect",
#     fill = "grey20",
#     alpha = 0.4,
#     xmin = 1929,
#     xmax = 1945,
#     ymin = -Inf,
#     ymax = Inf
#   ) +
#   facet_wrap(~ language) +
#   geom_line()
#
# ggsave(
#   paste0(
#     plot_path,
#     "popularity_of_movies_and_shorts_per_titleType.pdf"
#   ),
#   width = 10,
#   height = 4
# )

# POPULARITY MOVIES/SHORT DURING 1920 - 1960
ggplot(df,
       aes(
         x = startYear,
         y = mean,
         color = language,
         size = language,
         alpha = language
       )) +
  annotate(
    "rect",
    fill = "grey20",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  facet_wrap( ~ titleType) +
  scale_color_viridis(discrete = TRUE, end = 0.8) +
  scale_color_manual(values = c("brown", "mediumpurple3", "black")) +
  scale_size_manual(values = c(0.5, 0.5, 1)) +
  scale_alpha_manual(values = c(1, 1, 0.8)) +
  xlim(1920, 2010) +
  geom_line()

ggsave(
  paste0(
    plot_path,
    "popularity_of_movies_and_shorts_per_titleType.pdf"
  ),
  width = 10,
  height = 4
)