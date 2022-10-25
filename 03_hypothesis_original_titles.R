library(data.table)
library(ggplot2)
library(dplyr)
library(cld3)
library(cld2)

path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

titles = fread(paste0(path, "/title.merged.csv"))
#original = fread(paste0(path, "/title_original.csv"))


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

# now we have only 15% of missing values
paste0("Percentage of NA values in language ",
       as.integer(((count(
         original[is.na(language)]
       )) / nrow(original)) * 100), "%")

nrow(original)

original = na.omit(original, cols = "averageRating")
original = na.omit(original, cols = "startYear")

original = original[startYear >= 1900 & startYear <= 2022, ]


ggplot(original, aes(x = startYear, fill = titleType)) +
  ggtitle("Distribution of original titles during the years") +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_bar()


# Did the popularity of Italian/German/Japanese movie decreased/increased after
# WW2? -------------------------------------------------------------------------
it_original = original[language == 'it' | region == 'IT',]
de_original = original[language == 'de' | region == 'DEU',]

# ITALY ------------------------------------------------------------------------
count(it_original[is.na(language),])

it_original[, region := NULL]

paste0("Original italian movies: ", nrow(it_original))

summary(it_original)

# how many italian movies per year
ggplot(it_original, aes(x = startYear, fill = titleType)) +
  ggtitle("Distribution of italian titles during the years") +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_bar()


# focus
it_original[(startYear >= 1910 & startYear <= 1960),] %>%
  ggplot(aes(x = startYear, fill = titleType)) +
  ggtitle("Distribution of italian movies during fascism") +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_bar(position = "dodge")

# GERMANY ----------------------------------------------------------------------
count(de_original[is.na(language),])

de_original[, region := NULL]
de_original = de_original[!(is.na(startYear))]


paste0("Original german movies: ", nrow(de_original))

summary(de_original)

ggplot(de_original, aes(x = startYear, fill = titleType)) +
  ggtitle("Distribution of german titles during the years") +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_bar()



de_original[startYear >= 1910 & startYear <= 1960,] %>%
  ggplot(aes(x = startYear, fill = titleType)) +
  ggtitle("Distribution of german titles during nazism") +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1933,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_bar(position = "dodge")

# POPULARITY -------------------------------------------------------------------
it_mean = it_original %>% group_by(startYear, titleType) %>%
  summarise(mean = mean(averageRating),
            .groups = 'drop')


de_mean = de_original %>% group_by(startYear, titleType) %>%
  summarise(mean = mean(averageRating),
            .groups = 'drop')


original_mean = original %>% group_by(startYear, titleType) %>%
  summarise(mean = mean(averageRating),
            .groups = 'drop')

# POPULARITY OF ITALIAN MOVIES DURING THE YEARS
ggplot() +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_line(data = it_mean %>% filter(titleType == "movie"),
            aes(x = startYear, y = mean, colour = "ITALY")) +
  geom_line(data = original_mean %>% filter(titleType == "movie"),
            aes(x = startYear, y = mean, colour = "AVERAGE")) +
  scale_color_manual(name = "nations",
                     values = c("ITALY" = "darkblue", "AVERAGE" = "red")) +
  ggtitle("Popularity of italian movies during the years")

# POPULARITY OF GERMAN MOVIES DURING THE YEARS
ggplot() +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +m
  geom_line(data = de_mean %>% filter(titleType == "movie"),
            aes(x = startYear, y = mean, colour = "GERMANY")) +
  geom_line(data = original_mean %>% filter(titleType == "movie"),
            aes(x = startYear, y = mean, colour = "AVERAGE")) +
  scale_color_manual(name = "nations",
                     values = c("GERMANY" = "darkblue", "AVERAGE" = "red")) +
  ggtitle("Popularity of german movies during the years")


# POPULARITY OF ITALIAN SHORT DURING THE YEARS
ggplot() +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_line(data = it_mean %>% filter(titleType == "short"),
            aes(x = startYear, y = mean, colour = "ITALY")) +
  geom_line(data = original_mean %>% filter(titleType == "short"),
            aes(x = startYear, y = mean, colour = "AVERAGE")) +
  scale_color_manual(name = "nations",
                     values = c("ITALY" = "darkblue", "AVERAGE" = "red")) +
  ggtitle("Popularity of italian short during the years")

# POPULARITY OF GERMAN SHORT DURING THE YEARS
ggplot() +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1933,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_line(data = de_mean %>% filter(titleType == "short"),
            aes(x = startYear, y = mean, colour = "GERMANY")) +
  geom_line(data = original_mean %>% filter(titleType == "short"),
            aes(x = startYear, y = mean, colour = "AVERAGE")) +
  scale_color_manual(name = "nations",
                     values = c("GERMANY" = "darkblue", "AVERAGE" = "red")) +
  ggtitle("Popularity of german short during the years")

# FOCUS OF ITALIAN MOVIES AND SHORT DURING FASCISM

it_mean %>% filter(titleType == "movie" | titleType == "short") %>%
  ggplot(aes(
    x = startYear,
    y = mean,
    group = titleType,
    color = titleType
  )) +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1929,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  annotate(
    "rect",
    fill = "red",
    alpha = 0.4,
    xmin = 1939,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_line() +
  ggtitle("Popularity of italian short and movies from 1900 to 1970") +
  xlim(1900, 1970)

# FOCUS OF GERMAN MOVIES AND SHORT DURING NAZISM
de_mean %>% filter(titleType == "movie" | titleType == "short") %>%
  ggplot(aes(
    x = startYear,
    y = mean,
    group = titleType,
    color = titleType
  )) +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1933,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  annotate(
    "rect",
    fill = "red",
    alpha = 0.4,
    xmin = 1939,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_line() +
  ggtitle("Popularity of german short and movies from 1900 to 1970") +
  xlim(1900, 1970)

# COMPARISON BETWEEN GERMAN AND ITALIAN MOVIES
ggplot() +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1939,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_line(data = de_mean %>% filter(titleType == "movie"),
            aes(x = startYear, y = mean, colour = "GERMANY")) +
  geom_line(data = it_mean %>% filter(titleType == "movie"),
            aes(x = startYear, y = mean, colour = "ITALY")) +
  scale_color_manual(name = "nations",
                     values = c("GERMANY" = "darkblue", "ITALY" = "red")) +
  ggtitle("Comparison of opularity of german and italian movies") +
  xlim(1920, 1960)


# COMPARISON BETWEEN GERMAN AND ITALIAN SHORT
ggplot() +
  annotate(
    "rect",
    fill = "yellow",
    alpha = 0.4,
    xmin = 1939,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_line(data = de_mean %>% filter(titleType == "short"),
            aes(x = startYear, y = mean, colour = "GERMANY")) +
  geom_line(data = it_mean %>% filter(titleType == "short"),
            aes(x = startYear, y = mean, colour = "ITALY")) +
  scale_color_manual(name = "nations",
                     values = c("GERMANY" = "darkblue", "ITALY" = "red")) +
  ggtitle("Comparison of opularity of german and italian short") +
  xlim(1920, 1960)



# SAVE -------------------------------------------------------------------------
fwrite(original, paste0(path, "/title_original.csv"))
