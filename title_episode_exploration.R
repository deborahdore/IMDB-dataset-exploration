library(data.table)
library(ggplot2)
library(tidyverse)
library(psych)
library(dplyr)
library(textcat)
library(rpart)
library(mice)
library(tm)

path <- "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

# load datasets ----
te = fread(paste0(path, "title.episode.tsv"))

str(te)

# search NaN values ----
sum(te$tconst == "\\N")
sum(te$parentTconst == "\\N")
sum(te$seasonNumber == "\\N")
sum(te$episodeNumber == "\\N")

# tconst ----
te[tconst == "\\N", tconst:=NA]
sum(is.na(te$tconst)) # no nans
te$tconst <- as.numeric(gsub("[^0-9.-]", "", te$tconst))

# parentTconst ----
te[parentTconst == "\\N", parentTconst:=NA]
sum(is.na(te$parentTconst)) # no nans
te$parentTconst <- as.numeric(gsub("[^0-9.-]", "", te$parentTconst))

# seasonNumber ----
te[seasonNumber == "\\N", seasonNumber:=NA]
sum(is.na(te$seasonNumber))  # 1468784
te[is.na(seasonNumber)]
paste0("Percentage of nans in seasonNumber column is: ", as.numeric((sum(is.na(te$seasonNumber))/nrow(te))*100), "%")
te = subset(te[!(is.na(seasonNumber)), ])
te$seasonNumber <- as.numeric(te$seasonNumber)
max = max(te[, seasonNumber])
max
# a series with 2022 seasons seems impossibile -- max 200 seasons
te <- subset(te[te$seasonNumber < 200, ])


# episodeNumber ----
te[episodeNumber == "\\N", episodeNumber:=NA]
sum(is.na(te$episodeNumber)) # no nans
te$episodeNumber <- as.numeric(te$episodeNumber)
max(te[, episodeNumber])
ggplot(te, aes(x=factor(episodeNumber))) + geom_bar()

saveRDS(te, paste0(path, "/title.episode_cleaned.rds"))
