library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(cld3)
library(cld2)
library(viridis)


path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

titles = fread(paste0(path, "/title.basics_cleaned.csv"))
akas = fread(paste0(path, "/title.akas_cleaned.csv"))
ratings = fread(paste0(path, "/title.ratings_clean.csv"))
episodes = fread(paste0(path, "/title.episode_cleaned.csv"))


titles = titles[titleType %in% c('tvSeries','tvMiniSeries', 'tvEpisode'), ]

m1 = merge(titles, episodes, by=c("tconst"), all.x=TRUE)
m2 = merge(m1, ratings, by=c("tconst"), all.x=TRUE)

rm(m1)
gc()

colnames(m2)[2] <- "types"

m2[,originalTitle:=NULL]
m2[,endYear:=NULL]


translations = m2 %>% group_by(tconst) %>% tally() %>% ungroup()

m2 = merge(m2, translations, all.x=TRUE)
colnames(m2)[13] <- "translations"

m2 = m2[numVotes > 50, ]
m2 = m2[startYear > 2010 & startYear < 2022, ]
m2 = m2[averageRating > 3, ]

# todo: number of season
