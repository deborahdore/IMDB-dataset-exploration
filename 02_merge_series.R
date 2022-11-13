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
crew = fread(paste0(path, "/title.crew_clean.csv"))
principals = fread(paste0(path, "/title.principals_clean.csv"))


titles = titles[titleType %in% c('tvSeries','tvMiniSeries'),]

dataset = merge(titles, ratings, by=c("tconst"), all.x=TRUE)

colnames(dataset)[2] <- "types"

dataset[,originalTitle:=NULL]
dataset[,endYear:=NULL]
dataset = dataset[numVotes > 50, ]
dataset = dataset[startYear > 2010 & startYear < 2022, ]
dataset = dataset[averageRating > 3, ]

dataset = na.omit(dataset)

dataset_1 = merge(dataset, crew, all.x=TRUE)

principals = principals[category %in% c("actor"), ]

dataset_2 = merge(dataset_1, principals, all.x=TRUE)
colnames(dataset_2)[14] <- "id_actor"


translations = akas %>% group_by(titleId) %>% 
                    summarise(total_count=n(),.groups = 'drop')

colnames(translations)[1] <- "tconst"
colnames(translations)[2] <- "nTranslations"

ds = merge(dataset_2, translations, all.x = TRUE)

episodes[, episodeNumber:=NULL]

seasons = episodes %>% group_by(parentTconst) %>% 
              summarize(nSeasons = max(seasonNumber), .groups = 'drop')

colnames(seasons)[1] <- "tconst"

df = merge(ds, seasons, all.x = TRUE)

fwrite(df, paste0(path, "/merged_series.csv"))
