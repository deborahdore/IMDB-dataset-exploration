library(data.table)
library(dplyr)

path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

titles = fread(paste0(path, "/title.basics_cleaned.csv"))
akas = fread(paste0(path, "/title.akas_cleaned.csv"))
ratings = fread(paste0(path, "/title.ratings_clean.csv"))
episodes = fread(paste0(path, "/title.episode_cleaned.csv"))
crew = fread(paste0(path, "/title.crew_clean.csv"))
principals = fread(paste0(path, "/title.principals_clean.csv"))

titles = na.omit(titles)
akas = na.omit(akas)
ratings = na.omit(ratings)
episodes = na.omit(episodes)
crew = na.omit(crew)
principals = na.omit(principals)


titles = titles[titleType %in% c('tvSeries','tvMiniSeries'),]

dataset = merge(titles, ratings, by=c("tconst"), all.x=TRUE)

colnames(dataset)[2] <- "types"

dataset[,originalTitle:=NULL]
dataset[,endYear:=NULL]
dataset = dataset[numVotes > 50, ]
dataset = dataset[startYear > 2010 & startYear < 2022, ]
# dataset = dataset[averageRating > 7, ]

dataset_1 = merge(dataset, crew, all.x=TRUE)

principals = principals[category %in% c("actor"), ]
principals[, category:=NULL]
principals[, playsSelf:=NULL]

actors = principals %>% group_by(tconst)  %>% summarise(actors = list(nconst)) %>% setDT()
actors = actors[!sapply(actors, is.null)]
dataset_2 = merge(dataset_1, actors, all.x=TRUE)

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

df = na.omit(df)
df = df[!is.null(df)]
df = distinct(df)

df[actors == "NULL", actors:=NA]
fwrite(df, paste0(path, "/merged_series.csv"))
