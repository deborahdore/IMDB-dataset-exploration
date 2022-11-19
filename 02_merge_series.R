library(data.table)
library(dplyr)

# path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"
path = "../IMDB-dataset-exploration-data/"

titles = fread(paste0(path, "/title.basics_cleaned.csv"))
titles = titles[titleType %in% c('tvSeries','tvMiniSeries'),]

ratings = fread(paste0(path, "/title.ratings_clean.csv"))
dataset = merge(titles, ratings, by=c("tconst"), all.x=TRUE)
colnames(dataset)[2] <- "types"

dataset[,originalTitle:=NULL]
dataset[,endYear:=NULL]
# dataset = dataset[numVotes > 50, ]
dataset = dataset[startYear >= 2010 & startYear <= 2022, ]
# dataset = dataset[averageRating > 7, ]
gc()

crew = fread(paste0(path, "/title.crew_clean.csv"))
crew[, tconst := as.integer(gsub("^tt", "", tconst))]
dataset = merge(dataset, crew, all.x=TRUE)

rm(titles)
rm(ratings)
rm(crew)
gc()

principals = fread(paste0(path, "/title.principals_clean.csv"))
principals = principals[category == "actor", ]
principals[, category:=NULL]
principals[, playsSelf:=NULL]
actors = principals %>% group_by(tconst)  %>% summarise(actors = list(nconst)) %>% setDT()
actors = actors[!sapply(actors, is.null)]
dataset = merge(dataset, actors, all.x=TRUE)
dataset[actors == "NULL", actors := NA]
rm(principals)
rm(actors)
gc()

akas = fread(paste0(path, "/title.akas_cleaned.csv"))
# translations = akas %>% group_by(titleId) %>% 
#   summarise(total_count=n(),.groups = 'drop')
translations = akas[, .N, by = titleId]
colnames(translations)[1] <- "tconst"
colnames(translations)[2] <- "nTranslations"
dataset = merge(dataset, translations, all.x = TRUE)
rm(akas)
rm(translations)
gc()

episodes = fread(paste0(path, "/title.episode_cleaned.csv"))
episodes[, episodeNumber:=NULL]
episodes[, tconst := NULL]
episodes= unique(episodes)
# seasons = episodes %>% group_by(parentTconst) %>% 
#   summarize(nSeasons = max(seasonNumber), .groups = 'drop')
seasons = episodes[, `:=`(maxSeasonNumber = max(seasonNumber), nSeasons = .N), by = parentTconst]
episodes[, seasonNumber := NULL]
episodes = unique(episodes)
colnames(seasons)[1] <- "tconst"

dataset = merge(dataset, seasons, all.x = TRUE)
dataset[, count := sequence(.N), by = tconst]
dataset = dataset[count == 1]
dataset[, count := NULL]
rm(episodes)
rm(seasons)
gc()

dataset[!is.na(averageRating), .N]  # 53107
dataset[!is.na(averageRating) & !is.na(numVotes), .N]  # 53107
dataset[!is.na(averageRating) & !is.na(numVotes) & (numVotes >= 50), .N]  # 21971
dataset[!is.na(averageRating) & (numVotes >= 50) & !is.na(nTranslations), .N] # 21907
dataset[!is.na(averageRating) & (numVotes >= 50) & !is.na(nTranslations) & !is.na(runtimeMinutes), .N] # 13766

dataset = dataset[!is.na(averageRating) & (numVotes >= 50) & !is.na(nTranslations) & !is.na(runtimeMinutes)]

# filter all series that are in the top of ratings, numVotes and nTranslations
top = 200
qrating = quantile(dataset[, averageRating], 1 - top/nrow(dataset))
qvotes = quantile(dataset[, numVotes], 1 - top/nrow(dataset))
qtrans = quantile(dataset[, nTranslations], 1 - top/nrow(dataset))
dataset[, success := ((averageRating >= qrating) + (numVotes >= qvotes) & (nTranslations >= qtrans))]
dataset[(success), .N]
View(dataset)



fwrite(dataset, paste0(path, "merged_series_withNA.csv"))



##### CODE NOT USED

# titles = na.omit(titles)
# akas = na.omit(akas)
# ratings = na.omit(ratings)
# episodes = na.omit(episodes)
# crew = na.omit(crew)
# principals = na.omit(principals)
# 
# df = na.omit(df)
# df = df[!is.null(df)]
# df = distinct(df)
# 
# df[actors == "NULL", actors:=NA]
# fwrite(df, paste0(path, "/merged_series.csv"))
