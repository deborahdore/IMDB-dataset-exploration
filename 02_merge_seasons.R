
library(data.table)
library(dplyr)

# merge title.episode with title.basics
te = fread("../IMDB-dataset-exploration-data/title.episode_cleaned.csv")
tb = fread("../IMDB-dataset-exploration-data/title.basics_cleaned.csv")
tb[, `:=`(primaryTitle = NULL, originalTitle = NULL, titleType = NULL, endYear = NULL, isAdult = NULL)]
gc()
seasons = merge(te, tb, by = "tconst", all.x = TRUE)

# filter for episodes between 2010 and 2022
seasons = seasons[(startYear >= 2010) & (startYear <= 2022)]

# merge with title.crew
tc = fread("../IMDB-dataset-exploration-data/title.crew_clean.csv")
tc[, tconst := as.integer(gsub("^tt", "", tconst))]  # sorry, this step should be in the cleaning process
tc[, `:=`(nDirectors = NULL, nWriters = NULL)]
seasons = merge(seasons, tc, by = "tconst", all.x = TRUE)
gc()

# merge with title.ratings
tr = fread("../IMDB-dataset-exploration-data/title.ratings_clean.csv")
tr_series = tr[tconst %in% seasons[, parentTconst]]
seasons = merge(seasons, tr, by = "tconst", all.x = TRUE)
seasons = merge(seasons, tr_series, by.x = "parentTconst", by.y = "tconst", all.x = TRUE, suffixes = c("", "_series"))
gc()

# merge with title.principles
# and extract actors from category
tp = fread("../IMDB-dataset-exploration-data/title.principals_clean.csv")
tp[, playsSelf := NULL]
tp = tp[category == "actor"]
tp = tp[(tconst %in% seasons[, tconst]) | (tconst %in% seasons[, parentTconst])]
tp = tp[category == "actor", .(tconst, actors = nconst)]
gc()
tp = dcast(tp, tconst ~ ., fun.aggregate = list, value.var = "actors")
tp = tp[, `:=`(actors = ., . = NULL)]
gc()

seasons = merge(seasons, tp, by = "tconst", all.x = TRUE)
tp_series = tp[!(tconst %in% seasons[, tconst])]
rm(tp)
gc()

# transform concatenated columns to lists
seasons[, directors := strsplit(directors, "\\|")]
seasons[, writers := strsplit(writers, "\\|")]
seasons[, genres := strsplit(genres, "\\|")]
gc()

rm(tb)
rm(tc)
rm(te)
rm(tr)
gc()

# COLUMNS:
# tconst season
# tconst series
# nEpisodes
# runTime
# genres
# directors
# writers
# actors
# numVotes
# averageRating
# minRating
# maxRating


# prepare aggregation
seasons[, tconst := NULL]
seasons[, startYear := min(startYear), by = .(parentTconst, seasonNumber)]
seasons[, averageRuntimeMinutes := mean(runtimeMinutes, na.rm = TRUE), by = .(parentTconst, seasonNumber)]
seasons[, `:=`(averageRating = mean(averageRating, na.rm = TRUE),
               minRating = min(averageRating, na.rm = TRUE),
               maxRating = max(averageRating, na.rm = TRUE)), by = .(parentTconst, seasonNumber)]
seasons[minRating == Inf, minRating := NA]
seasons[maxRating == -Inf, maxRating := NA]
seasons[is.na(averageRating), averageRating := averageRating_series]
seasons[, ID := paste(parentTconst, seasonNumber, sep = "_")]

# aggregate the complicated columns
seasons_ls = seasons[, .(ID, episodeNumber, genres, directors, writers, actors)]
seasons_ls = dcast(seasons_ls, ID ~ ., fun.aggregate = list,
      value.var = c("episodeNumber", "genres", "directors", "writers", "actors"))
seasons_ls[, genres := lapply(genres, unlist)]
seasons_ls[, directors := lapply(directors, unlist)]
seasons_ls[, writers := lapply(writers, unlist)]
seasons_ls[, actors := lapply(actors, unlist)]
seasons_ls[, genres := lapply(genres, na.omit)]
seasons_ls[, directors := lapply(directors, na.omit)]
seasons_ls[, writers := lapply(writers, na.omit)]
seasons_ls[, actors := lapply(actors, na.omit)]
seasons_ls[genres == "NULL", genres := ""]
seasons_ls[directors == "NULL", directors := ""]
seasons_ls[writers == "NULL", writers := ""]
seasons_ls[actors == "NULL", actors := ""]
# seasons_ls[unlist(lapply(genres, length)) == 0, genres := NA]
# seasons_ls[unlist(lapply(directors, length)) == 0, directors := NA]
# seasons_ls[unlist(lapply(writers, length)) == 0, writers := NA]
# seasons_ls[unlist(lapply(actors, length)) == 0, actors := NA]
seasons_ls[, genres := lapply(genres, unique)]
seasons_ls[, directors := lapply(directors, unique)]
seasons_ls[, writers := lapply(writers, unique)]
seasons_ls[, actors := lapply(actors, unique)]
seasons_ls[, `:=`(episodes = episodeNumber, episodeNumber = NULL)]
seasons_ls[, nEpisodes := lapply(episodes, length) %>% unlist()]
seasons_ls[, episodes := lapply(episodes, sort)]
gc()

# aggregate the easy columns
seasons_agg = seasons[, .(ID, parentTconst, seasonNumber, startYear, averageRuntimeMinutes)]
seasons_agg = unique(seasons_agg)
seasons_agg = merge(seasons_agg, seasons_ls, by = "ID")
rm(seasons_ls)
gc()

# impute NA ratings and votes with average of the series
seasons = seasons[, .(ID, averageRating, averageRating_series, numVotes, numVotes_series, minRating, maxRating)]
seasons = merge(seasons, seasons_agg[, .(ID, nEpisodes)])
seasons[is.na(averageRating), averageRating := averageRating_series]
seasons[is.na(numVotes) & !is.na(numVotes_series) & !is.na(nEpisodes), numVotes := (numVotes_series / nEpisodes)]
seasons[, numVotes := sum(numVotes, na.rm = TRUE), by = ID]
seasons = unique(seasons)
seasons = seasons[, .(ID, numVotes, averageRating, minRating, maxRating)]
gc()

# complete aggregation
seasons = merge(seasons_agg, seasons, by = "ID")
seasons = seasons[, .(ID, parentTconst, seasonNumber, nEpisodes, averageRuntimeMinutes, genres, directors,
                      writers, actors, numVotes, averageRating, minRating, maxRating, episodes)]
rm(seasons_agg)
gc()



# SAVE SEASONS DATASET --------------------------------------------------------
# fwrite(seasons, paste0(path, "/seasons.csv"))
fwrite(seasons, "../IMDB-dataset-exploration-data/seasons.csv")
