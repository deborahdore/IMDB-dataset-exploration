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

# load dataset ----
tb <- fread(paste0(path, "title.basics.tsv"))
te <- fread(paste0(path, "title.episode.tsv"))
ta <- fread(paste0(path, "title.akas.tsv"))

paste0("Col number: ", ncol(tb))
paste0("Row number: ", nrow(tb))

str(tb)

describe(tb)


# search NaN values ----
sum(tb$titleType == "\\N")
sum(tb$primaryTitle == "\\N")
sum(tb$originalTitle == "\\N")
sum(tb$isAdult == "\\N")
sum(tb$startYear == "\\N")
sum(tb$endYear == "\\N")
sum(tb$runtimeMinutes == "\\N")
sum(tb$genres == "\\N")

# replace with NA ----
tb$titleType[which(tb$titleType == "\\N")] <- NA
tb$primaryTitle[which(tb$primaryTitle == "\\N")] <- NA
tb$originalTitle[which(tb$originalTitle == "\\N")] <- NA
tb$isAdult[which(tb$isAdult == "\\N")] <- NA
tb$startYear[which(tb$startYear == "\\N")] <- NA
tb$endYear[which(tb$endYear == "\\N")] <- NA
tb$runtimeMinutes[which(tb$runtimeMinutes == "\\N")] <- NA
tb$genres[which(tb$genres == "\\N")] <- NA


# encoding tb$tconst into numbers ----
tb$tconst <- as.integer(gsub("[^0-9.-]", "", tb$tconst))
te$tconst <- as.integer(gsub("[^0-9.-]", "", te$tconst))
te$parentTconst <- as.integer(gsub("[^0-9.-]", "", te$parentTconst))

# titleType ----
sum(is.na(tb$titleType))  # no nans
tb %>% count(titleType, sort=TRUE)


# there are only two tvPilot records 
tb[which(tb$titleType == 'tvPilot')]

# searching for them...
parentSeries <- te[which(tb$titleType == 'tvPilot')]$parentTconst 
tb[which(tb$tconst %in% parentSeries)]

# they are not pilots: a pilots is the first episode of a tv series
te[which(tb$titleType == 'tvPilot')]

# modify in tvEpisode
tb[which(tb$titleType == 'tvPilot')]$titleType <- "tvEpisode"

# there are a lot of tvEpisode compare to the others -> but this is normal and biasing. let's take out the tv episodes
tb_sorted <- within(tb, titleType <- factor(titleType, levels=names(sort(table(titleType), decreasing=TRUE))))
ggplot(tb_sorted, aes(x=factor(titleType), fill=titleType)) + geom_bar()


tb_filtered = filter(tb, !(titleType %in% "tvEpisode"))
tb_fsorted <- within(tb_filtered, titleType <- factor(titleType, levels=names(sort(table(titleType), decreasing=TRUE))))
ggplot(tb_fsorted, aes(x=factor(titleType), fill=titleType)) + geom_bar()

# primaryTitle ----
sum(is.na(tb$primaryTitle)) # 3 tv episode are NAN
tb[which(is.na(tb$primaryTitle))]

# search for it and substitute primaryTitle
id_title_pt_nan <- tb[which(is.na(tb$primaryTitle))]$tconst
id_parent_tv <- te[te$tconst %in% id_title_pt_nan]$parentTconst
name_parent_tv <- tb[tb$tconst %in% id_parent_tv]$primaryTitle
tb[which(is.na(tb$primaryTitle))]$primaryTitle <- name_parent_tv

table(tb$primaryTitle)
tb %>% count(primaryTitle, sort=TRUE)
language <- c(textcat(tb[sample(nrow(tb), 10000), ]$primaryTitle))
df_lang <- data.frame(language)
options(repr.plot.width=10, repr.plot.height=6, repr.plot.dpi=250)
## a lot of norwegian titles
ggplot(df_lang, aes(y=factor(language), fill=language)) + geom_bar(show.legend = FALSE)


# originalTitle ----
sum(is.na(tb$originalTitle)) # 3
tb[which(is.na(tb$originalTitle))]

id_title_pt_nan <- tb[which(is.na(tb$originalTitle))]$tconst
id_parent <- te[te$tconst %in% id_title_pt_nan]$parentTconst
name_parent_tv <- tb[tb$tconst %in% id_parent_tv]$originalTitle
tb[which(is.na(tb$originalTitle))]$originalTitle <- name_parent_tv


or_language <- c(textcat(tb[sample(nrow(tb), 10000), ]$originalTitle))
df_lang_or <- data.frame(or_language)
ggplot(df_lang_or, aes(x=factor(or_language), fill=or_language)) + geom_bar(show.legend = FALSE)

# isAdult ----
sum(is.na(tb$isAdult))  # no nans 
table(tb$isAdult)
ggplot(tb, aes(x=factor(isAdult), fill=isAdult)) + geom_bar()
ggplot(tb, aes(x=startYear, y=factor(isAdult), fill=isAdult)) + geom_bar(stat="identity")

# startYear ----
table(tb[which(is.na(tb$startYear))]$titleType)  
tb$startYear <- as.integer(tb$startYear)
percentage_of_nan <- (sum(is.na(tb$startYear)) / nrow(tb))*100
paste0("percentage of nan values in startYear is: ", as.integer(percentage_of_nan), "%")

min(tb$startYear, na.rm = TRUE)
max(tb$startYear, na.rm = TRUE)

# dataset contains titles that start after 2022
count(tb[which(tb$startYear > 2022)])
ggplot(tb, aes(x=factor(startYear), fill=titleType)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))


summary(tb)



# endYear ----
table(tb[which(is.na(tb$endYear))]$titleType)
tb$endYear <- as.integer(tb$endYear)
percentage_of_nan_ey <- (sum(is.na(tb$endYear)) / nrow(tb))*100
paste0("percentage of nan values in endYear is: ", as.integer(percentage_of_nan_ey), "%")


# runtimeMinutes ----
table(tb[which(is.na(tb$runtimeMinutes))]$titleType)
tb$runtimeMinutes <- as.integer(tb$runtimeMinutes)

# average rumtimeMinutes for short
mean(tb[titleType == 'short', runtimeMinutes], na.rm=TRUE)

# shorts are maximum 40 minutes
tb[titleType == 'short' & runtimeMinutes>40]
tb[titleType == 'short' & runtimeMinutes>40, titleType:="movie"] 

# tv shorts are maximum 40 minutes
tb[titleType == 'tvShort' & runtimeMinutes>40]
tb[titleType == 'tvShort' & runtimeMinutes>40, titleType:="tvMovie"] 

# a mini_series cannot have more than 15 episodes
mini_series <- tb[titleType == 'tvMiniSeries', tconst]
episode_per_mini_series <- te[parentTconst %in% mini_series] %>% group_by(parentTconst) %>% summarise(freq = n())
episode_per_mini_series <- filter(episode_per_mini_series, freq > 15)
tb[tconst %in% (episode_per_mini_series$parentTconst), titleType:="tvSeries"]


# genres ----
sum(is.na(tb$genres))
tb[which(is.na(tb$genres))]
table(tb$genres)
tb$genres <- as.list(strsplit(tb$genres, ","))


# SAVE CLEANINGS ----
saveRDS(tb, paste0(path, "/title.basics_cleaned.rds"))
