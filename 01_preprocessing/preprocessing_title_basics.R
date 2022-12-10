#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("textcat")

library(data.table)
library(ggplot2)
library(dplyr)
library(textcat)

setwd("..")
path <- paste0(getwd(), "/dataset")


tb = fread(paste0(path, "/title.basics.tsv"))
te = fread(paste0(path, "/title.episode.tsv"))

paste0("Col number: ", ncol(tb))
paste0("Row number: ", nrow(tb))

str(tb)

summary(tb)

# search NaN values ------------------------------------------
nrow(tb[titleType == "\\N", ])
nrow(tb[primaryTitle == "\\N",])
nrow(tb[originalTitle == "\\N",])
nrow(tb[isAdult == "\\N",])
nrow(tb[startYear == "\\N",])
nrow(tb[endYear == "\\N",])
nrow(tb[runtimeMinutes == "\\N",])
nrow(tb[genres == "\\N",])

# replace with NA ------------------------------------------
tb[titleType=="\\N", titleType:=NA]
tb[primaryTitle=="\\N", primaryTitle:=NA]
tb[originalTitle=="\\N", originalTitle:=NA]
tb[isAdult=="\\N", isAdult:=NA]
tb[startYear=="\\N", startYear:=NA]
tb[endYear=="\\N", endYear:=NA]
tb[genres=="\\N", genres:=NA]
tb[runtimeMinutes=="\\N", runtimeMinutes:=NA]

# encoding tb$tconst into numbers ---------------------------
tb[,tconst:=as.integer(gsub("[^0-9.-]", "", tconst))]
te[,tconst:=as.integer(gsub("[^0-9.-]", "", tconst))]
te[,parentTconst:=as.integer(gsub("[^0-9.-]", "", parentTconst))]

fn_percentage <- function(nan_rows, tot_rows){
  return ((nan_rows/tot_rows)*100)
}

# titleType ------------------------------------------------
sum(is.na(tb[,titleType]))  # no nans
tb %>% count(titleType, sort=TRUE)


# there are only two tvPilot records 
tb[titleType=='tvPilot']
parentSeries = te[which(tb$titleType == 'tvPilot')]$parentTconst 
tb[tb$tconst %in% parentSeries]

# they are not pilots: a pilots is the first episode of a tv series
te[which(tb$titleType == 'tvPilot')]

# modify in tvEpisode
tb[which(tb$titleType == 'tvPilot')]$titleType = "tvEpisode"


tb_filtered = filter(tb, !(titleType %in% "tvEpisode")) %>% group_by(titleType) %>% summarize(count=n())
ggplot(tb_filtered, aes(x=reorder(titleType, -count), y=count, 
                        fill=reorder(titleType, -count))) + geom_bar(stat = "identity")

# primaryTitle --------------------------------------------
count(tb[is.na(primaryTitle)]) # 3 tv episode are NAN
tb[is.na(primaryTitle)]

# search for it and substitute primaryTitle
id_title_pt_nan = tb[is.na(primaryTitle), tconst]
id_parent_tv = te[tconst %in% id_title_pt_nan, parentTconst]
name_parent_tv = tb[tconst %in% id_parent_tv, primaryTitle]
tb[is.na(primaryTitle), primaryTitle:=name_parent_tv]

language = c(textcat(tb[sample(nrow(tb), 10000), primaryTitle]))
df_lang = data.frame(language)
options(repr.plot.width=10, repr.plot.height=6, repr.plot.dpi=250)
ggplot(df_lang, aes(y=factor(language), fill=language)) + geom_bar(show.legend = FALSE)


# originalTitle -------------------------------------------
count(tb[is.na(originalTitle)]) # 3
tb[is.na(originalTitle)]

id_title_pt_nan = tb[is.na(originalTitle), tconst]
id_parent = te[tconst %in% id_title_pt_nan, parentTconst]
name_parent_tv = tb[tconst %in% id_parent_tv, originalTitle]
tb[is.na(originalTitle), originalTitle:= name_parent_tv]


or_language = c(textcat(tb[sample(nrow(tb), 10000), ]$originalTitle))
df_lang_or = data.frame(or_language)
ggplot(df_lang_or, aes(x=factor(or_language), fill=or_language)) + geom_bar(show.legend = FALSE)


# isAdult ------------------------------------------------
count(tb[is.na(isAdult)]) # no nans 
table(tb$isAdult)
ggplot(tb, aes(x=factor(isAdult), fill=isAdult)) + geom_bar()
ggplot(tb, aes(x=startYear, y=factor(isAdult), fill=isAdult)) + geom_bar(stat="identity")

# startYear ----------------------------------------------
count(tb[is.na(startYear)]) # 1237209
tb[,startYear:= as.integer(startYear)]
paste0("percentage of nan values in startYear is: ", fn_percentage(count(tb[is.na(startYear)]), nrow(tb)), "%")

min(tb$startYear, na.rm = TRUE)
max(tb$startYear, na.rm = TRUE)

# dataset contains titles that start after 2022
count(tb[startYear > 2022])
ggplot(tb, aes(x=factor(startYear), fill=titleType)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))


# endYear -----------------------------------------------
count(tb[is.na(endYear)]) #9171150
tb[,endYear:= as.integer(endYear)]
paste0("percentage of nan values in endYear is: ", fn_percentage(count(tb[is.na(endYear)]), nrow(tb)), "%")


# runtimeMinutes -----------------------------------------
count(tb[is.na(runtimeMinutes)]) # 6787250
tb[,runtimeMinutes:= as.integer(runtimeMinutes)]
paste0("percentage of nan values in runtimeMinutes is: ", fn_percentage(count(tb[is.na(runtimeMinutes)]), nrow(tb)), "%")

# average rumtimeMinutes for short
mean(tb[titleType == 'short', runtimeMinutes], na.rm=TRUE)

# shorts are maximum 40 minutes
tb[titleType == 'short' & runtimeMinutes>40]
tb[titleType == 'short' & runtimeMinutes>40, titleType:="movie"] 

# tv shorts are maximum 40 minutes
tb[titleType == 'tvShort' & runtimeMinutes>40]
tb[titleType == 'tvShort' & runtimeMinutes>40, titleType:="tvMovie"] 

# a mini_series cannot have more than 15 episodes
mini_series = tb[titleType == 'tvMiniSeries', tconst]
episode_per_mini_series = te[parentTconst %in% mini_series] %>% group_by(parentTconst) %>% summarise(freq = n())
episode_per_mini_series = filter(episode_per_mini_series, freq > 15)
tb[tconst %in% (episode_per_mini_series$parentTconst), titleType:="tvSeries"]


# genres ------------------------------------------------
count(tb[is.na(genres)]) #427915
paste0("percentage of nan values in genres is: ", fn_percentage(count(tb[is.na(genres)]), nrow(tb)), "%")
table(tb[, genres])
tb[, genres:= strsplit(tb[,genres], ",")]

# SAVE CLEANINGS -----------------------------------------
fwrite(tb, paste0(path, "/title.basics.clean.csv"))
