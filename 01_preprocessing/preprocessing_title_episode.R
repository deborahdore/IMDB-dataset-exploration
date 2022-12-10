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

# LOAD DATASET -----------------------------------------------------------------
te = fread(paste0(path, "/title.episode.tsv"))

# FUNCTIONS --------------------------------------------------------------------

fn_percentage <- function(nan_rows, tot_rows){
  return ((nan_rows/tot_rows)*100)
}

# CLEANING ---------------------------------------------------------------------
te[tconst == "\\N", tconst:=NA]
te[parentTconst == "\\N", parentTconst:=NA]
te[seasonNumber == "\\N", seasonNumber:=NA]
te[episodeNumber == "\\N", episodeNumber:=NA]

te[, tconst:=as.numeric(gsub("[^0-9.-]", "", tconst))]
te[, parentTconst:=as.numeric(gsub("[^0-9.-]", "", parentTconst))]
te[, seasonNumber:=as.numeric(seasonNumber)]
te[, episodeNumber:=as.numeric(episodeNumber)]

te = subset(te[!(is.na(seasonNumber)), ])



# EXPLORATORY DATA ANALYSIS ----------------------------------------------------
str(te)

#every time a season number is null, the episode number is null
count(te[is.na(tconst)])
count(te[is.na(parentTconst)])
count(te[is.na(seasonNumber)]) #1468784
count(te[is.na(episodeNumber)]) #1468784

paste0("percentage of nan values in startYear is: ", 
       fn_percentage(count(te[is.na(seasonNumber)]), nrow(te)), "%")
paste0("percentage of nan values in startYear is: ", 
       fn_percentage(count(te[is.na(episodeNumber)]), nrow(te)), "%")


max(te[, seasonNumber], na.rm=TRUE) 
# a series with 2022 seasons seems impossible
# turns out thay they mistaken the season by the year
nrow(te) == length(unique(te[, tconst]))


subs = te[seasonNumber >= 1000, ]  # year instead of season number
ordered = subs[order(seasonNumber)] %>% group_by(parentTconst)
ordered$seasonNumber = as.numeric(factor(ordered$seasonNumber))
ordered
total = merge(te[seasonNumber >= 1000, ], 
               ordered, by=c("tconst", "parentTconst", "episodeNumber"))
total[, seasonNumber.x:=NULL]
setnames(total, "seasonNumber.y", "seasonNumber")

te = te[seasonNumber<1000]
te_concat <- rbindlist(list(te, total), use.names=TRUE)    # Rbind data.tables
te_concat

max(te_concat[, episodeNumber])
ggplot(te, aes(x=factor(episodeNumber))) + geom_bar()


# SAVING -----------------------------------------------------------------------
fwrite(te_concat, paste0(path, "/title.episode.clean.csv"))
