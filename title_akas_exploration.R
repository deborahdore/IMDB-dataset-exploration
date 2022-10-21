library(data.table)
library(ggplot2)
library(tidyverse)
library(psych)
library(dplyr)
library(textcat)
library(rpart)
library(mice)

path <- "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

# load dataset ----
ta <- fread(paste0(path, "title.akas.tsv"))

str(ta)

# replace with NA ----
ta[ordering == "\\N", ordering:=NA]
ta[title == "\\N", title:=NA]
ta[region == "\\N", region:=NA]
ta[language == "\\N", language:=NA]
ta[types == "\\N", types:=NA]
ta[attributes == "\\N", attributes:=NA]
ta[isOriginalTitle == "\\N", isOriginalTitle:=NA]

# titleId ----
ta$titleId <- as.integer(gsub("[^0-9.-]", "", ta$titleId))
sum(is.na(ta$titleId)) # no nans
head(ta$titleId)

# ordering ----
sum(is.na(ta$ordering)) # no nas 

# title ----
# remove nan titles
sum(is.na(ta$title)) # 2 nans
ta <- ta[!is.na(title)] 

# region ----
sum(is.na(ta$region)) # a 1867411
paste0("Percentage of nans in region column is: ", as.integer((sum(is.na(ta$region))/nrow(ta))*100), "%")

table(ta$region)

# language ----
sum(is.na(ta$language)) #6290157
paste0("Percentage of nans in language column is: ", as.integer((sum(is.na(ta$language))/nrow(ta))*100), "%")
table(ta$language)
ta[is.na(language), language:=textcat(title)]

# types ----
sum(is.na(ta$types)) #28113709
paste0("Percentage of nans in types column is: ", as.integer((sum(is.na(ta$types))/nrow(ta))*100), "%")
ta$types <- as.list(ta$types, sep="\\")

# attributes ----
# drop columns -> too high number of nans
sum(is.na(ta$attributes))
paste0("Percentage of nans in attributes column is: ", as.integer((sum(is.na(ta$attributes))/nrow(ta))*100), "%")
ta[,attributes:=NULL]

# isOriginalTitle ----
ta$isOriginalTitle <- as.integer(ta$isOriginalTitle)
# redundant
sum(is.na(ta$isOriginalTitle))
ta[,isOriginalTitle:=NULL]


saveRDS(ta, paste0(path, "/title.akas_cleaned.rds"))
