library(data.table)
library(ggplot2)
library(dplyr)
library(cld2)

path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

titles = fread(paste0(path, "/title.merged.csv"))

original = titles[types=="original", c("tconst", "originalTitle", "region", "language",
                                       "titleType", "isAdult","startYear", "runtimeMinutes",
                                       "genres", "averageRating", "numVotes")]

rm(titles)
gc()

str(original)

original[region=="", region:=NA]
original[language=="", language:=NA]
colnames(original)[2] = "title"

original[is.na(language), language:=detect_language(title)]

as.integer(((count(original[is.na(language)])) / nrow(original))*100)
as.integer(((count(original[is.na(region)])) / nrow(original))*100)

nrow(original)


# Did the popularity of Italian movie decreased/increased after WW2? -----------
it_movies = original[language=='it' | region=='IT', ]
count(it_movies[is.na(language), ])
count(it_movies[is.na(region), ])

it_movies[,region:=NULL]

it_movies = it_movies[!(is.na(startYear))]

paste0("Original italian movies: ", nrow(it_movies))


ggplot(it_movies, aes(x=startYear, y=averageRating, fill=titleType)) + 
  geom_histogram(stat = "identity")

fwrite(original, paste0(path, "/original"))

