library(data.table)
library(magrittr)
library(ggplot2); theme_set(theme_bw())
library(corrplot)
library(ggcorrplot)
library(dplyr)

#path = "/Users/marianabarbosa/Desktop/DV_Project/IMDB/"
path = "./dataset"

series = fread(paste0(path, "/merged_series_withNA.csv"))
head(series,30)
summary(series)


#allseries

series %>% dplyr::select(averageRating, numVotes, nTranslations, runtimeMinutes, startYear)
series = series %>% dplyr::mutate("log(numVotes)"=log10(numVotes))

cor(series %>% dplyr::select(averageRating, `log(numVotes)`, nTranslations, runtimeMinutes, startYear))

all_series = cor(series %>% dplyr::select(averageRating, `log(numVotes)`, 
                                          nTranslations, runtimeMinutes, startYear), method="pearson")

pdf(file="./plots/corr_series.pdf", width = 4, height =4)
corrplot(all_series, method="color", tl.col = "black", type = 'lower', addCoef.col = "black", order = 'original')
dev.off()


#Crime

series[, Crime := lapply(genres, function(x) {"Crime" %in% x})]

crime = series[Crime == TRUE] %>% dplyr::select(averageRating, `log(numVotes)`, nTranslations, 
                                                     runtimeMinutes, startYear) %>% cor()
pdf(file="./plots/corr_series_crime.pdf", width = 4, height =4)
corrplot(crime, method="color", tl.col = "black", type = 'lower', addCoef.col = "black", order = 'original')
dev.off()

#Drama

series[, Drama := lapply(genres, function(x) {"Drama" %in% x})]

drama = series[Drama == TRUE] %>% dplyr::select(averageRating,`log(numVotes)`,  nTranslations, 
                                                     runtimeMinutes, startYear) %>% cor()

pdf(file="./plots/corr_series_drama.pdf", width = 4, height =4)
corrplot(drama, method="color", tl.col = "black", type = 'lower', addCoef.col = "black", order = 'original')
dev.off()


#Action

series[, Action := lapply(genres, function(x) {"Action" %in% x})]

action = series[Action == TRUE] %>% dplyr::select(averageRating, `log(numVotes)`, nTranslations, 
                                                     runtimeMinutes, startYear) %>% cor()

pdf(file="./plots/corr_series_action.pdf", width = 4, height =4)
corrplot(action, method="color", tl.col = "black", type = 'lower', addCoef.col = "black", order = 'original')
dev.off()

#Mistery

series[, Mystery := lapply(genres, function(x) {"Mystery" %in% x})]

mistery = series[Mystery == TRUE] %>% dplyr::select(averageRating, `log(numVotes)`, nTranslations, 
                                                     runtimeMinutes, startYear) %>% cor()

pdf(file="./plots/corr_series_mistery.pdf", width = 4, height =4)
corrplot(mistery, method="color", tl.col = "black", type = 'lower', addCoef.col = "black", order = 'original')
dev.off()

#Comedy

series[, Comedy := lapply(genres, function(x) {"Comedy" %in% x})]

comedy = series[Comedy == TRUE] %>% dplyr::select(averageRating,`log(numVotes)`,  nTranslations, 
                                                     runtimeMinutes, startYear) %>% cor()

pdf(file="./plots/corr_series_comedy.pdf", width = 4, height =4)
corrplot(comedy, method="color", tl.col = "black", type = 'lower', addCoef.col = "black", order = 'original')
dev.off()

#Sci-fi

series[, `Sci-Fi` := lapply(genres, function(x) {"Sci-Fi" %in% x})]

scify = series[`Sci-Fi` == TRUE] %>% dplyr::select(averageRating, `log(numVotes)`, nTranslations, 
                                                     runtimeMinutes, startYear) %>% cor()

pdf(file="./plots/corr_series_scify.pdf", width = 4, height =4)
corrplot(scify, method="color", tl.col = "black", type = 'lower', addCoef.col = "black", order = 'original')
dev.off()