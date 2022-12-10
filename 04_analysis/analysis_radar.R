
library(data.table)
library(plyr)
library(dplyr)
library(d3Network)
library(r2d3)
library(ggplot2); theme_set(theme_bw())
library(forcats)
library(fmsb)
library(RColorBrewer)
library(scales)
library(stringr)

setwd("..")
path <- paste0(getwd(), "/dataset")

series = fread(paste0(path, "/merged_series_withNA.csv"))
series = series[success==TRUE]

colors_border <- brewer.pal(6, "BuPu")[2:6]
colors_in <- alpha(colors_border,0.9)

series[, log_numVotes := log10(numVotes)]
top_5 = series[,list(primaryTitle, log_numVotes, averageRating, nTranslations, runtimeMinutes, nSeasons)]

min_top_5 = list("MINIMO",
                 min(top_5$log_numVotes),
                 min(top_5$averageRating),
                 min(top_5$nTranslations),
                 min(top_5$runtimeMinutes),
                 min(top_5$nSeasons))
max_top_5 = list("MASSIMO", max(top_5$log_numVotes),
                 max(top_5$averageRating),
                 max(top_5$nTranslations),
                 100, #max(top_5$runtimeMinutes),
                 max(top_5$nSeasons))

top_5 = top_5[primaryTitle %in% c("Game of Thrones", "Sherlock", "Rick and Morty", "Stranger Things", "True Detective")]

top_5 = rbindlist(list(max_top_5 , min_top_5 , top_5))
colnames(top_5) = c("primaryTitle", "log(Number of Votes)", "Average Rating", "Number of Translations", "Runtime", "Number of Seasons")
pdf("plots/analysis/analysis_radar.pdf", height = 5, width = 8)
top_5[,list(`log(Number of Votes)`, `Average Rating`, `Number of Translations`, `Runtime`, `Number of Seasons`)] %>%
  radarchart(axistype=0,
             pcol=colors_border, plwd=2.5, plty=1.5,
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels = seq(0, 2, 0.2), 
             cglwd=0.8,
             vlcex=0.8
  )
legend(x=1.2, y=1.3, legend = top_5[3:7, primaryTitle],
       bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.8, pt.cex=3)
dev.off()
