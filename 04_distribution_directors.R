library(data.table)
library(plyr)
library(dplyr)
library(d3Network)
library(r2d3)
library(ggplot2)
library(forcats)
library(fmsb)
library(RColorBrewer)
library(scales)
library(stringr)

path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

series = fread(paste0(path, "/merged_series_withNA.csv"))
series = series[success==TRUE]

# FORCE NETWORK ----------------------------------------------------------------

actorsList = series[, actors] %>% as.list()
writersList = series[, writers] %>% as.list()
directorsList = series[, directors] %>% as.list()

nodeActors = data.frame()
nodeWriters = data.frame()
nodeDirectors = data.frame()

lWriters = rapply(writersList, function(x) strsplit(x, split = "[|]")) %>% 
  as.integer() %>% unique()

lDirectors = rapply(directorsList, function(x) strsplit(x, split = "[|]")) %>% 
  as.integer() %>% unique()

lActors = rapply(actorsList, function(x) strsplit(x, split = "[|]")) %>% 
  as.integer() %>% unique()

nodeActors<- data.frame(matrix(unlist(lActors), nrow=length(lActors), byrow=TRUE))
nodeWriters<- data.frame(matrix(unlist(lWriters), nrow=length(lWriters), byrow=TRUE))
nodeDirectors<- data.frame(matrix(unlist(lDirectors), nrow=length(lDirectors), byrow=TRUE))

colnames(nodeActors) <- c('name')
colnames(nodeWriters) <- c('name')
colnames(nodeDirectors) <- c('name')

nodeActors$group = 1 # actors
nodeWriters$group = 2 # writers
nodeDirectors$group = 3 # directors

node = rbind(rbind(nodeActors, nodeWriters), nodeDirectors)
node <- distinct(node, name, .keep_all=TRUE)

node <-node[order(node$name),]
# should start from 0
node$name = node$name - 96
node$index = node$name
# add index
node_new <- data.frame(node[,c("name", "group")], 
                        row.names = node[, "index"])

### link -----
source = c()
target = c()

for (val in 1: length(actorsList))
{
  actorsElem = strsplit(sapply(actorsList[val],"[[",1), split = "[|]") %>% as.list()
  actorsElem = as.integer(unlist(actorsElem))
  
  directorsElem = strsplit(sapply(directorsList[val],"[[",1), split = "[|]") %>% as.list()
  directorsElem = as.integer(unlist(directorsElem))
  
  writersElem = strsplit(sapply(writersList[val],"[[",1), split = "[|]") %>% as.list()
  writersElem = as.integer(unlist(writersElem))
  
  l2 = append(append(actorsElem, directorsElem), writersElem)
  
  for (y in l2){
    for(k in l2){
      if(k!=y){
        source <- append(source,y)
        target <- append(target, k)
      }
    }
  }
}

link = data.table(source, target)
# set value to 1
link[, value:=1]
# sum all values to obtain closeness
link_new = link[, value := sum(value), by = .(source, target)]
# unique
link_new = distinct(link_new, .keep_all=TRUE)
# keep only link which value > 1 and transform to datafframe
link_new = as.data.frame(link_new[value>1])
# order by source
link_new <-link_new[order(link_new$source),]
# change index to keep similarity with node 
link_new$source = link_new$source - 96
link_new$target = link_new$target - 96
# add index
link_new <- data.frame(link_new[,c("source", "target", "value")], 
                        row.names = c(0:(length(link_new$target)-1)))

fwrite(link_new, paste0(path, "/link.csv"))
fwrite(node_new, paste0(path, "/node.csv"))

sink("./test.html")
d3ForceNetwork(Links = link_new, Nodes = node_new, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4, zoom = TRUE)
sink()


# DIRECTORS EXPLORATION --------------------------------------------------------

series = series[, averageRating:=as.integer(averageRating)]
series = series[, numVotes:=as.integer(numVotes)]
series = series[, nTranslations:=as.integer(nTranslations)]

series = series[, weighted_mean:=weighted.mean(c(averageRating,numVotes, nTranslations)),
                by = .(averageRating, numVotes, nTranslations)]

series = series[order(-weighted_mean), ]

top_100 = head(series, 100)

head(top_100, 40) %>%
  mutate(primaryTitle = fct_reorder(primaryTitle, weighted_mean)) %>%
  ggplot( aes(x=primaryTitle, y=weighted_mean),) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("top 40 series") + 
  ylab("linear combination of num translations, num votes, averate rating") +
  theme_bw()

ggsave("./plots/04_distribution_directors_top_40.pdf", width = 9, height = 5)


top_100 %>%
  ggplot(aes(x=numVotes, y=averageRating, size=nTranslations, color=types)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Translations")

ggsave("./plots/04_distribution_directors_top_100.pdf", width = 9, height = 5)


# radar 

colors_border <- brewer.pal(5, "BuPu")
colors_in <- alpha(colors_border,0.3)


top_5 = head(top_100, 5)
top_5 = top_5[,list(primaryTitle,numVotes, averageRating, nTranslations, runtimeMinutes, nSeasons)]

min_top_5 = list("MINIMO",
  min(top_5$numVotes),
                 min(top_5$averageRating),
                 min(top_5$nTranslations),
                 min(top_5$runtimeMinutes),
                 min(top_5$nSeasons))
max_top_5 = list("MASSIMO", max(top_5$numVotes),
                 max(top_5$averageRating),
                 max(top_5$nTranslations),
                 max(top_5$runtimeMinutes),
                 max(top_5$nSeasons))

top_5 = rbindlist(list(max_top_5 , min_top_5 , top_5))
top_5[,list(numVotes, averageRating, nTranslations, runtimeMinutes, nSeasons)] %>%
  radarchart(axistype=0,
              pcol=colors_border  , plwd=2.5, plty=1.5,
              cglcol="grey", cglty=1, axislabcol="grey",caxislabels = seq(0, 2, 0.2), 
             cglwd=0.8,
              vlcex=0.8
  ) + legend(x=1, y=1.2, legend = top_5[-c(1,2),primaryTitle],
       bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.8, pt.cex=3)

# success and failures of directors -------

directors = top_100[, directors] %>% as.list()

# all the directors in the top 100 series
directors = rapply(directors, function(x) strsplit(x, split = "[|]")) %>% 
  as.integer() %>% unique() %>% as.data.table()

colnames(directors) = "director"

directors[, success:=0]
directors[, unsuccess:=0]

series = fread(paste0(path, "/merged_series_withNA.csv"))
successful_directors = series[success==TRUE, directors] %>% as.list()
unsuccessful_directors = series[success==FALSE, directors] %>% as.list()


for (d in directors[,director]){
  s = str_count(successful_directors, as.character(d)) %>% sum()
  f = str_count(unsuccessful_directors, as.character(d)) %>% sum()
  directors[director==d, success:=s]
  directors[director==d, unsuccess:=f]
}

directors = directors[order(-success)] %>% head(50)

directors_new <- melt(directors, id.vars = "director", 
                      variable.name = "category",
                      value.name="total")

directors_new %>% 
  ggplot(aes(x=factor(director), y=total, fill=category)) +
  geom_bar(position="stack",  stat="identity") +
  theme(axis.text.x = element_text(angle=90)) +
  xlab("directors") + 
  ylab("total number of series directed")

ggsave("./plots/04_distribution_directors_directors.pdf", width = 10, height = 3.5)
  