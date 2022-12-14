library(data.table)
library(plyr)
library(dplyr)
library(d3Network)
library(r2d3)
library(ggplot2);
library(forcats)
library(fmsb)
library(RColorBrewer)
library(scales)

library(stringr)

setwd("..")
path <- paste0(getwd(), "/dataset")

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

node = rbind(nodeActors, nodeWriters, nodeDirectors)
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

minmaxscale = function(v) (v - min(v)) / (max(v) - min(v))
weighted_mean_vec = rowMeans(matrix(c(minmaxscale(series[, averageRating]),
                       minmaxscale(log10(series[, numVotes])),
                       minmaxscale(log10(series[, nTranslations]))), ncol = 3))
series[, weighted_mean := weighted_mean_vec]

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

ggsave("./plots/analysis/distribution_directors_top_40.pdf", width = 9, height = 5)


top_100 %>%
  ggplot(aes(x=numVotes, y=averageRating, size=nTranslations, color=types)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Translations")

ggsave("./plots/analysis/distribution_directors_top_100.pdf", width = 9, height = 5)


# success and failures of directors -------

directors = top_100[, directors] %>% as.list()

# all the directors in the top 100 series
directors = rapply(directors, function(x) strsplit(x, split = "[|]")) %>% 
  as.integer() %>% unique() %>% as.data.table()

colnames(directors) = "director"

directors[, success:=0]
directors[, unsuccess:=0]

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

ggsave("./plots/analysis/distribution_directors_directors.pdf", width = 10, height = 3.5)




##### CODE WITHOUT STRINGR
series = fread(paste0(path, "/merged_series_withNA.csv"))

gc()
nb = fread(paste0(path, "/name.basics.clean.csv"))

series = series[directors != ""]
series[, directors := strsplit(directors, "\\|")]
directorsDT = series[(success), directors] %>% unlist() %>% unique() %>% list() %>% setDT()
colnames(directorsDT) = "nconst"
directorsDT[, `:=`(successful = 0, unsuccessful = 0)]
for (d in directorsDT[, nconst]) {
  s = series[(success), lapply(directors, function(dfull) d %in% dfull) %>% unlist()] %>% sum()
  f = series[!(success), lapply(directors, function(dfull) d %in% dfull) %>% unlist()] %>% sum()
  directorsDT[nconst == d, `:=`(successful = s, unsuccessful = f)]
}
directorsDT[, nconst := as.integer(nconst)]
directorsDT = merge(directorsDT, nb[, .(nconst, primaryName)], by = "nconst", all.x = TRUE)
directorsDT = directorsDT[successful >= 4]
directorsDT[, total := successful + unsuccessful]
directorsDT[, share := successful / total]
directorsDT = melt(directorsDT, id.vars = c("primaryName", "total", "share"),
     measure.vars = c("unsuccessful", "successful"),
     variable.name = "success", value.name = "n")

ggplot(directorsDT, aes(x = reorder(primaryName, -n))) +
  geom_bar(aes(alpha = success, y = n), stat = "identity", position = "stack", fill = "darkorange3") +
  geom_text(data = directorsDT[(success == "successful")],
            aes(label = paste0(round(share, 2)*100, "%"), y = n), nudge_y = -1.5, color = "white", fontface = "bold") +
  geom_text(data = directorsDT[(success == "successful")],
            aes(label = round(total, 3), y = total), nudge_y = -1.5, fontface = "bold") +
  scale_alpha_manual(values = c(0.3, 1), labels = c("no", "yes")) +
  xlab("Directors of more than 3 successful series") +
  ylab("Total number of series") +
  theme(axis.text.x=element_text(angle=40,hjust=1))
ggsave("plots/analysis/analysis_BestDirectors.pdf", height = 4, width = 10)
