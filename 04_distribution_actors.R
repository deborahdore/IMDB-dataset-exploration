library(data.table)
library(plyr)
library(dplyr)
library(d3Network)
library(r2d3)

path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

series = fread(paste0(path, "/merged_series_withNA.csv"))
series = series[success==TRUE]

actorsList = series[, actors] %>% as.list()
writersList = series[, writers] %>% as.list()
directorsList = series[, directors] %>% as.list()

#### node
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

# unique
node <- distinct(node, name, .keep_all=TRUE)

# order
node <-node[order(node$name),]
# should start from 0
node$name = node$name - 96

# add index
node_new <- data.frame(node[,c("name", "group")], 
                        row.names = c(1:length(node$name)))

node_new$weight = 1

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
d3ForceNetwork(link = link_new, node = node_new, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4, zoom = TRUE)
sink()

