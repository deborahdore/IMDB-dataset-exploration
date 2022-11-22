library(data.table)
library(dplyr)
library(plyr)
library(d3Network)

path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

principals = fread(paste0(path, "/title.principals_clean.csv"))
crew = fread(paste0(path, "/title.crew_clean.csv"))

series = fread(paste0(path, "/merged_series.csv"))

actorsList = series[, actors] %>% as.list()
str(actorsList)

nodes = data.table()
nodes$actors = rapply(actorsList, function(x) strsplit(x, split = "[|]")) %>% 
  as.integer() %>% unique() %>% as.data.frame()
nodes$group = 1

source = c()
target = c()

for (x in actorsList) {
  elem = strsplit(x, split = "[|]") %>% as.list()
  elem = as.integer(unlist(elem))
  if(length(elem) > 1){
    for (y in elem){
      for(k in elem){
        source <- append(source,y)
        target <- append(target, k)
      }
    }
  }
}

links = data.frame(source, target)
links$value = 1
links %>% group_by(source, target) %>% summarize(x = sum(value))

d3ForceNetwork(Links = links, Nodes = nodes,
               Source = "source", Target = "target",
               NodeID = "actors", value=value, opacity = 0.8)
