library(data.table)
library(plyr)
library(dplyr)
library(d3Network)


path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

series = fread(paste0(path, "/merged_series_withNA.csv"))
series = series[success==TRUE]

actorsList = series[, actors] %>% as.list()
str(actorsList)

nodes = data.frame()
l = rapply(actorsList, function(x) strsplit(x, split = "[|]")) %>% 
  as.integer() %>% unique()
nodes<- data.frame(matrix(unlist(l), nrow=length(l), byrow=TRUE))
colnames(nodes) <- c('name')
nodes$group = 1
nodes = na.omit(nodes)

source = c()
target = c()

for (x in actorsList) {
  elem = strsplit(x, split = "[|]") %>% as.list()
  elem = as.integer(unlist(elem))
  if(length(elem) > 1){
    for (y in elem){
      for(k in elem){
        if(k!=y){
          source <- append(source,y)
          target <- append(target, k)
        }
      }
    }
  }
}

links = data.table(source, target)
links[, value:=1]
links_new = links[, value := sum(value), by = .(source, target)]
links_new = unique(links_new)



sink("test.html")
d3ForceNetwork(Links = as.data.frame(links_new), 
               Nodes = nodes, 
               Source = "source",
               Target = "target", NodeID = "name",
               Value = "value",
               Group = "group", opacity = 0.4)
