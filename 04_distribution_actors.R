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

links = data.table()

for (x in actorsList) {
  elem = strsplit(as.integer(x), split = "[|]")
  print(elem)
}




# Plot
d3ForceNetwork(Links = data.frame(), Nodes = nodes,
               Source = "source", Target = "target",
               NodeID = "actors", Group = "group", opacity = 0.8)
