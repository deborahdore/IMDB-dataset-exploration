
library(data.table)
library(dplyr)
library(igraph)
library(networkD3)

path = "../IMDB-dataset-exploration-data/"

series = fread(paste0(path, "/merged_series_withNA.csv"))


##### ACTORS of successsfull series
series = series[(success)]
series = series[actors != ""]
series[, actors := strsplit(actors, "\\|")]
series[, actors := lapply(actors, as.integer)]
series = series[lapply(actors, function(a) length(a) > 1) %>% unlist()]
actors_dist = lapply(series[, actors],
                     function(a) {
                       combn(a, 2) %>% t() %>% as.data.frame()
                     }) %>% do.call(what = rbind) %>% cbind(., "value" = 1) %>% as.data.table()

# create long format for distance matrix
actors_dist = unique(actors_dist)
actors_dist = rbindlist(list(actors_dist, actors_dist[, .(V1 = V2, V2 = V1, value)]))
all_actors = unique(actors_dist[, V1])
all_actors = sort(all_actors)
all_actorsDT = data.table("V1" = rep(all_actors, length(all_actors)),
                          "V2" = rep(all_actors, each = length(all_actors)),
                          "value" = 0L) #bottleneck
actors_dist = rbindlist(list(actors_dist, all_actorsDT))
actors_dist[, value := max(value), by = .(V1, V2)] #bottleneck
actors_dist = unique(actors_dist)

# create long format for distance matrix
actors_dist = lapply(series[, actors],
                     function(a) {
                       combn(a, 2) %>% t() %>% as.data.frame()
                     }) %>% do.call(what = rbind) %>% cbind(., "value" = 1) %>% as.data.table()

actors_dist[V1 > V2, `:=`(V1 = V2, V2 = V1)]
actors_dist[, value := as.integer(value)]
actors_dist[, value := sum(value), by = .(V1, V2)]
actors_dist = unique(actors_dist)

# interactive plot
p <- simpleNetwork(actors_dist, height="100px", width="100px", opacity = 1, zoom = TRUE)
p
library(htmlwidgets)
saveWidget(p, file="plots/networkInteractive.html")
