
library(data.table)
library(dplyr)
library(ggplot2)
library(igraph)
library(viridis)
library(RColorBrewer)


setwd("..")
path <- paste0(getwd(), "/dataset")

series = fread(paste0(path, "/merged_series_withNA.csv"))

##### ACTORS of successfull series
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
actors_dist[V1 > V2, `:=`(V1 = V2, V2 = V1)]
actors_dist[, value := as.integer(value)]
actors_dist[, value := sum(value), by = .(V1, V2)]
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

unique(actors_dist[, V1]) %>% length()
unique(actors_dist[, V2]) %>% length()

# create distance matrix
d = dcast(actors_dist, V1 ~ V2, value.var = "value") #bottleneck
d = d[order(V1)]
rownames(d) = d[, V1]
s = sort(as.integer(colnames(d)[-1])) %>% as.character
d = select(d, all_of(s))
rownames(d) = colnames(d)
d = as.matrix(d)

# genres for coloring
series[, genres := strsplit(genres, "\\|")]
color_genres = lapply(all_actors, function(a) {
  gen = series[lapply(actors, function(act) a %in% act) %>% unlist(), genres] %>% unlist() %>%
    table() %>% which.max() %>% names()
}) %>% unlist()
color_genres = as.factor(color_genres)
pal = categorical_pal(8)
pal = factor(color_genres, labels = pal)

# network graph
network <- graph_from_adjacency_matrix(d, add.colnames = NA, weighted=NULL, mode="undirected", diag=F)
pdf("plots/analysis/network_actors.pdf", height = 20, width = 20)
set.seed(345)
plot.igraph(network,
            vertex.label.font = 2, vertex.label.cex = 1.5, vertex.label.color = "black", vertex.label.family = "URWBookman",
            vertex.label=NA,
            label.cex = 0.1, label.dist = 1, label.degree = -pi/2,
            vertex.size=2.5, vertex.frame.color = "white",
            vertex.color = pal
)
legend('topleft',legend = levels(color_genres), col = categorical_pal(8),
       pch = 19, pt.cex = 4, cex = 2)
dev.off()


