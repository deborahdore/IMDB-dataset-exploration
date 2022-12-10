library(data.table)
library(dplyr)
library(ggplot2)
library(igraph)
library(viridis)
library(RColorBrewer)


setwd("..")
path <- paste0(getwd(), "/dataset")


series = fread(paste0(path, "/merged_series_withNA.csv"))


##### DIRECTORS of successfull series
series = series[(success)]
series = series[directors != ""]
series[, directors := strsplit(directors, "\\|")]
series[, directors := lapply(directors, as.integer)]
series = series[lapply(directors, function(a) length(a) > 1) %>% unlist()]
directors_dist = lapply(series[, directors],
                        function(a) {
                          combn(a, 2) %>% t() %>% as.data.frame()
                        }) %>% do.call(what = rbind) %>% cbind(., "value" = 1) %>% as.data.table()

# create long format for distance matrix
directors_dist[V1 > V2, `:=`(V1 = V2, V2 = V1)]
directors_dist[, value := as.integer(value)]
directors_dist[, value := sum(value), by = .(V1, V2)]
directors_dist = unique(directors_dist)
directors_dist = rbindlist(list(directors_dist, directors_dist[, .(V1 = V2, V2 = V1, value)]))
all_directors = unique(directors_dist[, V1])
all_directors = sort(all_directors)
all_directorsDT = data.table("V1" = rep(all_directors, length(all_directors)),
                             "V2" = rep(all_directors, each = length(all_directors)),
                             "value" = 0L) #bottleneck
directors_dist = rbindlist(list(directors_dist, all_directorsDT))
directors_dist[, value := max(value), by = .(V1, V2)] #bottleneck
directors_dist = unique(directors_dist)

unique(directors_dist[, V1]) %>% length()
unique(directors_dist[, V2]) %>% length()

# create distance matrix
d = dcast(directors_dist, V1 ~ V2, value.var = "value") #bottleneck
d = d[order(V1)]
rownames(d) = d[, V1]
s = sort(as.integer(colnames(d)[-1])) %>% as.character
d = select(d, all_of(s))
rownames(d) = colnames(d)
d = as.matrix(d)

# genres for coloring
series[, genres := strsplit(genres, "\\|")]
color_genres = lapply(all_directors, function(a) {
  gen = series[lapply(directors, function(dir) a %in% dir) %>% unlist(), genres] %>% unlist()
  gen = gen[gen != "Fantasy"]
  gen %>% table() %>% which.max() %>% names()
}) %>% unlist()
color_genres = as.factor(color_genres)
pal = categorical_pal(8)
pal = factor(color_genres, labels = pal)

# network graph
network <- graph_from_adjacency_matrix(d, add.colnames = NA, weighted=NULL, mode="undirected", diag=F)
pdf("plots/analysis/network_directors.pdf", height = 20, width = 20)
set.seed(345)
plot.igraph(network,
            layout=layout.kamada.kawai,
            vertex.label.font = 2, vertex.label.cex = 1.5, vertex.label.color = "black", vertex.label.family = "URWBookman",
            vertex.label=NA,
            edge.color = adjustcolor("darkgray", alpha.f = 0.2),
            edge.width = 0.1,
            vertex.size=3, vertex.frame.color = "white",
            vertex.color = pal
)
legend('topleft',legend = levels(color_genres), col = categorical_pal(8),
       pch = 19, pt.cex = 4, cex = 2)
dev.off()
