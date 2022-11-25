
library(data.table)
library(dplyr)
library(ggplot2)
library(igraph)
library(viridis)
library(RColorBrewer)

path = "../IMDB-dataset-exploration-data/"

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

# actor names for the labels
actors_names = fread(paste0(path, "/name.basics_clean.csv"))
actors_names = actors_names[nconst %in% all_actors]
actors_names = actors_names[order(nconst), primaryName]

# series names for grouping and labels
distinct_actors = lapply(all_actors, function(a){
  data = series[lapply(actors, function(b) a %in% b) %>% unlist()]
  if (data[, .N] == 1) data[, primaryTitle] else NA
}) %>% unlist()
series_names = distinct_actors
done = character(0)
for (s in seq_along(distinct_actors)) {
  if (distinct_actors[[s]] %in% done) series_names[[s]] = NA else done = c(done, distinct_actors[[s]])
}
rm(done)

# colors for grouping vertices by series
pal = colorRampPalette(brewer.pal(8, "Pastel1"))(110)
pal = factor(distinct_actors, labels = pal)
levels(pal) = c(levels(pal), "black"); pal[is.na(pal)] = "black"

# network graph
# d[d != 0] = abs(d[d != 0] - max(d[d != 0]) - 1)  # invert the weights
network <- graph_from_adjacency_matrix(d, add.colnames = NA, weighted=NULL, mode="undirected", diag=F)
pdf("plots/network_actors.pdf", height = 20, width = 20)
set.seed(345)
plot.igraph(network,
            # mark.groups = distinct_actors,
     # mark.col = distinct_actors,
     # layout=layout.kamada.kawai,
     vertex.label=series_names,
     vertex.label.font = 2, vertex.label.cex = 1.5, vertex.label.color = "black", vertex.label.family = "URWBookman",
     # vertex.label=NA,
     label.cex = 0.1, label.dist = 1, label.degree = -pi/2,
     # arrow.size = 100,
     # vertex.size=actors_size/4,
     vertex.size=2, vertex.frame.color = "white",
     vertex.color = pal # adjustcolor(pal, alpha.f = 0.9)
     )
dev.off()


