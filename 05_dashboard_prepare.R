
library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2); theme_set(theme_bw())
options(scipen = 1000000)
library(ggpubr)
library(viridis)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(fmsb)


path = "../IMDB-dataset-exploration-data/"

series = fread(paste0(path, "/merged_series_withNA.csv"))

plot_list = list()
value_list = list()


# topDensity ----
pal = c("#00e600", "#1a8cff", "#0073e6", "#0059b3", "#8600b3", "#ac00e6", "#c61aff") %>% rev()
linepal = c("black", adjustcolor("black", alpha.f = 0), adjustcolor("black", alpha.f = 0), "black", adjustcolor("black", alpha.f = 0), adjustcolor("black", alpha.f = 0), "black")
alpha_level = 0.7
for (top in seq(100, 1000, 100)) {
  qrating = quantile(series[, averageRating], 1 - top/nrow(series))
  qvotes = quantile(series[, numVotes], 1 - top/nrow(series))
  qtrans = quantile(series[, nTranslations], 1 - top/nrow(series))
  series[, success_rating := averageRating >= qrating]
  series[, success_votes := numVotes >= qvotes]
  series[, success_trans := nTranslations >= qtrans]
  series[, success_level := ifelse((averageRating >= qrating) & (numVotes >= qvotes) & (nTranslations >= qtrans), 7,
                                   ifelse((averageRating >= qrating) & (numVotes >= qvotes) & !(nTranslations >= qtrans), 6,
                                          ifelse((averageRating >= qrating) & !(numVotes >= qvotes) & (nTranslations >= qtrans), 5,
                                                 ifelse(!(averageRating >= qrating) & (numVotes >= qvotes) & (nTranslations >= qtrans), 4,
                                                        ifelse((averageRating >= qrating) & !(numVotes >= qvotes) & !(nTranslations >= qtrans), 3,
                                                               ifelse(!(averageRating >= qrating) & (numVotes >= qvotes) & !(nTranslations >= qtrans), 2,
                                                                      ifelse(!(averageRating >= qrating) & !(numVotes >= qvotes) & (nTranslations >= qtrans), 1,
                                                                             0)))))))]
  series[, success := success_level == 7]
  series[, success_level := factor(success_level, levels = 0:7,
                                   labels = c(paste0("SUCCESSFULL:\nin top ", top, " ratings &\nin top ", top, " votes &\nin top ", top, " translations"),
                                              paste0("in top ", top, " ratings &\nin top ", top, " votes"),
                                              paste0("in top ", top, " ratings &\nin top ", top, " translations"),
                                              paste0("in top ", top, " votes &\nin top ", top, " translations"),
                                              paste0("in top ", top, " ratings"),
                                              paste0("in top ", top, " votes"),
                                              paste0("in top ", top, " translations"), "0") %>% rev())]
  
  p = ggarrange(
    # plotting the distribution of the successfull series
    ggplot(series[success_level != "0"], aes(x = averageRating, fill = success_level, color = success_level)) +
      geom_density(position = "stack", size = 0.7, alpha = alpha_level) +
      # annotate("rect", fill = "grey80", alpha = 0.4, xmin = 0, xmax = qrating, ymin = -Inf, ymax = Inf) +
      # geom_vline(xintercept = qrating, color = "grey50") +
      coord_cartesian(xlim = c(5, 10)) +
      scale_fill_manual(values = pal) +
      scale_color_manual(values = linepal) + guides(color = "none") +
      # ggtitle("Ratings") +
      xlab("Average Rating") +
      theme(legend.title = element_blank(),
            legend.text = element_text(margin = margin(t = 5, b = 5, unit = "pt")))
    ,
    ggplot(series[success_level != "0"], aes(x = numVotes, fill = success_level, color = success_level)) +
      geom_density(position = "stack", size = 0.7, alpha = alpha_level) +
      # annotate("rect", fill = "grey80", alpha = 0.4, xmin = 0, xmax = qvotes, ymin = -Inf, ymax = Inf) +
      # geom_vline(xintercept = qvotes, color = "grey50) +
      scale_x_continuous(trans = "log10") +
      scale_fill_manual(values = pal) +
      scale_color_manual(values = linepal) + guides(color = "none") +
      # ggtitle("Number of Votes") +
      xlab("Number of Votes") + ylab("")
    ,
    ggplot(series[success_level != "0"], aes(x = nTranslations, fill = success_level, color = success_level)) +
      geom_density(position = "stack", size = 0.7, alpha = alpha_level) +
      # annotate("rect", fill = "grey80", alpha = 0.4, xmin = 0, xmax = qtrans, ymin = -Inf, ymax = Inf) +
      # geom_vline(xintercept = qtrans, color = "grey50") +
      scale_fill_manual(values = pal) +
      scale_color_manual(values = linepal) + guides(color = "none") +
      # ggtitle("Number of Translations") +
      xlab("Number of Translations") + ylab("") +
      scale_x_continuous(breaks = seq(0, 80, 20)) +
      coord_cartesian(xlim = c(0, 80)) 
    # bimodal distribution
    , ncol = 4, common.legend = TRUE, legend = "right", widths = c(1, 1, 1, 0.1))
  
  plot_list[[paste0("topDensity_", top)]] = p
  value_list[[paste0("topDensity_", top)]] = series[(success), .N]
  
  series[, success_level := NULL]
}



# histo ----




# radarPlot ----
series = fread(paste0(path, "/merged_series_withNA.csv"))
series = series[success==TRUE]
colors_border <- brewer.pal(6, "BuPu")[2:6]
colors_in <- alpha(colors_border,0.9)
top_5 = series[, log_numVotes := log10(numVotes)]
top_5 = series[,list(primaryTitle, log_numVotes, averageRating, nTranslations, runtimeMinutes, nSeasons)]
fun_radarPlot = function(titles, top_5) {
  min_top_5 = list("MINIMO",
                   min(top_5$log_numVotes),
                   min(top_5$averageRating),
                   min(top_5$nTranslations),
                   min(top_5$runtimeMinutes),
                   min(top_5$nSeasons))
  max_top_5 = list("MASSIMO", max(top_5$log_numVotes),
                   max(top_5$averageRating),
                   max(top_5$nTranslations),
                   100, #max(top_5$runtimeMinutes),
                   max(top_5$nSeasons))
  top_5 = merge(data.frame("primaryTitle" = titles), top_5, all.x = TRUE, all.y = FALSE)
  top_5 = rbindlist(list(max_top_5 , min_top_5 , top_5))
  colnames(top_5) = c("primaryTitle", "Number of Votes", "Average Rating", "Number of Translations", "Runtime", "Number of Seasons")
  top_5[,list(`Number of Votes`, `Average Rating`, `Number of Translations`, `Runtime`, `Number of Seasons`)] %>%
    radarchart(axistype=0,
               pcol=colors_border, plwd=2.5, plty=1.5,
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels = seq(0, 2, 0.2), 
               cglwd=0.8,
               vlcex=0.8
    )
  legend(x=1.2, y=1.3, legend = top_5[3:7, primaryTitle],
         bty = "n", pch=20 , col=colors_in , text.col = "black", cex=0.8, pt.cex=3)
}



# seasonsViolin ----
series = fread(paste0(path, "/merged_series_withNA.csv"))
seasons = fread(paste0(path, "/seasons.csv"))
colnames(seasons)[[2]] = "tconst"
seasons = seasons[tconst %in% series[, tconst]]
seasons = merge(seasons[, .(ID, tconst, seasonNumber, nEpisodes, averageRuntimeMinutes, genres, numVotes, averageRating, minRating, maxRating)],
                series[, .(tconst, primaryTitle, success, averageRating, numVotes, runtimeMinutes, nTranslations, nSeasons, maxSeasonNumber, startYear)],
                by = "tconst", suffixes = c("Season", "Series"))
seasons[(success), tconst] %>% unique() %>% length()
seasons = seasons[maxSeasonNumber == nSeasons]
series_seasons = unique(seasons[, .(tconst, primaryTitle, success, averageRatingSeries, numVotesSeries, runtimeMinutes, nTranslations, nSeasons, startYear)])
series_seasons[, Seasons := ifelse(nSeasons == 1, "1", ifelse(nSeasons %in% 2:4, "2-4", ifelse(nSeasons %in% 5:7, "5-7", ifelse(nSeasons %in% 8:12, "8-12", "13+"))))]
series_seasons[, Seasons := factor(Seasons, levels = c("1", "2-4", "5-7", "8-12", "13+"))]
seasons[, individualRating := (sd(averageRatingSeason, na.rm = TRUE) != 0), by = tconst]
seasons = seasons[(individualRating)][, individualRating := NULL]

plot_list$seasonsViolin_TRUE = ggpubr::ggarrange(
  ggplot(series_seasons, aes(x = Seasons, y = averageRatingSeries, alpha = success)) +
    scale_alpha_manual(values = c(0.2, 0.8),
                       labels = c("no", "yes")) +
    geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5)) +
    xlab("Number of Seasons") + ylab("") +
    ggtitle("Average Rating")
  ,
  ggplot(series_seasons, aes(x = Seasons, y = numVotesSeries, alpha = success)) +
    scale_y_continuous(trans = "log10") +
    scale_alpha_manual(values = c(0.2, 0.8),
                       labels = c("no", "yes")) +
    geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5)) +
    xlab("Number of Seasons") + ylab("") +
    ggtitle("Number of Votes")
  ,
  ggplot(series_seasons, aes(x = Seasons, y = nTranslations, alpha = success)) +
    scale_alpha_manual(values = c(0.2, 0.8),
                       labels = c("no", "yes")) +
    geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5)) +
    coord_cartesian(ylim = c(0, 80)) +
    xlab("Number of Seasons") + ylab("") +
    ggtitle("Number of Translations")
  ,
  ggplot(series_seasons[runtimeMinutes <= 200], aes(x = Seasons, y = runtimeMinutes, alpha = success)) +
    scale_alpha_manual(values = c(0.2, 0.8),
                       labels = c("no", "yes")) +
    geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5)) +
    coord_cartesian(ylim = c(0, 100)) +
    xlab("Number of Seasons") + ylab("") +
    ggtitle("Runtime in Minutes")
  , ncol = 4, common.legend = TRUE, legend = "right")

plot_list$seasonsViolin_FALSE = ggpubr::ggarrange(
  ggplot(series_seasons, aes(x = Seasons, y = averageRatingSeries)) +
    geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5), alpha = 0.2) +
    xlab("Number of Seasons") + ylab("") +
    ggtitle("Average Rating")
  ,
  ggplot(series_seasons, aes(x = Seasons, y = numVotesSeries)) +
    scale_y_continuous(trans = "log10") +
    geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5), alpha = 0.2) +
    xlab("Number of Seasons") + ylab("") +
    ggtitle("Number of Votes")
  ,
  ggplot(series_seasons, aes(x = Seasons, y = nTranslations)) +
    geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5), alpha = 0.2) +
    coord_cartesian(ylim = c(0, 80)) +
    xlab("Number of Seasons") + ylab("") +
    ggtitle("Number of Translations")
  ,
  ggplot(series_seasons[runtimeMinutes <= 200], aes(x = Seasons, y = runtimeMinutes)) +
    geom_violin(scale = "width", fill = "#4d0099", position = position_dodge(0.5), alpha = 0.2) +
    coord_cartesian(ylim = c(0, 100)) +
    xlab("Number of Seasons") + ylab("") +
    ggtitle("Runtime in Minutes"),
  ggplot() + theme(panel.border = element_blank())
  , ncol = 5, widths = c(1, 1, 1, 1, 0.2))


# genresBar ----
tb = fread(paste0(path, "/merged_series_withNA.csv"))
tb[, genres := strsplit(genres, "\\|")]
all_genres = tb[1:100000, genres] %>% unlist() %>% unique()
all_genres = all_genres[all_genres != "Western"]
# all_genres = c("Action", "Adventure", "Crime", "Mystery", "Drama", "Animation", "Sci-Fi", "Thriller", "Fantasy", "Comedy", "Romance")
for (g_col in all_genres) {tb[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]}
tb = melt(tb, id.vars = c("tconst", "success", "runtimeMinutes", "averageRating", "numVotes", "nTranslations"),
          measure.vars = all_genres,
          variable.name = "genres", value.name = "count")
tb = tb[(count == TRUE)]
tb[, count := NULL]
tb[, total := .N, by = genres]
tb[, total_success := .N, by = .(genres, success)]
tb[, share := total_success/total]
tb = unique(tb[, .(success, genres, total, total_success, share)])
tb[, genres := factor(genres, levels = tb[(success)][order(-share), genres])]
fun_genresBar = function(all_genres, tb) {
  ggpubr::ggarrange(
    ggplot(tb[(success) & (genres %in% all_genres)], aes(x = reorder(genres, genres), y = share, fill = genres,
                                                         label = paste0(" ", round(share, 3) * 100, "%"))) +
      geom_bar(aes(color = genres), stat = "identity", alpha = 0.7) +
      geom_text(nudge_y = 0.0007) +
      xlab("") + ylab("") +
      scale_y_continuous(breaks = c(0.005, 0.01, 0.015), labels = c("0.5%", "1%", "1.5%")) +
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE) +
      ggtitle("Share of successfull series in each genre") +
      theme(axis.text.x=element_text(angle=40,hjust=1),
            legend.position = "none")
    ,
    ggplot(tb[(success) & (genres %in% all_genres)], aes(x = genres, y = total_success, fill = genres,
                                                         label = total_success)) +
      geom_bar(aes(color = genres), stat = "identity", alpha = 0.7) +
      geom_text(nudge_y = 3) +
      xlab("") + ylab("") +
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE) +
      ggtitle("Number of successfull series in each genre") +
      theme(axis.text.x=element_text(angle=40,hjust=1),
            legend.position = "none")
    , ncol = 2)
}

# genresViolin ---- 
tb2 = fread(paste0(path, "/merged_series_withNA.csv"))
tb2[, genres := strsplit(genres, "\\|")]
all_genres = tb2[1:100000, genres] %>% unlist() %>% unique() %>% sort()
for (g_col in all_genres) {tb2[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]}
tb2 = melt(tb2, id.vars = c("tconst", "success", "runtimeMinutes", "averageRating", "numVotes", "nTranslations"),
           measure.vars = all_genres,
           variable.name = "genres", value.name = "count")
tb2 = tb2[(count == TRUE)]
tb2[, count := NULL]
fun_genresViolin = function(all_genres, tb2) {
  ggpubr::ggarrange(
    # rating
    ggplot(tb2[(genres %in% all_genres)], aes(x = genres, y = averageRating, fill = genres, alpha = success)) +
      geom_violin(scale = "width", position = position_dodge(0.5)) +
      scale_alpha_manual(values = c(0.2, 0.8), labels = c("no", "yes")) +
      scale_fill_viridis(discrete = TRUE, end = 0.85) +
      coord_cartesian(ylim = c(5, 10)) +
      ggtitle("Average Rating") + xlab("") + ylab("") +
      guides(fill = "none") +
      guides(alpha = guide_legend(override.aes = list(fill = "grey50"))) +
      theme(axis.text.x=element_text(angle=40,hjust=1)),
    # votes
    ggplot(tb2[(genres %in% all_genres)], aes(x = genres, y = numVotes, fill = genres, alpha = success)) +
      geom_violin(scale = "width", position = position_dodge(0.5)) +
      scale_alpha_manual(values = c(0.2, 0.8)) +
      scale_y_continuous(trans = "log10") +
      scale_fill_viridis(discrete = TRUE, end = 0.85) +
      ggtitle("Number of Votes") + xlab("") + ylab("") +
      theme(axis.text.x=element_text(angle=40,hjust=1)),
    # translations
    ggplot(tb2[(genres %in% all_genres)], aes(x = genres, y = nTranslations, fill = genres, alpha = success)) +
      geom_violin(scale = "width", position = position_dodge(0.5)) +
      scale_alpha_manual(values = c(0.2, 0.8)) +
      scale_fill_viridis(discrete = TRUE, end = 0.85) +
      coord_cartesian(ylim = c(0, 80)) +
      ggtitle("Number of Translations") + xlab("") + ylab("") +
      theme(axis.text.x=element_text(angle=40,hjust=1)),
    # run time
    ggplot(tb2[(runtimeMinutes <= 200) & (genres %in% all_genres)], aes(x = genres, y = runtimeMinutes, fill = genres, alpha = success)) +
      geom_violin(scale = "width", position = position_dodge(0.5)) +
      scale_alpha_manual(values = c(0.2, 0.8)) +
      scale_y_continuous(breaks = c(20, 40, 60, 80)) +
      scale_fill_viridis(discrete = TRUE, end = 0.85) +
      coord_cartesian(ylim = c(0, 100)) +
      ggtitle("Runtime in Minutes") + xlab("") + ylab("") +
      theme(axis.text.x=element_text(angle=40,hjust=1)),
    ncol = 4, common.legend = TRUE, legend = "right", widths = c(1, 1.2, 1, 1))
}


# genresLine ----
series = fread(paste0(path, "/merged_series_withNA.csv"))
seasons = fread(paste0(path, "/seasons.csv"))
colnames(seasons)[[2]] = "tconst"
seasons = seasons[tconst %in% series[, tconst]]
seasons = merge(seasons[, .(ID, tconst, seasonNumber, nEpisodes, averageRuntimeMinutes, genres, numVotes, averageRating, minRating, maxRating)],
                series[, .(tconst, primaryTitle, success, averageRating, numVotes, runtimeMinutes, nTranslations, nSeasons, maxSeasonNumber, startYear)],
                by = "tconst", suffixes = c("Season", "Series"))
seasons[(success), tconst] %>% unique() %>% length()
seasons = seasons[maxSeasonNumber == nSeasons]
seasons[, individualRating := (sd(averageRatingSeason, na.rm = TRUE) != 0), by = tconst]
seasons = seasons[(individualRating)][, individualRating := NULL]
season_genres = seasons
season_genres[, genres := strsplit(genres, "\\|")]
all_genres2 = c("Action", "Crime", "Mystery", "Drama", "Sci-Fi", "Comedy")
for (g_col in all_genres) {
  season_genres[, (g_col) := lapply(genres, function(g_row) any(g_row == g_col)) %>% unlist()]
}
season_genres = melt(season_genres,
                     id.vars = c("tconst", "seasonNumber", "maxSeasonNumber", "averageRatingSeason", "success", "numVotesSeason", "averageRuntimeMinutes", "nTranslations"),
                     measure.vars = all_genres2,
                     variable.name = "genre", value.name = "genreBool")
season_genres = season_genres[(genreBool)][, genreBool := NULL]
linesize = 0.5
for (k in 2:15) {
  p = ggpubr::ggarrange(
    # average Rating
    ggplot() +
      geom_violin(data = season_genres[(seasonNumber <= k) & !(success)],
                  mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRatingSeason)) +
      geom_violin(data = season_genres[(seasonNumber <= k) & (success)],
                  mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRatingSeason, color = genre, fill = genre),
                  alpha = 0.2, size = linesize) +
      geom_line(data = season_genres[(seasonNumber <= k) & !(success)],
                mapping = aes(x = seasonNumber, y = averageRatingSeason, group = tconst),
                alpha = 0.1) +
      geom_line(data = season_genres[(seasonNumber <= k) & (success)],
                mapping = aes(x = seasonNumber, y = averageRatingSeason, group = tconst, color = genre),
                alpha = 0.7, size = linesize) +
      scale_x_continuous(breaks = 1:k) +
      scale_fill_viridis(discrete = TRUE, end = 0.85) +
      scale_color_viridis(discrete = TRUE, end = 0.85) +
      ylab("Average Rating\nof Episode") + xlab("") +
      facet_wrap(~ genre, ncol = 6) +
      theme(legend.position = "none"),
    # number of votes
    ggplot() +
      geom_violin(data = season_genres[(seasonNumber <= k) & !(success)],
                  mapping = aes(x = seasonNumber, group = seasonNumber, y = numVotesSeason)) +
      geom_violin(data = season_genres[(seasonNumber <= k) & (success)],
                  mapping = aes(x = seasonNumber, group = seasonNumber, y = numVotesSeason, color = genre, fill = genre),
                  alpha = 0.2, size = linesize) +
      geom_line(data = season_genres[(seasonNumber <= k) & !(success)],
                mapping = aes(x = seasonNumber, y = numVotesSeason, group = tconst),
                alpha = 0.1) +
      geom_line(data = season_genres[(seasonNumber <= k) & (success)],
                mapping = aes(x = seasonNumber, y = numVotesSeason, group = tconst, color = genre),
                alpha = 0.7, size = linesize) +
      scale_x_continuous(breaks = 1:k) +
      scale_y_continuous(trans = "log10") +
      scale_fill_viridis(discrete = TRUE, end = 0.85) +
      scale_color_viridis(discrete = TRUE, end = 0.85) +
      ylab("Average Number of Votes\nof Episodes") + xlab("") +
      facet_wrap(~ genre, ncol = 6) +
      theme(legend.position = "none"),
    # run time
    ggplot() +
      geom_violin(data = season_genres[(seasonNumber <= k) & !(success)],
                  mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRuntimeMinutes)) +
      geom_violin(data = season_genres[(seasonNumber <= k) & (success)],
                  mapping = aes(x = seasonNumber, group = seasonNumber, y = averageRuntimeMinutes, color = genre, fill = genre),
                  alpha = 0.2, size = linesize) +
      geom_line(data = season_genres[(seasonNumber <= k) & !(success)],
                mapping = aes(x = seasonNumber, y = averageRuntimeMinutes, group = tconst),
                alpha = 0.1) +
      geom_line(data = season_genres[(seasonNumber <= k) & (success)],
                mapping = aes(x = seasonNumber, y = averageRuntimeMinutes, group = tconst, color = genre),
                alpha = 0.7, size = linesize) +
      scale_x_continuous(breaks = 1:k) +
      scale_y_continuous(breaks = c(20, 40, 60, 80)) +
      scale_fill_viridis(discrete = TRUE, end = 0.85) +
      scale_color_viridis(discrete = TRUE, end = 0.85) +
      ylab("Average Runtime\nof Episodes in Minutes") + xlab("Season Number") +
      coord_cartesian(ylim = c(0, 100)) +
      facet_wrap(~ genre, ncol = 6) +
      theme(legend.position = "none"),
    ncol = 1)
  
  plot_list[[paste0("genresLine_", k)]] = p
}



# networksActors ----
series = fread(paste0(path, "/merged_series_withNA.csv"))
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
# network graph
network_actors <- graph_from_adjacency_matrix(d, add.colnames = NA, weighted=NULL, mode="undirected", diag=F)
# genres for coloring
series[, genres := strsplit(genres, "\\|")]
all_genres = series[1:100000, genres] %>% unlist() %>% unique()
for (g in all_genres) {
  vcol = lapply(all_actors, function(a) {
           bool_a = lapply(series[, actors], function(s) a %in% s) %>% unlist()
           bool_g = lapply(series[, genres], function(genre) g %in% genre) %>% unlist()
           any(bool_a & bool_g)
         }) %>% unlist()
  value_list[[paste0("networkActors_", g)]] = vcol
}



# networksDirectors ----
series = fread(paste0(path, "/merged_series_withNA.csv"))
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
# network graph
network_directors <- graph_from_adjacency_matrix(d, add.colnames = NA, weighted=NULL, mode="undirected", diag=F)
# genres for coloring
series[, genres := strsplit(genres, "\\|")]
all_genres = series[1:100000, genres] %>% unlist() %>% unique()
for (g in all_genres) {
  vcol = lapply(all_directors, function(a) {
    bool_a = lapply(series[, directors], function(s) a %in% s) %>% unlist()
    bool_g = lapply(series[, genres], function(genre) g %in% genre) %>% unlist()
    any(bool_a & bool_g)
  }) %>% unlist()
  value_list[[paste0("networkDirectors_", g)]] = vcol
}




# SAVE ----
rm("p", "season_genres", "seasons", "series", "series_seasons",
   "all_genres2", "g_col", "k", "qrating", "qtrans", "qvotes", "top")
save.image("05_dashboard_workspace.rda")
