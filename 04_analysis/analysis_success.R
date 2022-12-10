library(data.table)
library(magrittr)
library(ggplot2); theme_set(theme_bw())
options(scipen = 1000000)
library(GGally)
library(viridis)

setwd("..")
path <- paste0(getwd(), "/dataset")

series = fread(paste0(path, "/merged_series_withNA.csv"))

# filter all series that are in the top of ratings, numVotes and nTranslations
top = 800
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
series[(success), .N]

##### PLOT

pal = c("#00e600", "#1a8cff", "#0073e6", "#0059b3", "#8600b3", "#ac00e6", "#c61aff") %>% rev()
linepal = c("black", adjustcolor("black", alpha.f = 0), adjustcolor("black", alpha.f = 0), "black", adjustcolor("black", alpha.f = 0), adjustcolor("black", alpha.f = 0), "black")
alpha_level = 0.7

ggpubr::ggarrange(
# plotting the distribution of the successfull series
ggplot(series[success_level != "0"], aes(x = averageRating, fill = success_level, color = success_level)) +
  geom_density(position = "stack", size = 0.7, alpha = alpha_level) +
  coord_cartesian(xlim = c(5, 10)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = linepal) + guides(color = "none") +
  xlab("Average Rating") +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 5, b = 5, unit = "pt")))
,
ggplot(series[success_level != "0"], aes(x = numVotes, fill = success_level, color = success_level)) +
  geom_density(position = "stack", size = 0.7, alpha = alpha_level) +
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = linepal) + guides(color = "none") +
  xlab("Number of Votes") + ylab("")
,
ggplot(series[success_level != "0"], aes(x = nTranslations, fill = success_level, color = success_level)) +
  geom_density(position = "stack", size = 0.7, alpha = alpha_level) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = linepal) + guides(color = "none") +
  xlab("Number of Translations") + ylab("") +
  scale_x_continuous(breaks = seq(0, 80, 20)) +
  coord_cartesian(xlim = c(0, 80)) 
, ncol = 1, common.legend = TRUE, legend = "top", widths = c(1, 1, 1, 0.1))

ggsave("plots/analysis/analysis_successHistogram.pdf", height = 3, width = 12)
