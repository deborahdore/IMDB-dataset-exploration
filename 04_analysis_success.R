
library(data.table)
library(magrittr)
library(ggplot2); theme_set(theme_bw())
options(scipen = 1000000)
library(GGally)
library(viridis)

series = fread("../IMDB-dataset-exploration-data/merged_series_withNA.csv")

# filter all series that are in the top of ratings, numVotes and nTranslations
top = 500
qrating = quantile(series[, averageRating], 1 - top/nrow(series))
qvotes = quantile(series[, numVotes], 1 - top/nrow(series))
qtrans = quantile(series[, nTranslations], 1 - top/nrow(series))
series[, success_rating := averageRating >= qrating]
series[, success_votes := numVotes >= qvotes]
series[, success_trans := nTranslations >= qtrans]
series[, success_level := (success_rating + success_votes) %>% factor(levels = as.character(0:2))]
# series[, success_level := (success_rating + success_votes) %>% factor(levels = c("2", "1", "0"))]
series[, success := success_level != "0"]
series[success_level == 2, .N]

#View(series[(success)])

ggpubr::ggarrange(
# plotting the distribution of the successfull series
ggplot(series[(success)], aes(x = averageRating, fill = success_level)) +
  geom_density(position = "stack") +
  annotate("rect", fill = "grey20", alpha = 0.4, xmin = 0, xmax = qrating, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = qrating) +
  ggtitle("Ratings")
  # clear mode at 8-9
  # "successfull" series with averageRating < 5:
  # Metástasis (because of translations)
  # Inhumans (because of votes and translations)
  # Bizaardvark (because of translations)
  # Charmed (because of translations)
  # School (because of translations)
  # Batwoman (because of votes and translations)
  # The I-Land (because of votes)
  # The Walking Dead: World Beyond (because of translations)
,
ggplot(series[(success)], aes(x = numVotes, fill = success_level)) +
  geom_density(position = "stack") +
  annotate("rect", fill = "grey20", alpha = 0.4, xmin = 0, xmax = qvotes, ymin = -Inf, ymax = Inf) +
  scale_x_continuous(trans = "log10") +
  geom_vline(xintercept = qvotes) +
  ggtitle("Number of Votes")
  # outlier: Game of Thrones with > 2 million votes and Chernobyl with >700.000 votes
  # still very left skewed
,
ggplot(series[(success)], aes(x = nTranslations, fill = success_level)) +
  geom_density(position = "stack") +
  annotate("rect", fill = "grey20", alpha = 0.4, xmin = 0, xmax = qtrans, ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = qtrans) +
  ggtitle("Number of Translations")
  # bimodal distribution
, ncol = 3)
