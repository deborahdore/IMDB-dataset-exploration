#install.packages("data.table")
#install.packages("magrittr")
#install.packages("ggplot2")
#install.packages("gocookbook")

library(data.table)
library(magrittr)
library(ggplot2); theme_set(theme_bw())

setwd("..")
path <- paste0(getwd(), "/dataset")


# EXPLORATORY DATA ANALYSIS AND CLEANING ---------------------------------------

### TITLE RATINGS ----- 22 MB
# 1.263.181 million rows 
# links a title with average rating and number of votes
ratings = fread(paste0(path, "/title.ratings.tsv"))
head(ratings,30)
summary(ratings)

### MISSING VALUES
# no missing values 
sum(is.na(ratings))
sum(ratings == '') 
sum(ratings == "//N")


# TCONST
# no missing value, converted to integer
grepl("^tt\\d{7,8}$", ratings[, tconst]) %>% sum()  # 1263181 ==> every entry consists of "nm" followed by 7 or 8 digits
unique(ratings[, tconst]) %>% length()  # 126381 ==> each row has a unique identifier
ratings[, tconst := as.integer(gsub("^tt", "", tconst))]  # convert to integer


# SAVE CLEANED DATSET ---------------------------------------------------------
fwrite(ratings, paste0(path, "/title.ratings.clean.csv"))
ratings = fread(paste0(path, "/title.ratings.clean.csv"))

ggplot(ratings, aes(x=numVotes)) + geom_histogram()

ggplot(ratings[numVotes<100], aes(x=numVotes, y=..density..)) + 
  geom_histogram(colour = "black", fill = "blue") + 
  geom_density()
ggsave("plots/preprocessing/num_of_votes_histogram.jpg",height = 5, width = 8)

ggplot(ratings[numVotes<100], aes(x=numVotes)) + geom_density()
ggplot(ratings, aes(x=averageRating)) + 
  geom_histogram(colour = "black", fill = "blue")
ggsave("plots/preprocessing/average_rating_histogram.jpg",height = 5, width = 8)
