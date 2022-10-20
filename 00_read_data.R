
# install.packages("data.table")
library(data.table)
path = "../IMBD-dataset-exploration-data"

# missing values are denoted "\\N"


### NAMES ----- 717 MB
# 11.972690 million rows
# lists single artists with their
# year of birth, year of death, primary profession(s)
# and max. 4 titles of pieces they are known for
names = fread(paste0(path, "/name.basics.tsv"))
head(names, 30)
summary(names)


### TITLE AKAS ----- 1.7 GB
# 33.426498 million rows
# list single titles with their
# title, region, language, ordering within all languages it was released in,
# type (no idea what this means), attribute (comment on title),
# and whether isOriginalTitle (binary)
akas = fread(paste0(path, "/title.akas.tsv"))
head(akas, 30)
summary(akas)


### TITLE BASICS ----- 793 MB
# 9.267897 million rows
# lists single titles with their
# titleType (i.e. short, movie, tvSeries, ...), primaryTitle and originalTitle,
# whether it is only suited for adults (isAdult, binary),
# startYear and endYear, runtimeMinutes and a list of genres
basics = fread(paste0(path, "/title.basics.tsv"))
head(basics, 30)
summary(basics)
unique(basics[, titleType])


### TITLE CREW ----- 304 MB
# 9.267897 million rows
# links a title with its director(s) and writer(s)
crew = fread(paste0(path, "/title.crew.tsv"))
head(crew, 30)
summary(crew)


### TITLE EPISODE ----- 182 MB
# 6.991827 million rows
# links an episode with its parent episode,
# the season number and the episode number
episode = fread(paste0(path, "/title.episode.tsv"))
head(episode, 30)
summary(episode)


### TITLE PRINCIPALS ----- 2.3 GB
# 52.370246 million rows
# lists for each title the principal persons,
# the persons have an ordering for each title, a categoray (which role they have),
# a job (which seems to be similar to category but with more missing values)
# and a character if they are actors
principals = fread(paste0(path, "/title.principals.tsv"))
head(principals, 30)
summary(principals)


### TITLE RATINGS ----- 22 MB
# 1.263181 million rows
# lists for each title the averageRating (between 1 and 10)
# and the number of Votes (min 5, max >2 million)
ratings = fread(paste0(path, "/title.ratings.tsv"))
head(ratings, 30)
summary(ratings)
