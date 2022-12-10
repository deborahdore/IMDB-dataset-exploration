# install.packages("data.table")
# install.packages("magrittr")
# install.packages("ggplot2")

library(data.table)
library(magrittr)
library(ggplot2); theme_set(theme_minimal())

setwd("..")
path <- paste0(getwd(), "/dataset")

# EXPLORATORY DATA ANALYSIS AND CLEANING --------------------------------------

### NAMES ----- 717 MB
# 11.972689 million rows (before cleaning: 11.972690 million)
# lists single artists with their
# year of birth, year of death, primary profession(s)
# and max. 4 titles of pieces they are known for
names = fread(paste0(path, "/name.basics.tsv"))
head(names, 30)
summary(names)


# NCONST
# no missing values, converted to integer
grepl("^nm\\d{7,8}$", names[, nconst]) %>% sum()  # 11972690 ==> every entry consists of "nm" followed by 7 or 8 digits
unique(names[, nconst]) %>% length()  # 11972690 ==> each row has a unique identifier
names[, nconst := as.integer(gsub("^nm", "", nconst))]  # convert to integer


# PRIMARYNAME
# not really usable
# IDEA: create column indicating whether it is a singl person or a institution
# primaryName is sometimes only the initials
# can be single people or groups of people like bands and orchestras
names[primaryName == "\\N", ] %>% nrow(); names[is.na(primaryName), ]  # one NA
names = names[!is.na(primaryName)]  # delete row with NA
names[, nchar(primaryName)] %>% summary()  # between 1 and 105 characters
names[nchar(primaryName) > 70, ]  # the long primaryName entries are mostly instutions like bands or choirs


# BIRTHYEAR and DEATHYEAR
# more than 95% missing values ==> not really usable
names[birthYear == "\\N", birthYear := NA_character_]  # transform "\\N" to NA
names[deathYear == "\\N", deathYear := NA_character_]
names[, `:=`(birthYear = as.integer(birthYear), deathYear = as.integer(deathYear))]  # convert to integer
names[, lifespan := deathYear - birthYear]  # create column lifespan
names[(birthYear > 2022) | (deathYear > 2022)] %>% nrow()  # no birth or death years in the future
names[lifespan < 0, `:=`(birthYear = NA_integer_, deathYear = NA_integer_, lifespan = NA_integer_)]  # transform negative lifespans to NA
# analyze missing values
names[is.na(birthYear)] %>% nrow()  # 11.409893 missing values (95.3%)
names[is.na(deathYear)] %>% nrow()  # 11.765993 missing values (98.3%)
names[is.na(lifespan)] %>% nrow()  # 11.778068 cases where either birthYear or deathYear are missing (98.4%)
names[!is.na(lifespan)] %>% nrow()  # 194.621 cases with both birthYear and deathYear
# visualizations:
ggplot(names[birthYear > 1872, ], aes(x = birthYear)) + geom_histogram(bins = 150)
ggsave("plots/preprocessing/birthYear_histogram.jpg", height = 6, width = 12)
ggplot(names[deathYear > 1872, ], aes(x = deathYear)) + geom_histogram(bins = 150)
ggsave("plots/preprocessing/deathYear_histogram.jpg", height = 6, width = 12)
ggplot(names[!is.na(lifespan)], aes(x = lifespan)) + geom_histogram(bins = 123) + scale_x_continuous(breaks = seq(0, 122, 2))
ggsave("plots/preprocessing/lifespan_histogram.jpg", height = 6, width = 12)

# PRIMARYPROFESSION
# transformed into list, added column nProfessions
names[primaryProfession == "\\N", ] %>% nrow()
names[primaryProfession == "", ] %>% nrow()  # 2.577.463 missing values (21.5%)
((nchar(gsub("[^,]", "", names[, primaryProfession]))) + 1) %>% table()  # between 1 and 3 professions
names[, primaryProfession := gsub("actress", "actor", primaryProfession)]  # transform "actress" to "actor"
names[primaryProfession == "", primaryProfession := NA_character_]  # transform empty characters to NA
names[, primaryProfession := strsplit(names[, primaryProfession], ",")]  # transform into list
names[, primaryProfession] %>% unlist() %>% table()
# create column with number of professions
names[, nProfessions := names[, primaryProfession] %>% lapply(length) %>% unlist()]
names[is.na(primaryProfession), nProfessions := NaN]


# KNOWNFORTITLES
# maybe add a column indicating the number of titles known for
# 2.121.080 missing values
# between 1 and 6 titles known for
names[knownForTitles == "\\N", ] %>% nrow()  # 2.121.080 missing values (17.7%)
names[knownForTitles == "\\N", knownForTitles := NA_character_]  # transform "\\N" to NA
(names[!is.na(knownForTitles), knownForTitles] %>% grepl("((tt\\d{7,8},)+)|(^tt\\d{7,8}$)", .) %>% sum()) +
  nrow(names[is.na(knownForTitles)])  # 11972690 ==> every entry is either NA or a (series of) tconst
names[, knownForTitles := gsub("tt", "", knownForTitles)]
names[, knownForTitles := strsplit(knownForTitles, ",") %>% lapply(as.integer)]  # split the titlesKnownFor into a list
# create column with number of titles known for
names[, nTitles := names[, knownForTitles] %>% lapply(length) %>% unlist()]
names[is.na(knownForTitles), nTitles := 0]
table(names[, nTitles])



# SAVE CLEANED DATASET --------------------------------------------------------
fwrite(names, paste0(path, "/name.basics.clean.csv"))
