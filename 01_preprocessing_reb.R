
library(data.table)
library(magrittr)
library(ggplot2); theme_set(theme_minimal())
path = "../IMDB-dataset-exploration-data"


# POSSIBLE HYPOTHESES
# - titles with more (>= 6?) principal persons have better rating
# - titles portraying someone playing themselves have better rating
# - titles with people born before 1880 have different distribution of categories (i.e. mostly documentaries?)
# - people with more professions are known for titles which have better rating
#   OR: titles whose directors and/or writers have multiple professions have better ratings


### TITLE PRINCIPALS ----- 2.3 GB
# 52.370246 million rows
# lists for each title the principal persons,
# the persons have an ordering for each title, a categoray (which role they have),
# a job (which seems to be similar to category but with more missing values)
# and a character if they are actors
principals = fread(paste0(path, "/title.principals.tsv"))
head(principals, 30)
summary(principals)

# TCONST
# no missing values, 8.346.176 unique identifiers
grepl("^tt\\d{7,8}$", principals[, tconst]) %>% sum()  # 52370246
unique(principals[, tconst]) %>% length()  # 8346176

# ORDERING
# redundant
# no missing values, up to 10 persons per title
unique(principals[, ordering])
table(principals[, ordering]) / 1e5

# NCONST
# no missing values, 4.802.538 unique identifiers
grepl("^nm\\d{7,8}$", principals[, nconst]) %>% sum()  # 52370246
unique(principals[, nconst]) %>% length()  # 4802538

# CATEGORY
# no missing values, category "actress" changed to "actor"
principals[category == "//N", ]
principals[category == "actress", category := "actor"]
unique(principals[, category])

# JOB
# redundant:
# specifies the column "category".
# These details are not valuable for a quantitative analysis.
# Contains 8.591.949 missing values.
# In 4.323.578 it is equal to category.
principals[job == "\\N", ] %>% nrow()
principals[job == category, ] %>% nrow()
principals[job != "\\N", ][4000000:4000100, , with = TRUE]

# CHARACTER -----
# could be reduced to 3 categories "NA", "some character" and "self"
# 26.796.202 missing values
principals[characters == "\\N", ] %>% nrow()  # 26796202
principals[characters != "\\N", ] %>% head(100)





### NAMES ----- 717 MB
# 11.972690 million rows
# lists single artists with their
# year of birth, year of death, primary profession(s)
# and max. 4 titles of pieces they are known for
names = fread(paste0(path, "/name.basics.tsv"))
head(names, 30)
summary(names)

# NCONST
# no missing values, 11.972.690 unique identifiers
grepl("^nm\\d{7,8}$", names[, nconst]) %>% sum()  # 11972690
unique(names[, nconst]) %>% length()  # 11972690

# PRIMARYNAME
# potentially redundant
# 1 missing value
# primaryName is sometimes only the initials
# can be single people or groups of people like bands and orchestras
names[primaryName == "\\N", ] %>% nrow()
names[is.na(primaryName), ]
names[, nchar(primaryName)] %>% summary()
names[nchar(primaryName) > 50, ] %>% head(20)

# BIRTHYEAR and DEATHYEAR
# birthYear: 11.409.886 missing values
# deathYear: 11.765.986 missing values
# create lifespan = deathYear - birthYear and delete negative lifespans
names[birthYear == "\\N", birthYear := NA_character_]
names[deathYear == "\\N", deathYear := NA_character_]
names[, `:=`(birthYear = as.integer(birthYear), deathYear = as.integer(deathYear))]
names[, lifespan := deathYear - birthYear]
names[lifespan < 0, `:=`(birthYear = NA_integer_, deathYear = NA_integer_, lifespan = NA_integer_)]
# visualizations:
ggplot(names[birthYear > 1872, ], aes(x = birthYear)) + geom_histogram(bins = 150)
ggplot(names[deathYear > 1872, ], aes(x = deathYear)) + geom_histogram(bins = 150)
ggplot(names[!is.na(lifespan)], aes(x = lifespan)) + geom_histogram(bins = 123) + scale_x_continuous(breaks = seq(0, 122, 2))

# PRIMARYPROFESSION
# no missing values, between 1 and 3 professions
names[primaryProfession == "\\N", ] %>% nrow()
names[, primaryProfession] %>% unique() %>% length()
((nchar(gsub("[^,]", "", names[, primaryProfession]))) + 1) %>% table()

# KNOWNFORTITLES
# maybe add a column indicating the number of titles known for
# 2.121.080 missing values
# between 1 and 6 titles known for
names[knownForTitles == "\\N", ] %>% nrow()
((nchar(gsub("[^,]", "", names[, knownForTitles]))) + 1) %>% table()
names[nchar(gsub("[^,]", "", names[, knownForTitles])) >= 4, ]





### TITLE CREW ----- 304 MB
# 9.267897 million rows
# links a title with its director(s) and writer(s)
crew = fread(paste0(path, "/title.crew.tsv"))
head(crew, 30)
summary(crew)

# TCONST
# no missing value, one row per unique title
grepl("^tt\\d{7,8}$", crew[, tconst]) %>% sum()  # 9267897
unique(crew[, tconst]) %>% length()  # 9267897

# DIRECTORS and WRITERS
# The 3.285.054 rows which have neither directors nor writers are redundant
# some entries contain the whole list of contributors, these have to be deleted
crew[directors == "\\N", directors := NA_character_]
crew[writers == "\\N", writers := NA_character_]
crew[is.na(directors), ] %>% nrow()  # 3966150
crew[is.na(writers), ] %>% nrow()  # 4506648
crew[is.na(directors) & is.na(writers), ] %>% nrow()  # 3285054
((nchar(gsub("[^,]", "", crew[, directors]))) + 1) %>% table()
crew[((nchar(gsub("[^,]", "", crew[, directors]))) + 1) > 30] %>% nrow()  # 805