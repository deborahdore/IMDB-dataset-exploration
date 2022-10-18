
library(data.table)
library(magrittr)
library(ggplot2); theme_set(theme_minimal())
path = "../IMDB-dataset-exploration-data"



# CLEANING OPERATIONS ONLY ----------------------------------------------------
crew = fread(paste0(path, "/title.crew.tsv"))
crew[directors == "\\N", directors := NA_character_]
crew[writers == "\\N", writers := NA_character_]
crew = crew[!(is.na(directors) & is.na(writers))]
crew[, directors := gsub("nm", "", directors)]
crew[, writers := gsub("nm", "", writers)]
crew[, directors := strsplit(directors, ",") %>% lapply(as.integer)]
crew[, writers := strsplit(writers, ",") %>% lapply(as.integer)]
crew[, nDirectors := crew[, directors] %>% lapply(length) %>% unlist()]
crew[, nWriters := crew[, writers] %>% lapply(length) %>% unlist()]
crew[is.na(directors), nDirectors := 0]
crew[is.na(writers), nWriters := 0]




# EXPLORATORY DATA ANALYSIS AND CLEANING --------------------------------------

### TITLE CREW ----- 304 MB
# 5.982.843 million rows (before cleaning 9.267897 million rows)
# links a title with its director(s) and writer(s)
crew = fread(paste0(path, "/title.crew.tsv"))
head(crew, 30)
summary(crew)


# DIRECTORS and WRITERS
# The 3.285.054 rows which have neither directors nor writers are redundant ==> delete
crew[directors == "\\N", directors := NA_character_]
crew[writers == "\\N", writers := NA_character_]
crew[is.na(directors), ] %>% nrow()  # 3966150 (42.8%)
crew[is.na(writers), ] %>% nrow()  # 4506648 (48.6%)
crew[is.na(directors) & is.na(writers), ] %>% nrow()  # 3285054 (35.4%) ==> redundant
crew = crew[!(is.na(directors) & is.na(writers))]  # delete redundant rows
nrow(crew)  # 5982843
(crew[!is.na(directors), directors] %>% grepl("((nm\\d{7,8},)+)|(^nm\\d{7,8}$)", .) %>% sum()) +
  nrow(crew[is.na(directors)])  # 5982843 ==> every entry of directors is either NA or a (series of) nconst
(crew[!is.na(writers), writers] %>% grepl("((nm\\d{7,8},)+)|(^nm\\d{7,8}$)", .) %>% sum()) +
  nrow(crew[is.na(writers)])  # 5982843 ==> every entry of writers is either NA or a (series of) nconst
crew[, directors := gsub("nm", "", directors)]
crew[, writers := gsub("nm", "", writers)]
crew[, directors := strsplit(directors, ",") %>% lapply(as.integer)]  # split the directors into a list
crew[, writers := strsplit(writers, ",") %>% lapply(as.integer)]  # split the writers into a list
# create columns with number of directors and writers known for
crew[, nDirectors := crew[, directors] %>% lapply(length) %>% unlist()]
crew[, nWriters := crew[, writers] %>% lapply(length) %>% unlist()]
crew[is.na(directors), nDirectors := 0]
crew[is.na(writers), nWriters := 0]
table(crew[, nDirectors])  # some rows have very many directors
table(crew[, nWriters])  # some rows have very many writers


# TCONST
# no missing value, converted to integer
grepl("^tt\\d{7,8}$", crew[, tconst]) %>% sum()  # 5982843 ==> every entry consists of "nm" followed by 7 or 8 digits
unique(crew[, tconst]) %>% length()  # 5982843==> each row has a unique identifier
crew[, tconst := as.integer(gsub("^tt", "", tconst))]  # convert to integer




# SAVE CLEANED DATSET ---------------------------------------------------------
fwrite(crew, paste0(path, "/title.crew_clean.csv"))
