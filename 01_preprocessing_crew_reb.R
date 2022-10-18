
library(data.table)
library(magrittr)
library(ggplot2); theme_set(theme_minimal())
path = "../IMDB-dataset-exploration-data"




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