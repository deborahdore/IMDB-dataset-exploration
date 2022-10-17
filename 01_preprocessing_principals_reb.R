
library(data.table)
library(magrittr)
library(ggplot2); theme_set(theme_minimal())
path = "../IMDB-dataset-exploration-data"




### TITLE PRINCIPALS ----- 2.3 GB
# 52.370246 million rows
# links a title, a person and the role ("category") which the person has in the title.
principals = fread(paste0(path, "/title.principals.tsv"))
head(principals, 30)
summary(principals)

# TCONST
# no missing values, 8.346.176 unique identifiers, converted to integer
grepl("^tt\\d{7,8}$", principals[, tconst]) %>% sum()  # 52370246 ==> every entry consists of "tt" followed by 7 or 8 digits
unique(principals[, tconst]) %>% length()  # 8346176 ==> number of unique identifiers
principals[, tconst := as.integer(gsub("^tt", "", tconst))]  # convert to integer

# ORDERING
# redundant ==> delete
# no missing values, up to 10 persons per title
unique(principals[, ordering])  # values between 1 and 10
table(principals[, ordering]) / 1e5  # distribution of the values
principals[, ordering := NULL]  # delete column

# NCONST
# no missing values, 4.802.538 unique identifiers, converted to integer
grepl("^nm\\d{7,8}$", principals[, nconst]) %>% sum()  # 52370246 ==> every entry consists of "nm" followed by 7 or 8 digits
unique(principals[, nconst]) %>% length()  # 4802538 ==> number of unique identifiers
principals[, tconst := as.integer(gsub("^nm", "", tconst))]  # convert to integer

# CATEGORY
# no missing values
principals[category == "//N", ] %>% nrow()  # ==> zero missing values
unique(principals[, category])  # only 12 unique categories
table(principals[, category])  # distribution of the categories
# merge "archive_footage" and "archive_sound" to category "archive":
principals[category %in% c("archive_footage", "archive_sound"), category := "archive"]
principals[category == "actress", category := "actor"]  # category "actress" changed to "actor"
categories = unique(principals[, category])
ggplot(principals, aes(x = category)) + geom_bar()  # distribution of the 10 categories

# JOB
# redundant ==> delete
# specifies the column "category", not valuable for a quantitative analysis.
principals[job == "\\N", ] %>% nrow()  # 43.778.297 missing values (that's 83.6%!)
principals[job == category, ] %>% nrow()  # In 4.323.578, the job equals the category
unique(principals[!(job %in% c("\\N", categories)), job]) %>% length()  # else: 38.065 unique jobs
principals[, job := NULL]

# CHARACTERS -----
# redundant ==> delete
# specifies the role of actors
principals[characters == "\\N", ] %>% nrow()  # 26.796.202 missing values
principals[characters != "\\N", ] %>% head(100)
principals[(category == "self") & (characters != "[\"Self\"]"), ] %>% head(100)
principals[, characters := NULL]



# SAVE CLEANINGS -----
saveRDS(principals, paste0(path, "/title.principals_clean.rds"))
