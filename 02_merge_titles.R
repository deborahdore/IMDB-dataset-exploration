library(data.table)
path = "//Users/deborah/Documents/IMDB-dataset-exploration/dataset/"

ta = fread(paste0(path, "/title.akas_cleaned.csv"))
tb = fread(paste0(path, "/title.basics_cleaned.csv"))

colnames(ta)[1] <- "tconst"

m1 = merge(ta, tb, by=c("tconst"), all=TRUE)
rm(ta)
rm(tb)
gc()

tr = fread(paste0(path, "/title.ratings_clean.csv"))
m2 = merge(m1, tr, by=c("tconst"), all=TRUE)
rm(tr)
rm(m1)
gc()

te = fread(paste0(path, "/title.episode_cleaned.csv"))
titles = merge(m2, te, by=c("tconst"), all=TRUE)
rm(te)
rm(m2)
gc()

str(titles)

# filter non unique rows
nrow(unique(titles)) == nrow(titles)

titles = unique(titles)

fwrite(titles, paste0(path, "/title.merged.csv"))

