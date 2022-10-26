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


# remove duplicate original titles
original = titles[types == "original"]
rm(titles); gc()
nrow(original) - length(unique(original[, tconst]))  # 164 duplicate tconst
original[, N := .N, by = tconst]
original[N > 1, N := lapply(unique(tconst), function(t) {
  dat = original[tconst == t]
  keep = dat[, title] == dat[, originalTitle]
  if (sum(keep) == 1) return(keep)
  keep = !is.na(dat[, language]) | !is.na(dat[, region])
  if (sum(keep) == 1) return(keep)
  keep = !is.na(dat[, language])
  if (sum(keep) == 1) return(keep)
  else return(c(TRUE, rep(FALSE, nrow(dat) - 1)))
}) %>% unlist()]
original = original[N == 1]
nrow(original) - length(unique(original[, tconst]))  # now everything is all right


fwrite(original, paste0(path, "/title.original.csv"))