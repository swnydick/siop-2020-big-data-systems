#########################################
# Data.Table Analysis                   #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2021-04-15                            #
#########################################

# 1. Required Packages =========================================================

# repo to use
options(repos = "https://cran.rstudio.com/")

# install the data.table package if it's not installed
cur_pkgs  <- rownames(installed.packages())
req_pkgs  <- "data.table"

# install data.table if it's not installed
miss_pkgs <- setdiff(req_pkgs, cur_pkgs)

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
} # END if STATEMENT

# load data.table
# - note: data.table is a single package and not a collection of packages
#         (unlike the other systems we talked about today)
library(data.table)

# 2. Loading Data ==============================================================

# get project directory (do it the same way as we did earlier)
proj_dir    <- here::here()
data_dir    <- file.path(proj_dir, "data")

# files to read into R (keeping it simple with fread, fast read, from data.table)
dt_demos   <- fread(file.path(data_dir, "demos_to_merge.csv"))
dt_scores  <- fread(file.path(data_dir, "scores_to_merge.csv"))
dt_comb_1  <- fread(file.path(data_dir, "data_to_rowbind.csv"))

# could also create a vector of file names and use data.table::rbindlist to bind them
# into a single dt in one line

# take a look at the data (prints first 5 and last 5 rows)
print(dt_demos)
print(dt_scores)

# for some reason, dt_demos read in the first row not AS names but as data. GROSS!

# take the intended column names from row 1 (first argument is ALWAYS row on dt)
cnames    <- as.character(dt_demos[1])

# remove row 1 from demographic data
dt_demos  <- dt_demos[-1]

# change first value of cnames to "V1"
cnames[1] <- "V1"

# update demos using setnames (which is a more efficient, data.table method)
setnames(dt_demos, cnames)

# like much of data.table, data is often updated BY REFERENCE rather than BY VALUE
print(dt_demos)

# now we need to remove all of those "V1" column that were row.names in a previous
# life (any unnamed column will automatically be V1, ...)

# dropping columns in data.table is easy, efficient, and updated in-place (rather than copying)
dt_demos[ , V1 := NULL]
dt_scores[ , V1 := NULL]
dt_comb_1[ , V1 := NULL]

print(dt_demos)
print(dt_scores)

# 3. Merging ===================================================================

#   A. Combine Variables -------------------------------------------------------

# data.table can arrange by reference using the setorder and sertorderv (the latter
# being slightly faster as it takes a vector rather than an unquoted column name)
setorderv(dt_demos, "guid")
setorderv(dt_scores, "guid")

# to make it easier to demonstrate joins, let's remove a few rows from our data
# (need to use copy so that we don't affect our original dt with updates)
col_order   <- c("row_n", colnames(dt_demos))
dt_demos_1  <- data.table::copy(dt_demos)[ , row_n := 1:.N][-c(1,3,5), ..col_order]

col_order   <- c("row_n", colnames(dt_scores))
dt_scores_1 <- data.table::copy(dt_scores)[ , row_n := 1:.N][-c(2,4), ..col_order]

# note that we can chain commands in data.table together by chaining [][][][]...

dt_demos_1[ , 1:3]
dt_scores_1[ , 1:3]

print(dt_demos_1)
print(dt_scores_1)

# we can do joins in data.table by using base R merging language and using a
# something that is ... a little terse (like take a subset of data?)

# in both cases, it's important to get "keys" in the data to make DT efficient
id_var <- c("row_n", "guid")
setkeyv(dt_demos_1, id_var)
setkeyv(dt_scores_1, id_var)

# inner join (can use "merge", but a little slower)
dt_demos_1[dt_scores_1, nomatch = 0]

# left and right outer joins are bit odd
# - ... just remember that it's like "subsetting"
# - the thing you want to keep safe (or want ALL of) should go on the inside

# left outer join
dt_scores_1[dt_demos_1]

# "right" outer join
dt_demos_1[dt_scores_1]

# full join needs to use merge syntax (it's terse syntax doesn't do full outer join)
merge(x   = dt_demos_1,
      y   = dt_scores_1,
      all = TRUE)

# we can also do anti joins using dt syntax (semi-joins are inner joins but
# without some of the columns)
dt_demos_1[!dt_scores_1]

# now we make the actual join we'll use downstream (specifying the "on" argument
# allows us to do this without setting the keys of both variables first)
dt_comb_2   <- dt_demos[dt_scores, on = "guid"]

#   B. Combine People ----------------------------------------------------------

# rbindlist will bind rows, but we should specify use.names so that it doesn't
# just bind by indices
rbindlist(list(dt_comb_1, rev(dt_comb_2)))

rbindlist(list(dt_comb_1, rev(dt_comb_2)),
          use.names = TRUE)

# what if columns of your data are not the same?
dt_1 <- dt_comb_1[ , c(1, 3, 5)]
dt_2 <- dt_comb_2[ , c(1, 2, 4, 6, 7)]

# by default it will error
tryCatch(
  expr  = rbindlist(list(dt_1, dt_2), use.names = TRUE),
  error = function(e) message(e)
)

# but we can specify "fill = TRUE" (which will also allow us to not need use.names)
rbindlist(list(dt_1, dt_2), fill = TRUE)

# what if variable types are not the same?
dt_1 <- copy(dt_comb_1)
dt_1[ , X1 := as.character(X1)]

dt_2 <- copy(dt_comb_2)

rbindlist(list(dt_1, dt_2), fill = TRUE)

# unlike tidyverse, it coerces the X1 column to a character string

# finalizing the actual data we want to bind
dt_comb   <- rbindlist(list(dt_comb_1, dt_comb_2), use.names = TRUE)

# 4. Reshaping =================================================================

# the data is currently in wide format with each item (`X1`, ... , `X10`) a column
# of the data - we might want to make the data so that each variable forms a
# column and each observation forms a row.

# data.table uses the dcast/melt functions (originally from the reshape2 package)
var_name       <- "trait"        # the category variable (made from the columns)
val_name       <- "normed_score" # the numeric varialbe (made from the values)

# note that "patterns" is similar to the tidyverse function "matches"
dt_comb_long   <- melt(dt_comb,
                       measure.vars  = patterns("^X[0-9]+$"),
                       variable.name = var_name,
                       value.name    = val_name)
dt_comb_wide   <- dcast(dt_comb_long,
                        ... ~ trait,
                        value.var = val_name)

# in our case, we're using two special functions:
# - "patterns" is a function that says "we want to use this pattern to systematically
#   indicate the response variables
# - ... indicates "all other variables that are not mentioned anywhere else"
# - x ~ y (x-vars are the subject vars, and y-vars are the transposed vars)


# 5. Aggregating ===============================================================

# aggregating starts to really take advantage of data.table syntax ... remember:
# - everything is like "subsetting": DT[stuff goes here]
# - first argument is what happens to the rows: DT[rows to extract and stuff]
# - second argument is what happens to the cols: DT[ , cols to extract and stuff]
#   - we CAN extract OR create new variables here
# - "." is a synonym for "list"

#   A. Long Format Data --------------------------------------------------------

# if your data is in long format, you can simply put what you want to aggregate
# in the column argument and specify something for "by".
dt_comb_long[, .(mean_score = mean(normed_score),
                 sd_score   = sd(normed_score)),
             by = .(data_level, trait)]

# you can also do this with standard evaluation
# - by can be c("data_level", "trait")
# - you can use "get" in the column argument if normed_score is a character string
normed_var <- "normed_score"
dt_comb_long[, .(mean_score = mean(get(normed_var)),
                 sd_score   = sd(get(normed_var))),
             by = c("data_level", "trait")]

# in both of these cases X10 seems to be coming at the end, so we don't need to
# do the re-factoring of the previous methods here due to how things were
# handled when converting to long format
                                  
#   B. Wide Format Data --------------------------------------------------------

# if your data is in wide format, you can manually specify each column you want
# to aggregate as you did before
dt_comb_wide[, .(X1_mean = mean(X1),
                 X1_sd   = sd(X1),
                 X2_mean = mean(X2),
                 X2_sd   = sd(X2),
                 X3_mean = mean(X3),
                 X3_sd   = sd(X3)),
             by = data_level]

# this can get a bit cumbersome

# `data.table` has a special way of handeling sets of columns
# - .SDcols is a list of a subset of columns to perform operations on
# - You can then use the special variable .SD in the column argument (usually
#   in a lapply statement) to do something on ALL specified columns

# this can be a bit gross in data table
item_idx <- grep(x       = names(dt_comb_wide),
                 pattern = "^X[0-9]+$")


dt_comb_wide[, c(lapply(setNames(.SD, paste0("mean_", names(.SD))), mean),
                 lapply(setNames(.SD, paste0("sd_",   names(.SD))), sd)),
             by      = data_level,
             .SDcols = item_idx]

# you can also do this a slightly different, more verbose way
# means      <- dt_wide[, lapply(.SD, mean),
#                       by = data_level,
#                       .SDcols = item_idx]
# 
# stdevs     <- dt_wide[, lapply(.SD, sd),
#                       by = data_level,
#                      .SDcols = item_idx]
# 
# summarised <- means[stdevs, on = "data_level"]
#
# setnames(summarised, gsub("^X", "mean_X", colnames(summarised)))
# setnames(summarised, gsub("^i\\.X", "sd_X", colnames(summarised)))
