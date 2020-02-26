#########################################
# Data.Table Analysis                   #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2020-04-24                            #
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

print(tidy_demos_1)
print(tidy_scores_1)

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

# TO HERE #

# 4. Reshaping =================================================================

# the data is currently in wide format with each item (`X1`, ... , `X10`) a column
# of the data - we might want to make the data so that each variable forms a
# column and each observation forms a row.

#   A. OLD WAY: gather + spread ------------------------------------------------
var_name       <- "trait"
val_name       <- "normed_score"
tidy_comb_long <- gather(tidy_comb,
                         key   = !!var_name,
                         value = !!val_name,
                         matches("^X[0-9]+$"))
tidy_comb_wide <- spread(tidy_comb_long,
                         key   = !!var_name,
                         value = !!val_name)

# check if we've reversed everything
all_equal(target  = tidy_comb_wide,
          current = tidy_comb)

# note that gather's optional argument are the variables to make long

# - can use raw variable names
gather(tidy_comb,
       key   = !!var_name,
       value = !!val_name,
       X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)

# - can use "-" syntax to remove variables we don't want to combine
gather(tidy_comb,
       key   = !!var_name,
       value = !!val_name,
       -guid, -data_level, -data_industry, -data_function)

# - can use tidyselect helpers to select all variables that "starts_with" or
#   "matches" or "ends_with" something
gather(tidy_comb,
       key   = !!var_name,
       value = !!val_name,
       starts_with("X"))

#   B. NEW WAY: pivot_longer and pivot_wider -----------------------------------

# essentially the same:
#  - key is now "names_to" (in pivot_longer) and "names_from" (in pivot_wider)
#  - value is now "values_to" (in pivot_longer) and "values_from" (in pivot_wider)
#  - ... is now a single argument "cols"
#  - other arguments indicate what to do AFTER the reshaping
tidy_comb_long_2 <- pivot_longer(tidy_comb,
                                 cols      = matches("^X[0-9]+$"),
                                 names_to  = var_name,
                                 values_to = val_name)
tidy_comb_wide_2 <- pivot_wider(tidy_comb_long_2,
                                names_from  = all_of(var_name),
                                values_from = all_of(val_name))

# check if we've reversed everything
all_equal(target  = tidy_comb_wide_2,
          current = tidy_comb_wide)
all_equal(target  = tidy_comb_long_2,
          current = tidy_comb_long)

# note: see the additional file/documentation for other fun things to do with
#       pivot_longer/pivot_wider as well as how to pivot from a specification
#       data frame.

# 5. Aggregating ===============================================================

#   A. Long Format Data --------------------------------------------------------

# if your data is in long format, you can simply do a group by and summarize
# with each variable that you're calculating as additional call in summarize
tidy_comb_long %>%
  group_by(data_level, trait) %>%
  summarize(mean_score = mean(normed_score),
            sd_score   = sd(normed_score))

# you can use another function (forcats, which deals with factors) to make trait
# (or any other "factor" in the correct order)
pull_out_number <- function(x){
  stringr::str_extract(string = x,
                       pattern = "[0-9]+") %>%
  unique() %>%
  as.numeric()
}

tidy_comb_long  <- tidy_comb_long %>%
                   mutate(trait = as.factor(trait) %>%
                                  fct_reorder(.f = .,
                                              .x = .,
                                              .fun = pull_out_number))

# note that now the order is correct :)
tidy_comb_long %>%
  group_by(data_level, trait) %>%
  summarize(mean_score = mean(normed_score),
            sd_score   = sd(normed_score))
                                  
#   B. Wide Format Data --------------------------------------------------------

# if your data is in wide format, you can manually specify each column you want
# to aggregate
tidy_comb_wide %>%
   group_by(data_level) %>%
   summarize(X1_mean = mean(X1),
             X1_sd   = sd(X1),
             X2_mean = mean(X2),
             X2_sd   = sd(X2),
             X3_mean = mean(X3),
             X3_sd   = sd(X3))

# this gets unbelievably complicated

# `dplyr`` has versions of `summarize` that work systematically on subsets of
# the data without you having to specify those subsets directly.
#  - `all` (do this thing on all of the columns)
#  - `at` (do this thing on the columns specified)
#  - `if` (do this thing to columns that are TRUE according to some function)

# in each case, we can specify `.funs` which can be a named list of multiple
# functions
tidy_comb_wide %>%
   group_by(data_level) %>%
   summarize_at(.vars = vars(matches("^X[0-9]+$")),
                .funs = list(mean = mean,
                             sd   = sd))

# note: that what is in "vars" is similar to how we selected columns for spread or
# pivot_wider

# additional examples are in a separate file!