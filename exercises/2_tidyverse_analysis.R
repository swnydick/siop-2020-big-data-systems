#########################################
# Tidyverse Analysis                    #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2020-04-24                            #
#########################################

# 1. Required Packages =========================================================

# repo to use
options(repos = "https://cran.rstudio.com/")

# install the tidyverse if it's not installed
cur_pkgs  <- rownames(installed.packages())
req_pkgs  <- "tidyverse"

# install tidyverse if it's not installed
miss_pkgs <- setdiff(req_pkgs, cur_pkgs)

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
} # END if STATEMENT

# load necessary tidyverse packages
# - note: tidyverse is a meta-package (collection of packages) - if you are
#         writing a package that depends on the tidyverse, do NOT import the
#         tidyverse - import the specific dependencies
library(tidyverse)

# check to see if any dependencies are out of date
tidyverse::tidyverse_update(recursive = TRUE)

# what packages are part of the `tidyverse`
tidyverse::tidyverse_packages(include_self = FALSE)

# 2. Loading Data ==============================================================

# get project directory
proj_dir    <- here::here()
data_dir    <- file.path(proj_dir, "data")

# files to read into R
tidy_demos  <- read_csv(file.path(data_dir, "demos_to_merge.csv"))
tidy_scores <- read_csv(file.path(data_dir, "scores_to_merge.csv"))
tidy_comb_1 <- read_csv(file.path(data_dir, "data_to_rowbind.csv"))

# - if we look at tidy_demos/tidy_scores/tidy_comb_1, read_csv read in the rownames
#   and called them X1.
# - if we had another column called "X1", read_csv renamed that column X1_1
print(tidy_demos)
print(tidy_scores)

remove_rowname_column <- function(tbl){
  
  # remove first column (which is rownames)
  tbl <- select(tbl, -1)
  
  # if we have another column that got changed, fix it!
  if("X1_1" %in% names(tbl)){
    tbl <- rename(tbl, X1 = X1_1)
  } # END if STATEMENT
  
  return(tbl)
}

tidy_demos  <- remove_rowname_column(tidy_demos)
tidy_scores <- remove_rowname_column(tidy_scores)
tidy_comb_1 <- remove_rowname_column(tidy_comb_1)

print(tidy_demos)
print(tidy_scores)

# another "tidyverse" package: vroom can also read in data (probably faster)

# 3. Merging ===================================================================

#   A. Combine Variables -------------------------------------------------------

# let's do the following:

# - arrange by guid (so that merging doesn't change the order)
tidy_demos    <- arrange(tidy_demos, guid)
tidy_scores   <- arrange(tidy_scores, guid)

# - remove some rows (so that we can see what happens when merging)
tidy_demos_1  <- tidy_demos %>%
                 mutate(row_n = row_number()) %>%
                 slice(-c(1, 3, 5)) %>%
                 select(row_n, everything())

tidy_scores_1 <- tidy_scores %>%
                 mutate(row_n = row_number()) %>%
                 slice(-c(2, 4)) %>%
                 select(row_n, everything())

print(tidy_demos_1)
print(tidy_scores_1)

# these are the standard joins and match SQL syntax (sort of)
id_var <- c("row_n", "guid")
inner_join(x  = tidy_demos_1,
           y  = tidy_scores_1,
           by = id_var) %>%
  arrange(row_n)

left_join(x  = tidy_demos_1,
          y  = tidy_scores_1,
          by = id_var) %>%
  arrange(row_n)

right_join(x  = tidy_demos_1,
           y  = tidy_scores_1,
           by = id_var) %>%
  arrange(row_n)

full_join(x  = tidy_demos_1,
          y  = tidy_scores_1,
          by = id_var) %>%
  arrange(row_n)

# we can leave out by (in some cases), but then the functions are chatty
inner_join(x = tidy_demos_1,
           y = tidy_scores_1)

# there's also TWO additional joins: semi_join and anti_join
semi_join(x  = tidy_demos_1,  # ???
          y  = tidy_scores_1,
          by = id_var)
anti_join(x  = tidy_demos_1,  # ???
          y  = tidy_scores_1,
          by = id_var)

# we can also think about "merging" by binding rows together
id_var      <- "guid"
tidy_comb_2 <- inner_join(x  = tidy_demos,
                          y  = tidy_scores,
                          by = id_var)

#   B. Combine People ----------------------------------------------------------

# to bind rows, we can:

# - enter data one at a time
bind_rows(tidy_comb_1, tidy_comb_2)

# - enter data in list format
bind_rows(list(tidy_comb_1, tidy_comb_2))

# - or do a combination
bind_rows(list(tidy_comb_1, tidy_comb_2), tidy_comb_1)

# what if columns of your data are not the same?
df_1 <- select(tidy_comb_1,
               1, 3, 5) %>%
        slice(1, 2)
df_1
df_2 <- select(tidy_comb_2,
               1, 2, 4, 6, 7) %>%
        slice(1, 2)
df_2

bind_rows(df_1,
          df_2)

# what if variable types are not the same?
df_1 <- mutate(tidy_comb_1,
               X1 = as.character(X1))
df_2 <- tidy_comb_2

tryCatch(
  expr  = bind_rows(df_1, df_2),
  error = function(e) message(e)
)

# finalizing the actual data we want to bind
tidy_comb <- bind_rows(tidy_comb_1,
                       tidy_comb_2)

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