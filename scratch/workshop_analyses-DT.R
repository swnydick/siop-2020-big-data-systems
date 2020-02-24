#' ---
#' title: "Examples of Workshop Analyses - data.table style"
#' author: "Korn Ferry Institute"
#' date: 2020-02-15
#' output:
#'   html_document:
#'     df_print: paged
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo       = TRUE,
                      out.width  = '1600px',
                      out.height = '900px',
                      dpi        = 200)

#' 
#' 
#' The purpose of this document is to do what was done with the tidyverse, only faster with fewer dependencies. 
#' 
#' ## Required Packages: 
#' 
#' Just data.table
#' 
## ------------------------------------------------------------------------------------------------------------------------

# load the tidyverse packages
library(data.table)
library(microbenchmark)


#' 
#' ## Loading Data
#' 
#' If your data is in ".csv" format, the fastest way to read and load it into memory is data.table's `fread()`
#' 
## ------------------------------------------------------------------------------------------------------------------------
# pull project directory
project_dir <- here::here()

# combine file names to pull paths - with a simple vector
data_files  <- c(demos  = "Demos_to_merge.csv",
                 scores = "Scores_to_merge.csv",
                 comb   = "Data_to_rowbind.csv"
                 )

list.files(file.path(project_dir, "data"))

# can check to make sure the files exist before reading them in
# Note - base R does this just fine, no added dependencuies needed
print(file.exists(data_paths))

## ------------------------------------------------------------------------------------------------------------------------
# method one of reading files - 
# keeping it simple with fread (fast read) from data.table

# could create vector of file names with list.files(), use data.table::rbindlist(lapply(list, fread)) if you 
# want to bind them all into a single dt - fastest way around. 

demos  <- fread(file.path(project_dir, "data", "Demos_to_merge.csv"))
scores <- fread(file.path(project_dir, "data", "Scores_to_merge.csv"))
comb   <- fread(file.path(project_dir, "data", "Data_to_rowbind.csv"))

head(demos)
head(scores)
head(comb)

# To see how fast fread is we can run a quick benchmark
microbenchmark(data.table = fread(file.path(project_dir, "data", "Scores_to_merge.csv")),
               readr = readr::read_csv(file.path(project_dir, "data", "Scores_to_merge.csv")),
               base = read.csv(file.path(project_dir, "data", "Scores_to_merge.csv")))

## ------------------------------------------------------------------------------------------------------------------------
# Gross - look at the first row of demos, let's re-read that in to fix that

# Take the inteded column names from row 1
cnames <- as.character(demos[1,])
# Remove row 1
demos  <- demos[-1,] 
# Change cnames[1] from NA to "V1"
cnames[1] <- "V1"
# Setnames for demos, note that data.table has a more efficient method with setnemae
setnames(demos, cnames)


# Presto
head(demos)

# Awesome, now just need to remove all of those "V1" columns that appear to have
# been row names in a previous life. in data.table, any unnamed column will be automatically
# labelled V1, V2, V3, and so on

# Dropping columns in data.tabke is super easy (and memory efficient)
# Note that this is an in-place modification, it isn't copying a modified version of the 
# data, which makes it fast!
demos[, V1 := NULL]
scores[, V1 := NULL]
comb[, V1 := NULL]

## ------------------------------------------------------------------------------------------------------------------------

#' 
#' ## Merging Data
#' 
#' The Tidyverse package `dplyr` contains several functions to merge two tibbles.
#' Unlike base-R, which has one function to do all of the joining, the Tidyverse
#' tries to make individual functions for specific goals (with each function
#' having a different, descriptive name).
#' 
#' 1. `inner_join`: keep rows of `x` and rows of `y` that have the same elements
#'    of the `by` variable. Named after the SQL join "INNER JOIN" or "JOIN".
#' 2. `left_join`: keep all rows of `x` and ONLY rows of `y` that have `by` elements
#'    in `x`. Named after the SQL join "LEFT JOIN" or "LEFT OUTER JOIN".
#' 3. `right_join`: keep all rows of `y` and ONLY rows of `x` that have `by` elements
#'    in `y`. Named after the SQL join "RIGHT JOIN" or "RIGHT OUTER JOIN".
#' 4. `full_join`: keep all rows of `x` and all rows of `y`. Named after the SQL
#'    join "FULL JOIN" or "FULL OUTER JOIN".
#' 6. `anti_join`: keep rows of `x` that do not have `by` elements in `y`. Named
#'    after the SQL join "LEFT ANTI JOIN".

#'    
#' To see how these work, let's remove some rows from `tidy_demos` and `tidy_scores`.
#' Lets sort the rows ahead of time so that the joins return data in the same order.
#' Note that merging in the Tidyverse doesn't automatically sort the rows unlike
#' base R functions.
#' 
## ------------------------------------------------------------------------------------------------------------------------
# sort dts by guid

#' data.table can arrange by reference, which is superior in terms of efficiency.
#' To do this, we call setorder() or setorderv(). The latter is slightly faster as
#' it takes a vector (hence *v) of names rather than performing NSE on an unquoted column name.
#' it's also just easier to soft-code using setorderv()!

setorderv(demos, "guid")
setorderv(scores, "guid")

# To make it easier to demonstrate what our joins will do...
# remove a few odd rows from tidy_demos and even rows from tidy_scores

# tidy_demos_1  <- tidy_demos %>%
#    mutate(row_n = row_number()) %>%
#    slice(-vctrs::vec_c(1, 3, 5)) %>%
#    select(row_n, everything())

# Can chain data.table commands together like so


# If we don't want to affect the original with our :=,
# need to use copy
col_order <- c("row_n", colnames(demos))
demos_1   <- data.table::copy(demos)[, row_n := 1:.N][-c(1,3,5), ..col_order]

# tidy_scores_1 <- tidy_scores %>%
#                  mutate(row_n = row_number()) %>%
#                  slice(-vctrs::vec_c(2, 4)) %>%
#                  select(row_n, everything())
col_order <- c("row_n", colnames(scores))
scores_1  <- data.table::copy(scores)[, row_n := 1:.N][-c(2,4), ..col_order]


#' 
## ------------------------------------------------------------------------------------------------------------------------
head(demos_1[, 1:3])
head(scores_1[, 1:3])

#' 
#' What happens when we apply each of the standard join types?
#' 
## ------------------------------------------------------------------------------------------------------------------------
id_var <- c("row_n", "guid")

#' Data.table has two syntaxes for merging, one is just like base R, the other is
#' ...a little terse. 
#' In both cases, it's important to set the "keys" in the data, this is what makes 
#' joins in data.table so efficient

setkeyv(demos_1, id_var)
setkeyv(scores_1,id_var)

# Inner 
j_inner <- demos_1[scores_1, nomatch = 0]
head(j_inner)
# can also data.table:::merge.data.table 
# BUT there's a little overhead in doing that
microbenchmark(dt = demos_1[scores_1, nomatch = 0], 
               s3 = merge(demos_1, scores_1, all=FALSE))

# It's looks a little backwards...
# just remember to put the thing you want to keep safe [inside]
# left_join(x  = tidy_demos_1,
#           y  = tidy_scores_1,
#           by = id_var) %>%
#    arrange(row_n)
j_left <- scores_1[demos_1]
head(j_left)

j_right <- demos_1[scores_1]
head(j_right)

# data.table's terse syntax doesn't really do full outer joins
# need to fall back on data.table:::merge.data.table()

j_outer <- merge(demos_1, scores_1, all = TRUE)
head(j_outer)



#' Note that the `inner` kept only `row` that was in both tables (6, ...),
#' `left` kept `row` that was in the first table but not the second (2, 4, 6, ...),
#' `right` kept `row` that was in the second table but not the first (1, 3, 5, 6, ...),
#' and `outer` kept all rows. In all cases, all of the variables were kept and
#' data missing data (missing rows) for all variables were set to `NA`.
#' 
#' Filtering joins (`semi_join` and `anti_join`) are a quick way of keeping rows
#' in the data by using "join" terminlogy rather than filtering data directly.
#' 
## ------------------------------------------------------------------------------------------------------------------------

# keep all demos_1 rows without equivalents in scores_1
j_anti <- demos_1[!scores_1]
head(j_anti)


## ------------------------------------------------------------------------------------------------------------------------

#' Now we make the actual join we'll use downstream
#' note that data.table now also lets you set keys directly with an 
#' on= argument

comb_2 <- demos[scores, on="guid"]


#' Finally, we have additional data (`comb`) that we want to combine with the
#' merged data (`comb_2`). We can bind everything together using `rbidlist`. The simplest way
#' to use the function is in the same way that you use `rbind`.
#' 
## ------------------------------------------------------------------------------------------------------------------------

# The safest way is to bind using column names
combined <- rbindlist(list(comb, comb_2), use.names = TRUE)

# Binding by position is a bit faster, but we need to check to see if column names match
# colnames(comb_2)
# colnames(comb)
# combined <- rbindlist(list(comb, comb_2), use.names = FALSE)

## ------------------------------------------------------------------------------------------------------------------------

#' 
#' ## Reshaping Data
#' 
## ------------------------------------------------------------------------------------------------------------------------
var_name       <- "trait"        # the category variable made from the columns
val_name       <- "normed_score" # the numeric variable made from the values

dt_long <- melt(combined, 
                measure.vars  = patterns("^X[0-9]+$"),
                variable.name = var_name, 
                value.name    = val_name)
head(dt_long)

dt_wide <- dcast(dt_long, 
                 ... ~ trait, 
                 value.var = val_name)

head(dt_wide)

#' 
#' Note that dcast syntax is difficult to softcode. To do thanm a hack like
# dt_long2 <- data.table::copy(dt_long)
# # make sure column cast_id will be made from whatever is in var_name
# dt_long2[, cast_id := dt_long2[[var_name]]] 
# dt_long2[[var_name]] <- NULL
# dt_wide2 <- dcast(dt_long2, 
#                  ... ~ cast_id, 
#                  value.var = val_name)
#' 
## ------------------------------------------------------------------------------------------------------------------------
#' 
#' ## Aggregating Data
#' 
#' How you would approach aggregating data in the Tidyverse framework depends on
#' whether your data is in long format or wide format. If your data is in long
#' format, then it would be easiest to use the `group_by` `summarize` pattern
#' directly. In this case `group_by` is the variables we want to split by and
#' `summarize` is the stuff that we want to happen within each group.
#' 
## ------------------------------------------------------------------------------------------------------------------------

# rerun
dt_long[, .(mean_score = mean(normed_score),
            sd_score   = sd(normed_score)),
        by = .(data_level, trait)]


#' Note that the order is correct! 
#' 
## ------------------------------------------------------------------------------------------------------------------------
# repeat this for all 10 variables ...

# This is a bit gross in data.table..
means <- dt_wide[, lapply(.SD, mean)
                 , by = data_level,
                   .SDcols = grep("^X[0-9]+$", names(dt_wide))]

stdevs <- dt_wide[, lapply(.SD, sd)
                 , by = data_level,
                 .SDcols = grep("^X[0-9]+$", names(dt_wide))]

summarised <- means[stdevs, on = "data_level"]
setnames(summarised, gsub("^X", "mean_X", colnames(summarised)))
setnames(summarised, gsub("^i\\.X", "sd_X", colnames(summarised)))
summarised
